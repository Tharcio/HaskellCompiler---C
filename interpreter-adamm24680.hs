
540 lines (472 sloc) 14.4 KB
module Interpreter where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Error
import System.IO(hPutStrLn, stderr)
import Data.Map.Lazy hiding (map)
import AbsGrammar
import PrintGrammar
import ErrM


---------------------------
-- typy i stale
-----------------------------
data Type = Simple Type_specifier
            deriving(Eq)

data FullType = Ref Type |
                NoRef Type
              deriving(Eq)

data Fun = Fun ([Value] -> M Value)

returnByRefFlag :: String
returnByRefFlag = "1retref"

data Value = VInt Integer |
             VBool Bool |
             VString String |
             VLoc Loc |
             VVoid
             --VDouble Double |
             --VChar Char
instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VString s) = s
  show b = ""

defVal:: Type_specifier -> M Value
defVal Tint = return (VInt 0)
defVal Tbool = return (VBool False)
defVal Tstring = return (VString "")
--defVal Tvoid = throwError "type void has no value"
   

type Loc = Integer
type Store = Map Loc Value
type VEnv = Map String Loc
type FEnv = Map String (Fun, [FullType])
type Env = (VEnv, FEnv)

type M = ReaderT Env (ErrorT String (StateT Store IO))

-------------------------------------------
-- niskopoziomowe funkcje obslugujace
-- czesc kodu sie powtarza, zostanie przeniesione tutaj
----------------------------------------------------

checkFlag :: String -> M Bool
checkFlag s = do
  (env, _)<- ask
  return (member s env)

getLoc :: String -> M Loc
getLoc s = do
  (env,_) <- ask
  case Data.Map.Lazy.lookup s env of
   Just l -> return l
   Nothing -> lift $ throwError ("Could not find variable " ++ s ++ " in the environment.")


dealloc :: Maybe Loc -> M ()
dealloc (Just l) = do
  s <- get
  put (delete l s)
dealloc Nothing = return ()
   
alloc :: M Loc
alloc = do
  s <- get
  let x = if Data.Map.Lazy.null s then 0
          else 1 + fst (findMax s)
    in return x
    
readL :: Loc -> M Value
readL l = do
  s <- get
  case Data.Map.Lazy.lookup l s of
   Just x -> return x
   Nothing -> lift $ throwError "Invalid reference"
writeL :: (Loc, Value) -> M ()
writeL (l, val) = do
  modify (insert l val)
 
getFn :: String -> M (Fun, [FullType])
getFn str = do
  (_, fenv) <- ask
  case Data.Map.Lazy.lookup str fenv of
   Just x -> return x
   Nothing -> lift $ throwError ("Could not find function " ++ str ++ " in the environment.")




--------------------------------------------
-- funkcja semantyczna dla wyrazen
------------------------------------
   
execFun :: String -> [Exp] -> M Value
execFun id l = do
  (Fun func, types) <- getFn id
  args <- processArgs l types
  func args
  
eval:: Exp -> M Value
eval expression =
   case expression of
    (Econst x) ->
      return (getConstant x)
    (Estring x) ->
      return (VString x)
    (Evar (Ident x)) -> do
      loc <- getLoc x
      readL loc
    (Eeq a1 a2) -> cmp a1 a2 (==)
    (Eneq a1 a2) -> cmp a1 a2 (/=)
    (Elthen a1 a2) -> cmp a1 a2 (<)
    (Egrthen a1 a2) -> cmp a1 a2 (>)
    (Ele a1 a2) -> cmp a1 a2 (<=)
    (Ege a1 a2) -> cmp a1 a2 (>=)
    (Eplus a1 a2) -> arOpExp a1 a2 (+)
    (Eminus a1 a2) -> arOpExp a1 a2 (-)
    (Etimes a1 a2) -> arOpExp a1 a2 (*)
    (Ediv a1 a2) -> arOpExp a1 a2 (div)
    (Emod a1 a2) -> arOpExp a1 a2 (mod)
    (Epreinc e) -> incdec e (+1)
    (Epredec e) -> incdec e (+(-1))
    (Epreop op exp) -> do
      res <- eval exp
      case op of
       Plus -> case res of
                VInt x -> return $ VInt x
                _ -> lift $ throwError "Wrong type"
       Negative -> case res of
                    VInt x -> return $ VInt (-x)
                    _ -> lift $ throwError "Wrong type"
       Logicalneg -> case res of
                      VBool x -> return $ VBool (not x)
                      _ -> lift $ throwError "Wrong type"
    (Efunkpar (Ident id) l) -> do
      res <- handleErrors expression $ execFun id l
      case res of
       VLoc loc -> readL loc
       VVoid -> lift$ throwError "Void value returned"
       _ -> return res
    (Eassign exp1 op exp2) -> do
      loc <- handleErrors exp1 $ evalLoc exp1
      val2 <- handleErrors exp2 $ eval exp2
      val1 <- readL loc
      t1 <- getValType val1
      t2 <- getValType val2
      (checkTypes t1 t2)
      res <- assignOp val1 val2 op
      writeL(loc, res)
      return $! res
      

------------------------------------------------
-- funkcja semantyczna dla instrukcji
-- typ Value jest potrzebny ze wzgledu na obecna implementacje return
------------------------------------------------------
data StmRes =
  Val Value |
  Environ (Env ->Env, Maybe Loc) |
  Void
  
          
exec :: Stm -> M StmRes
exec (BlockS (ScompBlock l)) =
  let iter ll =
        case ll of
         [] -> return Void
         (h:t) -> do
           res <- exec h
           case res of
            Void -> exec (BlockS (ScompBlock t))
            Environ (f,loc) -> do
              res <- local f $ exec (BlockS (ScompBlock t))
              dealloc loc
              return res
            Val val -> return res
  in iter l
exec (IterS (SiterWhile exp stm)) = do
  res <- handleErrors exp $ evalBool exp
  if (res) then do
    t1 <- exec stm
    case t1 of
     Val v -> return t1 
     x -> exec (IterS (SiterWhile exp stm))
    else return Void
exec (IterS (SiterFor3 e1 e2 exp stm)) = do
  exec $ ExprS e1
  let cexp =
        case e2 of
         SexprEmpty -> expTrue
         SexprExp x -> x
      sexp = ExprS (SexprExp exp)
    in
   let nstm = BlockS (ScompBlock [stm,sexp])
   in exec (IterS (SiterWhile cexp nstm))
exec (IterS (SiterFor2 e1 e2 stm)) =
  exec (IterS (SiterFor3 e1 e2 expTrue stm))
exec (IterS (SiterDoWhile stm exp)) = do
  res <- exec stm
  case res of
   Val v -> return res
   _ -> exec (IterS (SiterWhile exp stm))

exec stmt =
  handleErrors stmt $
   case stmt of
    (ExprS SexprEmpty)  -> return Void
    (ExprS (SexprExp exp))  ->
      eval exp >> return Void
    (JumpS (SjumpReturn exp))  -> do
      flag <- checkFlag returnByRefFlag
      if (flag) then
        do
          ret <- evalLoc exp
          return $ Val (VLoc ret)
        else
        do
          ret <- eval exp
          return $ Val ret
    (SelS sels)  ->
      case sels of
       SselIf exp stm -> do
         res <- handleErrors exp $ evalBool exp
         if (res) then exec stm
           else return Void
       SselIfElse exp stm1 stm2 -> do
         res <- handleErrors exp $ evalBool exp
         if (res) then exec stm1
           else exec stm2
    (DecS d)  -> do
      x <- vDec d
      return $ Environ x
    (FunDefS d) -> do
      env <- fnDec d
      return $ Environ ((\_ -> env), Nothing)
    (PrintS (Sprint exp))  -> do
      res <- eval exp
      liftIO $ putStrLn (show res)
      return Void
   
--------------------------------------------------
-- funkcja semantyczna dla lvalue
--------------------------------------------------

evalLoc :: Exp -> M Loc
evalLoc (Evar (Ident x)) = getLoc x
evalLoc (Efunkpar (Ident id) l) = do
  res <- execFun id l
  case res of
   VLoc loc -> return loc
   _ -> lift  (throwError "Lvalue expected")
evalLoc x = lift (throwError "Lvalue expected")
   

       
---------------------------------------------------------
-- deklaracje zmiennych i procedur
-------------------------------------------------


vDec :: Dec -> M ((Env -> Env), Maybe Loc)
vDec (Decl d) = do
  (tp, name) <- parseDeclarator d
  case tp of
   NoRef (Simple t) -> do
     df <- defVal t
     (m, loc) <- insVEnv name df
     return (m, Just loc)
   Ref (Simple t) -> lift $ throwError ("Reference must be initialized") --- " ++ printTree (Decl d))
   _ -> lift $ throwError "Arrays not implemented"
vDec (IDecl d exp) = do
  (tp, name) <- parseDeclarator d
  case tp of
   NoRef (Simple t) -> do
     res <- handleErrors exp $ eval exp
     tp2 <- getValType res
     (checkTypes tp tp2)
     (m, loc) <- insVEnv name res
     return(m, Just loc)
   Ref (Simple t) -> do
     loc <- handleErrors exp $ evalLoc exp
     tp2 <- (readL loc >>= getValType)
     (checkTypes tp tp2)
     return (modEnv name loc, Nothing)
   _ -> lift $ throwError "Arrays not implemented"
  

fnDec :: Function_def -> M Env
fnDec (AbsGrammar.Func (VDecl t (Ident id)) decls stm) = do
  (tp,_) <- parseDeclarator (VDecl t (Ident id))
  case tp of
   Ref (Simple Tvoid) -> lift $ throwError ("Void return type not implemented") 
   NoRef (Simple Tvoid) -> lift $ throwError ("Void return type not implemented")
   _ -> return ()
  (venv, fenv) <- ask
  typesnames <- parseDeclarators decls
  let errm = (printTree (VDecl t (Ident id))) in
   let sv = runReaderT ((exec$ BlockS stm) >>= (checkExec tp)) in
    let f vals = do
          (_venv, locs) <- insertArgs venv typesnames vals
          let nvenv = case tp of
                Ref k -> insert returnByRefFlag 0 _venv
                _ -> _venv
          retval <- lift $ sv (nvenv, insert id (Fun f, map fst typesnames) fenv)
          deallocList locs
          return retval
    in return (venv, insert id (Fun f, map fst typesnames) fenv)

-----------------------------------------------------------
    
execGlobalDec :: [External_declaration] -> M Env
execGlobalDec [] = ask
execGlobalDec (h:t) =
  handleErrors h $
   case h of
    Afunc def -> do
      nenv <- fnDec def
      local (\_ -> nenv) $ execGlobalDec t
    Global dec -> do
      (modenv, _) <- vDec dec
      local modenv $ execGlobalDec t

execP :: Program -> M ()
execP (Progr l) = do
  env <- execGlobalDec l
  local (\_ -> env) $ eval (Efunkpar (Ident "main") [])
  return ()

  
interpret :: Program -> IO ()
interpret x =
  let comp = execP x
  in let unenv = runReaderT comp (empty,empty)
   in let unerror = runErrorT unenv
      in let unstate = runStateT unerror empty
         in do
           (res, _) <- unstate
           case res of
            Left e -> hPutStrLn stderr $ "error: "++ e
            Right _ -> return ()

------------------------------------------------------------------------------
--funkcje pomocnicze
--------------------------------------------------------------

handleErrors t m = do
    env <- ask
    let comp = runReaderT m env in
     lift $ catchError comp (\e -> throwError (e ++ "\n//--------in--------\n" ++ printTree t))
    

--------------------------------------------------

getValType :: Value -> M FullType
getValType v =
  case v of
   VInt _-> return (NoRef $ Simple Tint)
   VBool _-> return (NoRef $ Simple Tbool)
   VString _-> return (NoRef $ Simple Tstring)
   VVoid -> return (NoRef $ Simple Tvoid)
   VLoc l -> do
     s <- get
     case Data.Map.Lazy.lookup l s of
      Just x -> case x of
        VInt _-> return (Ref $ Simple Tint)
        VBool _-> return (Ref $ Simple Tbool)
        VString _-> return (Ref $ Simple Tstring)
        _ -> lift$ throwError "Wrong type referenced"
      Nothing -> lift$throwError "Invalid reference"

checkTypes:: FullType ->FullType-> M ()
checkTypes t1 t2 = do
  if t1 == t2 then
    return ()
  else
    lift $ throwError ("Wrong type") 

evalBool exp = do
  res <- eval exp
  case res of
   VBool b -> return b
   _ -> lift $ throwError ("Wrong type, expected bool")

--------------------------------------------------------

getConstant :: Constant -> Value
getConstant x = case x of
  --Edouble d  -> VDouble d
  --Echar c  -> VChar c
  Eint n  -> VInt n
  Ebool cbool  -> case cbool of
                   CTrue -> VBool True
                   CFalse -> VBool False


cmp:: Exp -> Exp -> (Integer -> Integer -> Bool) -> M Value
cmp a1 a2 f = do
  r1 <- eval a1
  case r1 of
   VInt x -> do
     r2 <- eval a2
     case r2 of
      VInt y ->
        return $ VBool (f x y)
      _ -> lift $ throwError "Wrong type"
   _ -> lift $ throwError "Wrong type"

arOp :: Value -> Value -> (Integer ->Integer ->Integer) -> M Value
arOp r1 r2 f =
  case r1 of
   VInt x ->
     case r2 of
      VInt y ->
        return $! VInt (f x y)
      _ -> lift $ throwError "Wrong type"
   _ -> lift $ throwError "Wrong type"

arOpExp:: Exp -> Exp -> (Integer -> Integer -> Integer) -> M Value
arOpExp a1 a2 f = do
  r1 <- eval a1
  r2 <- eval a2
  arOp r1 r2 f

incdec :: Exp -> (Integer->Integer) -> M Value
incdec x f = do
  loc <- evalLoc x
  v <- readL loc
  case v of
   VInt i ->
     let res = VInt (f i)
     in do
       writeL (loc, res)
       return res
   _ -> lift $ throwError "Wrong type"


assignOp :: Value -> Value -> Assignment_op -> M Value
assignOp val1 val2 op =
  case op of
   Assign  -> return val2
   AssignMul  -> arOp val1 val2 (*)
   AssignDiv  -> arOp val1 val2 (div)
   AssignMod  -> arOp val1 val2 (mod)
   AssignAdd  -> arOp val1 val2 (+)
   AssignSub  -> arOp val1 val2 (-)

expTrue :: Exp
expTrue = Econst (Ebool CTrue)
----------------------------------------
modEnv :: String -> Loc -> Env -> Env
modEnv str l (venv, fenv) =
  (insert str l venv, fenv)

insVEnv :: String -> Value-> M ((Env -> Env), Loc)
insVEnv name value = do
  loc <- alloc
  writeL (loc, value)
  return (modEnv name loc, loc)
----------------------------------------------- 
parseDeclarator (VDecl t id) =
  let Ident str = id in
   case t of
    RefDec (NoArrayDec x) ->
      return (Ref $Simple x, str)
    NoRefDec (NoArrayDec x)->
      return (NoRef $Simple x, str)
    _ -> lift $throwError "Arrays not implemented"

parseDeclarators x =
  case x of
   h:t -> do
     prev <- parseDeclarators t
     res <- parseDeclarator h
     return (res:prev)
   []-> return []
     

processArgs :: [Exp] -> [FullType] -> M [Value]
processArgs exps types =
  case (exps, types) of
   ([],[]) -> return []
   (exp:t1, tp:t2) -> do
     prev <- processArgs t1 t2
     case tp of
      Ref _ -> do
        loc <- evalLoc exp
        return $ (VLoc loc):prev
      NoRef _ -> do
        val <- eval exp
        return $ val:prev
   _ -> lift $ throwError "Argument count mismatch"
   
checkExec :: FullType -> StmRes -> M Value
checkExec t1 (Val x) = do
  t2 <- getValType x
  checkTypes t1 t2
  return x
checkExec _ _ = lift $ throwError ("Function didn't return a value")  

insertArgs venv tns vals=
  case (tns,vals) of
   ([], []) -> return (venv, [])
   ((t,n):rtn, val:rval) -> do
     (nvenv, locs) <- insertArgs venv rtn rval
     tval <- getValType val
     if (tval /= t)
       then lift (throwError $ "Argument type mismatch")
       else
       case val of
        VLoc l -> 
          return (insert n l nvenv, locs)
        x -> do
          l <- alloc
          writeL(l, x)
          return (insert n l nvenv, l:locs) 
   _ -> lift(throwError$  "Argument count mismatch")

deallocList :: [Loc] -> M ()
deallocList l =
  case l of
   [] -> return ()
(h:t) ->
    dealloc (Just h) >> deallocL
ist t