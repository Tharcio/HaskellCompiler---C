{-modEnv :: String -> Loc -> Env -> Env
modEnv str l (venv, fenv) =
  (insert str l venv, fenv)

insVEnv :: String -> Int-> M ((Env -> Env), Loc)
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
-}

import Text.Parsec 
import Text.Parsec.Char
import System.IO
import Data.Char(toUpper)
 
getStringIO IO
main :: IO()
main = do
     inh <- openFile "C:/Users/tmams/Documents/tharcio/HaskellCompiler---C-master/entrada.txt" ReadMode;
     outh <- openFile "C:/Users/tmams/Documents/tharcio/HaskellCompiler---C-master/saida.txt" WriteMode;
     inpStr <- hGetContents inh;
     s <-Stream (inpStr);
     hPutStr outh (map toUpper inpStr);
     hClose inh;
     hClose outh;
 {-
mainloop inh outh = 
         do ineof <- hIsEOF inh
            if ineof
               then return()
               else do inpStr <- hGetLine inh
                       hPutStrLn outh (map toUpper inpStr)
                       mainloop inh outh

lerArquivo :: IO ([Char]) -> [String]
lerArquivo -}