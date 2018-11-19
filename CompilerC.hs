

data Exp = Constant Int 
 | Variable String
 | Minus Exp Exp
 | Plus Exp Exp
 | Times Exp Exp
 | Divided Exp Exp
 | Greater Exp Exp
 | GreaterEqual Exp Exp
 | Smaller Exp Exp
 | SmallerEqual Exp Exp
 | Equal Exp Exp
 deriving (Show)

data Com = Assign String Exp
 | Seq Com Com
 | Cond Exp Com Com
 | While Exp Com
 | Declare String Exp Com
 | Print Exp
 deriving (Show)

type Location = Int
type Index = [ String ]
type Stack = [(String,Int)]
type Code = [Op]

data Op = PUSH Int | ADD | SUB | MULT | DIV
          deriving Show

initStack::[(String,Int)] -> Stack
initStack s = s

getStringStack :: Stack -> String
getStringStack ((str,val):xs) = str


getIntStack :: Stack -> Int
getIntStack ((str,val):xs) = val

stackUse = initStack [];

newtype M a = StOut (Stack -> (a, Stack, String))
newtype N a = St (Stack -> (a, Stack))
--StOut (\x-> (1,(initStack x),""))
unStOut (StOut f) = f

exec :: Code -> Stack -> Stack
exec []           s           = s
exec (PUSH n : c) s           = exec c (("int",n) : s)
exec (ADD : c)    ((w1,m) : (w2,n) : s) = exec c ((w1,n+m) : s)
exec (SUB : c)    ((w1,m) : (w2,n) : s) = exec c ((w1,n-m) : s)
exec (MULT : c)    ((w1,m) : (w2,n) : s) = exec c ((w1,n*m) : s)
exec (DIV : c)    ((w1,m) : (w2,n) : s) = exec c ((w1,(div n m)) : s)

compute :: M a -> Stack -> (a,Stack,String)
compute (StOut st) s = st s --(1,s,"String")

item :: Int -> M Int
item n = StOut (\st -> case st of 
	[] -> (0,[],"")
	((s,n):xs) -> (n,xs,s))

parse :: N a -> Stack -> (a, Stack)
parse (St s) inp = s inp
{-
app :: N a -> Stack-> (a, Stack)
app (StOut st) x = (unStOut st) x

dn :: Stack -> (a,Stack)
dn s = (StOut,s,"")

calcule :: Stack -> Code-> (a,Stack)
calcule s1@((w,n):(w1,n1):xs) cd@(y:ys) = (cd, exec cd s1)

compute :: M a -> Stack -> (a,Stack,String)
compute (StOut st) s = (unStOut st) s

transf ::Num a => (a,Stack,String) -> (a,Stack,String)
transf (c,stk,str) = (getIntStack stk, stk, getStringStack stk)
-}
instance Functor N where
    fmap f s = St (\ra -> case parse s ra of
        (v,out) -> (f v, out))

instance Functor M where
	 fmap f m@(StOut st) = StOut (\x-> case compute m x of
		(a,state,string) -> (f a, state,string))

instance Applicative M where
	pure a = StOut (\state-> (a,state,""))
	f <*> g = StOut (\x-> case compute f x of
		(a,state,string) -> compute (fmap a g)  (state))

instance Monad M where
	return a = StOut (\st -> (a,st,""))
	(>>=) (StOut s) f = StOut (\st -> let (v, newS, string) = s st
	                                      (StOut resF) = f v
	                                      in resF newS )

{-										
instance Show M () where
	showsPrec n (StOut st) = showsPrec n st
-}

--A posição é um inteiro
position :: String -> Index -> Location
position name index = let
 pos n (nm: nms) = if name == nm
 then n
 else pos (n+1) nms in pos 1 index
-- Obter o enésimo valor (específico de uma posição)
fetch :: Location -> Stack -> Int
fetch l ((s,v) : vs ) = if l == 1 then v else fetch (l-1) vs
{- Uma função para computar um ambiente atualizado. Obtém-se
uma nova pilha baseada no número da posição atualizada, do valor
armazenado e do conteúdo anterior da pilha.-}
aux :: Location -> Int -> Int -> Stack -> Stack
aux l a n s@(x:xs)
 | (l>a) = [x] ++ aux (l) (a+1) (n) (xs)
 | otherwise = [x] ++ [("",n)] ++ xs

put :: Location -> Int -> Stack -> Stack
put l n s@(x:xs) = aux l 0 n s


{- para retornar o valor de um ambiente como principal resultado de
uma computação-}
getfrom :: Location -> Stack -> M Int
getfrom l = (\s-> item (fetch l s)) 

-- para modificação da pilha
-- write :: Location -> Int -> M ()

{- para modificar a pilha, sem modificação do índice, para colocar
um valor no topo da pilha. Pode ser útil ao declarar uma variável,
por exemplo. 
push :: Int -> M ()
push n = [n] ++ stack >>= return 
-}



{-Escrever a expressão de avaliação que, dada uma expressão e uma tabela
(índice), retorna um valor monádico com um inteiro.-}
eval1 :: Exp -> Index -> M Int
eval1 exp index = case exp of 
 (Constant n) -> return $ (n)
 (Variable x) ->let loc = position x index in getfrom loc stackUse
 (Plus x y) -> arOpExp x y (+) index
 (Minus x y) -> arOpExp x y (-) index
 (Times x y) -> arOpExp x y (*) index 
 (Divided x y) -> arOpExp x y div index
 (Greater x y) -> cmp x y (>) index
 (Smaller x y) -> cmp x y (<) index
 (GreaterEqual x y) -> cmp x y (>=) index
 (SmallerEqual x y) -> cmp x y (<=) index
 (Equal x y) -> cmp x y (==) index

{-• Escrever a função para execução de comandos que recebe como argumento
um comando e uma tabela (índice), retornando um valor monádico
com unit (tupla vazia) como resultado.

exec :: Com −> Index −>M( )
exec stmt index = case stmt of
Assign name e −> let loc = position name index in do { v <−eval1 e index;
write loc v }
(Seq com1 com2) -> let loc = position name index in do { v <-eval1 e index; }
(Cond exp com1 com2) -> let loc = position name index in do { v <-eval1 e index; }
(While exp com) -> let loc = position name index in do { v <-eval1 e index; }
(Declare String Exp Com) -> let loc = position name index in do { v <-eval1 e index; }
(Print exp) -> let loc = position name index in do { v <-eval1 exp index; Print v }

-}

cmp:: Exp -> Exp -> (Int -> Int -> Bool) -> Index-> M Int
cmp a1 a2 f i = do
  r1 <- eval1 a1 i
  case r1 of
    x -> do
     r2 <- eval1 a2 i
     case r2 of
       y ->
        return $ (do if (f x y) ==True then 1 else 0)

arOp :: Int -> Int -> (Int ->Int ->Int) -> M Int
arOp r1 r2 f =
  case r1 of
    x ->
     case r2 of
       y ->
        return $!  (f x y)

arOpExp:: Exp -> Exp -> (Int -> Int -> Int) -> Index-> M Int
arOpExp a1 a2 f i = do
  r1 <- eval1 a1 i
  r2 <- eval1 a2 i
  arOp r1 r2 f
{-
incdec :: Exp -> (Int->Int) -> M Int
incdec x f = do
  loc <- evalLoc x
  v <- readL loc
  case v of
    i ->
     let res =  (f i)
     in do
       writeL (loc, res)
       return res

incdec :: Exp -> (Int->Int) -> M Int
incdec x f = do
	 loc <- evalLoc x
	 v <- readL loc
	 case v of
	   i ->
		let res =  (f i)
		in do
		  writeL (loc, res)
		  return res
	  _ -> lift $ throwError "Wrong type"
   
   
assignOp :: Int -> Int -> Assignment_op -> M Int
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
-}