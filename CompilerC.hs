

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
type Stack = [Int]
type Code = [Op]

data Op = PUSH Int | ADD | SUB | MULT | DIV
          deriving Show

initStack::[Int] -> Stack
initStack s = s

initIndex::[String] -> Index
initIndex i = i

stackUse = initStack []
indexUse = initIndex []

newtype M a = StOut (Stack -> (a, Stack, String))

unStOut (StOut f) = f

exec1 :: Code -> Stack -> Stack
exec1 []           s           = s
exec1 (PUSH n : c) s           = exec c (n : s)
exec1 (ADD : c)    (m : n : s) = exec c ((n+m) : s)
exec1 (SUB : c)    (m : n : s) = exec c ((n-m) : s)
exec1 (MULT : c)    (m : n : s) = exec c ((n*m) : s)
exec1 (DIV : c)    (m : n : s) = exec c ((div n m) : s)

compute :: M a -> Stack -> (a,Stack,String)
compute (StOut st) s = st s

item :: Int -> M Int
item n = StOut (\st -> case st of 
	[] -> (0,[],"")
	(x:xs) -> (n,x:xs,head indexUse))


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
auxPut :: Location -> Int -> Int -> Stack -> Stack
auxPut l a n s@(x:xs)
 | (l>a) = [x] ++ auxPut (l) (a+1) (n) (xs)
 | otherwise = [x] ++ [n] ++ xs

put :: Location -> Int -> Stack -> Stack
put l n s@(x:xs) = auxPut l 0 n s


{- para retornar o valor de um ambiente como principal resultado de
uma computação-}
getfrom :: Location -> Stack -> M Int
getfrom l = (\s-> item (fetch l s)) 

-- para modificação da pilha
write :: Location -> Int -> M ()
write l n = (\s -> (n, put l n s, head indexUse) ) useStack

{- para modificar a pilha, sem modificação do índice, para colocar
um valor no topo da pilha. Pode ser útil ao declarar uma variável,
por exemplo. -}
push :: Int -> M ()
push n = StOut (\s-> (n,(n:s),head indexUse)) useStack


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
-}

exec :: Com -> Index -> M( )
exec stmt idx = case stmt of
Assign name e = let loc = position name idx in do { v <- eval1 e idx;
write loc v }
(Seq com1S com2S) = {-let loc = position name idx in-} do { exec com1S idx; exec com2S idx}
(Cond expC com1C com2C) = let loc = position name idx in do { v <- eval1 expC idx; 
if v==1 then exec com1C idx else exec com2C idx  }
(While expW comW) = let loc = position name idx in do { v <-eval1 expW idx; 
if v == 1 then exec comW idx >>= exec stmt idx; else return $ ()  }
(Declare string expD ComD) = let loc = position name idx in do { v <-eval1 expD idx; write  }
(Print expP) = let loc = position name idx in do { v <- eval1 expP idx; Print v }


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
