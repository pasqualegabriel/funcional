{-# LANGUAGE RankNTypes #-}



--Lambda Booleans
--Dada las siguientes definiciones que representan a los booleanos mediante funciones:

type BoolLam a = a -> a -> a

trueLam = \ x y -> x

falseLam = \ x y -> y

--Y considerando las siguientes operaciones simples sobre esta representación de los booleanos:

ifThenElseLam = \ x -> x

notLam = \ x -> ifThenElseLam x falseLam trueLam

--Definir las siguientes operaciones (que se comportan como sus contrapartes booleanas):
--orLam :: BoolLam a -> BoolLam a -> BoolLam a
orLam b1 b2 = b1 trueLam b2

--andLam :: BoolLam a -> BoolLam a -> BoolLam a
andLam b1 b2 = b1 b2 falseLam

--Lambda Pairs
--Dada la siguiente definición de pairLam que representa a los pares mediante funciones 
--(e.g. (pairLam 1 True) es una expresión que denota el par (1,True) ):

--data Either' a b = Left' a | Right' b

type Projector a b = a -> b -> Either a b

left:: a -> b -> Either a b 
left = \ x y -> Left x

right:: a -> b -> Either a b
right = \ x y -> Right y

type PairLam a b = a -> b -> Projector a b -> Either a b

pairLam:: a -> b -> Projector a b -> Either a b
pairLam = \ x y p -> p x y

--Definir las siguientes operaciones (que se comportan como las proyecciones de las componentes del par):

--(e.g. fstLam (pairLam 1 2) debería retornar (Left 1) ).
--fstLam :: PairLam a b -> Either a b
fstLam f = f left 
sndLam f = f right


--(e.g. fstLam (pairLam 1 True) debería retornar (Right True) ).
--sndLam :: PairLam a b -> Either' a b 

--Lambda Sets

type Set a = a -> Bool

belongs ::Set a -> a -> Bool
belongs  = id  

belongs' :: a -> Set a -> Bool
belongs' x f = ( f . id) x

singleton :: Eq a => a -> Set a
singleton  = (==)


--Defina las siguientes operaciones para conjuntos representados por extensión (como [a] ) y
--alternativamente por comprensión (como predicado a -> Bool ) cuando sea posible.

union :: Set a -> Set a -> Set a
union s1 s2 = \x -> (s1 x) || (s2 x)


intersect :: Set a -> Set a -> Set a
intersect s1 s2 = \x -> (s1 x) && (s2 x)

--Devuelve los elementos que no estan en el set 
complement  :: Set a -> Set a
complement  =  (.) not  

--Mala leche 
--cardinal :: Set a -> Nat

-- image:: Set (a,b) -> a -> Set b
-- image s x = \y - >  s(x,y) 
-- image = \s -> curry s
-- image = curry

--  odds :: Set Int
--  odds = odd     
  



--Fuzzy Sets
--Un conjunto difuso (o fuzzy set en inglés) indica para cada elemento x un nivel de certeza de
--que x pertenezca al conjunto (i.e., un número real entre 0 y 1, donde 0 indica que con certeza el
--elemento no pertenece al conjunto y 1 indica que con certeza el elemento sí pertenece al conjunto).
--Considere una representación de fuzzy sets mediante funciones:

--type Fuzzy a = a -> Float

--Y defina la siguientes operaciones:

--belongs :: Fuzzy a -> a -> Float
--complement :: Fuzzy a -> Fuzzy a
--union :: Fuzzy a -> Fuzzy a -> Fuzzy a
--intersect :: Fuzzy -> Fuzzy a -> Fuzzy a



--Church Numerals

--Dada la siguiente representación de números mediante funciones (donde un número n se repre-
--senta mediante n composiciones de una función f , es decir, la repetición de n aplicaciones sucesivas
--de la función f ):

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (n-1) f


type Numeral = ( Int -> Int ) -> Int -> Int

zero' = \ f x -> x -- 0 aplicaciones de la funcion f

succ' n = \ f x -> f ( n f x) -- 1 aplicacion mas de la funcion f

--Defina la siguientes operaciones:

--Retorna el numeral que representa a un número dado.


-- church :: Int -> Numeral Int
-- church x = applyN x succ'


--Retorna el entero representado por un numeral.
-- unchurch :: Numeral  -> Int
-- unchurch f = f 0  

--Retorna el numeral que representa la suma de otros dos.
--add :: Numeral a -> Numeral a -> Numeral a

--Retorna el numeral que representa el producto de otros dos.
--mul :: Numeral a -> Numeral a -> Numeral a 
