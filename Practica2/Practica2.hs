-- 1 Tipos

--Dar tipo a las siguientes expresiones:
--True
--[2]
--Maybe ["Jorge"]
--Nothing
--[]
--let x = [] in x ++ x
--let f x = f x in f [

--Dar ejemplos de expresiones que posean los siguientes tipos:
--Bool
--(Int, Int)
--Int -> Int -> Int
--a -> a
--a
--a -> b

--2 Typeclasses
--Describa el propósito de las siguientes typeclasses:
--a) Eq
--b) Ord
--c) Enum
--d) Bounded
--e) Num
--f) Show
--g) Read

--3 Funciones de alto orden
--Definir las siguientes funciones:

const :: a -> b -> a
const x = \y -> x
--const 1 "2"

alph :: a -> b -> b
alph x = \y -> y
-- alph 1 "2"

apply :: (a -> b) -> a -> b
apply f = f
-- apply (+1) 2

twice :: a -> b -> b
twice x = \y -> y

flip :: (a -> b -> c) -> b -> a -> c
flip f = \x -> \y -> f y x
-- flip (+) 1 2

--f) (.) :: (b -> c) -> (a -> b) -> (a -> c)
--g) curry :: ((a,b) -> c) -> a -> b -> c
--h) uncurry :: (a -> b -> c) -> (a,b) -> c

--4 Recorridos
--Definir las siguientes funciones de alto orden sobre listas:
--a) map :: (a -> b) -> [a] -> a
--b) filter :: (a -> Bool) -> [a] -> [a]
--c) all :: (a -> Bool) -> [a] -> Bool
--d) any :: (a -> Bool) -> [a] -> Bool
--e) takeWhile :: (a -> Bool) -> [a] -> [a]
--f) concatMap :: (a -> [b]) -> [a] -> [b]
--g) partition :: (a -> Bool) -> [a] -> ([a], [a])
--h) zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

--5 Composición
--Sea h x y = f (g x y) . Decidir cuáles de las siguientes afirmaciones son verdaderas:
--a) h = f . g
--b) h x = f . (g x)
--c) h x y = (f . g) x y

--6 Lógica de cortocircuito
--Dada la siguiente definición para los Booleanos:
--data Bool = True | False
--Considerando el orden de evaluación Lazy, definir las siguientes funciones de forma tal que sólo
--se evalué el primer parámetro cuando sea posible (eg. or True bottom debería reducir a True):
--a) and :: Bool -> Bool -> Bool
--b) or :: Bool -> Bool -> Bool
--c) ifThenElse :: Bool -> a -> a -> a
-- ¿Es posible dar una definición para estas funciones que tenga el beneficio de cortocircuito aún
--con un orden de evaluación Eager?

