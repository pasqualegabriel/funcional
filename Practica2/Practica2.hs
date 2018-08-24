-- 1 Tipos

--Dar tipo a las siguientes expresiones:
--True
--[2]
--Maybe ["Jorge"]
--Nothing
--[]
--let x = [] in x ++ x
--let f x = f x in f []
--undefined (o bottom )

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
-- apply (1+) 2

twice :: a -> b -> b
twice x = \y -> y

flip :: (a -> b -> c) -> b -> a -> c
flip f = \x -> \y -> f y x
-- flip (+) 1 2

--(.) :: (b -> c) -> (a -> b) -> (a -> c)

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)
--curry fst 2 3

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b
--uncurry (+) (1,2)

--4 Recorridos
--Definir las siguientes funciones de alto orden sobre listas:
 
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = [ f x | x <- xs]

map3 :: (a -> b) -> [a] -> [b]
map3 f   []   = []
map3 f (x:xs) = f x : map3 f xs
--map2 (1+) [1,2,3,4,5,6]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f xs = [ x | x <- xs, f x ] 

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f   []   = []
filter3 f (x:xs) | f x = x : filter3 f xs
                 | otherwise = filter3 f xs
--filter2 (>5) [1,6,2,8,9,3]

all2 :: (a -> Bool) -> [a] -> Bool
all2 f   []   = True
all2 f (x:xs) = f x && all2 f xs

any2 :: (a -> Bool) -> [a] -> Bool
any2 f   []   = False
any2 f (x:xs) = f x || any2 f xs

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f   []   = []
takeWhile2 f (x:xs) | f x = x : takeWhile2 f xs
                    | otherwise = takeWhile2 f []
-- takeWhile2 (>2) [3,4,5,6,2,7,1]

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

-- Extras
positives :: [Int] -> [Int]
positives [] = []
positives (x:xs) | x > 0 = x : positives xs
                 | otherwise = positives xs
--positives xs = [ x | x <- xs, x > 0 ]