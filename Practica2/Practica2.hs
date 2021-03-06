import Prelude hiding ((.), all, any, iterate, flip)
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
--c) Enum    enumera un data https://stackoverflow.com/questions/6000511/better-way-to-define-an-enum-in-haskell 
--d) Bounded https://stackoverflow.com/questions/4557394/how-do-you-use-the-bounded-typeclass-in-haskell-to-define-a-type-with-a-floating
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
apply = id
--apply f = f
-- apply (1+) 2

twice :: (a -> a) -> a -> a
twice f = f . f
-- twice = \f -> \x -> f (f x)
-- twice (1+) 3

flip :: (a -> b -> c) -> b -> a -> c
flip f = \x -> \y -> f y x
-- flip (+) 1 

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

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

all :: (a -> Bool) -> [a] -> Bool
all f = and . map f
--all f   []   = True
--all f (x:xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any f = or . map f
--any f   []   = False
--any f (x:xs) = f x || any f xs

and' :: [Bool] -> Bool
and' = all id
--and' [] = True
--and' (x:xs) = x && and' xs
--and' = all (True==)

or' :: [Bool] -> Bool
or' = any id

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f   []   = []
takeWhile2 f (x:xs) | f x = x : takeWhile2 f xs
                    | otherwise = takeWhile2 f []
-- takeWhile2 (>2) [3,4,5,6,2,7,1]

concatMap2 :: (a -> [b]) -> [a] -> [b]
concatMap2 f   []   = []
concatMap2 f (x:xs) = f x ++ concatMap2 f xs 

partition2 :: (a -> Bool) -> [a] -> ([a], [a])
partition2 f   []   = ([], [])
partition2 f (x:xs) = let res = partition2 f xs
                          l1  = fst res
                          l2  = snd res
                      in if f x  
                      	 then (x:l1, l2)
                      	 else (l1, x:l2)
-- partition2 (>2) [1,2,3,4,5,6]

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f [] _  = []
zipWith2 f _  [] = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys
-- zipWith2 (+) [1,2,3,4,5] [6,7,8,9,10,11]

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
and2 :: Bool -> Bool -> Bool
and2 False _   = False 
and2  _  False = False
and2  _    _   = True
-- and2 False undefined

or2 :: Bool -> Bool -> Bool
or2 True _   = True 
or2  _  True = True
or2  _   _   = False
-- or2 True undefined

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y
-- ifThenElse True 1 undefined

-- ¿Es posible dar una definición para estas funciones que tenga el beneficio de cortocircuito aún
--con un orden de evaluación Eager?

-- Extras
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
-- iterate f x = map (\n -> applyN n f x) [0..]

iterate' :: (a -> a) -> a -> [a]
iterate' f x = map (\y -> applyN y f x) [0..]
--iterate' f x = (map ($ x) . map ($ f) . map applyN) [0..]
--                                        [1,2,3,4,5,6]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (n-1) f
-- applyN n f x = iterate f x !! n

--subset [1,2] [1,2,3] = True
--subset [1,2] [1,3,6] = False
subset :: Eq a => [a] -> [a] -> Bool
subset = flip (all . flip elem)
--subset [] _ = True
--subset _ [] = False
--subset (x:xs) ys = elem x ys && subset xs ys

-- Mapa de tesoros

data Dir = Izq | Der

data Objeto = Tesoro | Chatarra

data Mapa = Cofre Objeto | Bifurcacion Objeto Mapa Mapa

--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Cofre x)             = esTesoro x 
hayTesoro (Bifurcacion x m1 m2) = esTesoro x || hayTesoro m1 || hayTesoro m2
--hayTesoro (Bifurcacion Chatarra (Bifurcacion Chatarra (Cofre Chatarra) (Cofre Chatarra)) (Bifurcacion Chatarra (Bifurcacion Chatarra (Cofre Tesoro) (Cofre Chatarra)) (Cofre Chatarra)))

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro    _   = False  

--Indica si al final del camino hay un tesoro. Nota: el final del camino es la lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn    []    (Cofre x)            = esTesoro x
hayTesoroEn    []    (Bifurcacion x _ _)  = esTesoro x
hayTesoroEn (Izq:xs) (Bifurcacion _ m1 _) = hayTesoroEn xs m1
hayTesoroEn (Der:xs) (Bifurcacion _ _ m2) = hayTesoroEn xs m2
--hayTesoroEn [Der,Izq,Izq] (Bifurcacion Chatarra (Bifurcacion Chatarra (Cofre Chatarra) (Cofre Chatarra)) (Bifurcacion Chatarra (Bifurcacion Chatarra (Cofre Tesoro) (Cofre Chatarra)) (Cofre Chatarra)))

--Indica el camino al tesoro. Precondición: hay un sólo tesoro en el mapa.
--caminoAlTesoro :: Mapa -> [Dir]

--Indica el camino de la rama más larga.
--caminoRamaMasLarga :: Mapa -> [Dir]

--Devuelve los tesoros separados por nivel en el árbol.
--tesorosPerLevel :: Mapa -> [[Objecto]]

--Devuelve todos lo caminos en el mapa.
--todosLosCaminos :: Mapa -> [[Dir]]
