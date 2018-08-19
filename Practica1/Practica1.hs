-- Ejercicio 1
--data Bool = True | False

not2 :: Bool -> Bool
not2 True = False
not2  _   = True

and :: Bool -> Bool -> Bool
and True True = True
and   _   _   = False

or :: Bool -> Bool -> Bool
or False False = False
or   _    _    = True

equal :: Bool -> Bool -> Bool
equal True  True  = True
equal False False = True
equal   _    _    = False

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y

-- Ejercicio 2
--data (a, b) = (a , b)

first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, y) = y

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

(===) :: (Eq a, Eq b) => (a,b) -> (a,b) -> Bool
(===) (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

(<==) :: (Ord a, Ord b) => (a,b) -> (a,b) -> Bool
(<==) (x1, y1) (x2, y2) = x1 <= x2 && y1 <= y2

-- Ejercicio 3
-- Maybe a = Nothing | Just a

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing    _    = False

fromJust :: Maybe a -> a
fromJust (Just x) = x

liftMaybe :: a -> Maybe a
liftMaybe x = Just x

(====) :: Eq a => Maybe a -> Maybe a -> Bool
(====) Nothing Nothing   = True
(====)   _     Nothing   = False
(====) Nothing    _      = False
(====) (Just x) (Just y) = x == y

maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply _ Nothing  = Nothing
maybeApply f (Just x) = Just (f x)
--maybeApply (==1) (Just 1)

-- Ejercicio 4 
data Nat = Zero | Suc Nat

inc :: Nat -> Nat
inc n = Suc n

add :: Nat -> Nat -> Nat
add n Zero = n
add Zero n = n
add (Suc n1) n2 = add n1 (Suc n2) 
--add (Suc Zero) (Suc (Suc Zero))

sub :: Nat -> Nat -> Maybe Nat
sub Zero Zero = Just Zero
sub n Zero = Just n
sub Zero n = Nothing
sub (Suc n1) (Suc n2) = sub n1 n2
--sub (Suc (Suc Zero)) (Suc Zero)

(=====) :: Nat -> Nat -> Bool
(=====) Zero Zero = True
(=====) Zero  _   = False
(=====)  _   Zero = False
(=====) (Suc n1) (Suc n2) = n1 ===== n2 
-- (Suc (Suc Zero)) ===== (Suc (Suc Zero))

(>==) :: Nat -> Nat -> Bool
(>==) _ Zero = True
(>==) Zero _ = False
(>==) (Suc n1) (Suc n2) = n1 >== n2
-- (Suc (Suc Zero)) >== (Suc (Suc Zero))

-- Ejercicio 5
--data [ a ] = [] | a : [ a ]

vacia :: [a] -> Bool --(indica si está vacía)
vacia [] = True
vacia _  = False

cabeza :: [a] -> a --(retorna el primer elemento)
cabeza (x:_) = x

sinPrimero :: [a] -> [a] --(retorna la lista con todos los elementos menos el primero)
sinPrimero (_:xs) = xs

size :: [a] -> Int --(retorna la longitud de la lista)
size   []   = 0
size (x:xs) = 1 + size xs 

(+++) :: [a] -> [a] -> [a] --(concatena dos listas)
(+++) xs [] = xs
(+++) [] ys = ys
(+++) (x:xs) ys = x : xs +++ ys 
 
belongs :: Eq a => a -> [a] -> Bool --(indica si un elemento pertenece a la lista)
belongs _   []   = False
belongs e (x:xs) = e == x || belongs e xs

(!!!) :: [a] -> Int -> a --(retorna el i-ésimo elemento de la lista indexado desde 0)
(!!!) (x:_)  0 = x 
(!!!) (x:xs) n = xs !!! (n -1)

alreves :: [a] -> [a] --(retorna la lista al revés)
alreves   []   = []
alreves (x:xs) = (alreves xs) ++ [x]

-- Ejercicio 6
-- Patrones: Indicar si los siguientes patterns son correctos:
--a) (x, y)         
--b) (1, y)
--c) (n+1)         
--d) ('a',('a',b)) 
--e) (a,(a,b))
--f) ([]:[4])
--g) (x:y:[])
--h) [x]
--i) ([]:[])

-- Ejercicio 7
--Reduzca las siguientes expresiones hasta alcanzar la forma normal 
--(indique la regla utilizada en cada paso):

--a) add (Suc Zero) (Suc Zero)
--      Suc (Suc Zero)

--b) isNothing (sub Zero (Suc Zero))
--   isNothing Nothing
--   True

--c) fst (swap (True, False))
--   fst (False, True)
--   False

--d) length [1,2,3]
--   1 + length [2,3]
--   1 + 1 + length [3]
--   1 + 1 + 1 + length []
--   1 + 1 + 1 + 0
--   3

--e) [1,2,3] !! 2
--   [2,3] !! 1
--   [3] !! 0
--   3

--f) not (elem 2 [1,2,3])
--   not (2 == 1 || elem 2 [2,3])
--   not (2 == 1 || 2 == 2 || elem 2 [3])
--   not (2 == 1 || 2 == 2 || 2 == 3 || elem 2 [])
--   not (2 == 1 || 2 == 2 || 2 == 3 || False)
--   not True
--   False