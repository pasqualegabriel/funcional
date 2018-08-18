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


--cabeza :: [a] -> a --(retorna el primer elemento)


--sinPrimero :: [a] -> [a] --(retorna la lista con todos los elementos menos el primero)


--size :: [a] -> Int --(retorna la longitud de la lista)


--(+++) :: [a] -> [a] -> [a] --(concatena dos listas)


--belongs :: Eq a => a -> [a] -> Bool --(indica si un elemento pertenece a la lista)


--(!!!) :: [a] -> Int -> a --(retorna el i-ésimo elemento de la lista indexado desde 0)


--alreves :: [a] -> [a] --(retorna la lista al revés)


-- Practica 6
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

-- Practica 7
--Reduzca las siguientes expresiones hasta alcanzar la forma normal 
--(indique la regla utilizada en cada paso):

--a) add (Suc Zero) (Suc Zero)
--b) isNothing (sub Zero (Suc Zero))
--c) fst (swap (True, False))
--d) length [1,2,3]
--e) [1,2,3] !! 2
--f) not (elem 2 [1,2,3])
