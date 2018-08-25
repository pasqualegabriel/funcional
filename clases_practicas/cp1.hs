-- 1. Booleanos
-- Dada la siguiente definición para los Booleanos:

-- data Bool = True | False

-- Denir las siguientes funciones:

-- c) or :: Bool -> Bool -> Bool
or' :: Bool -> Bool -> Bool
or' True _  = True
or' False x = x

-------------------------------------------------

--2. Pares
-- Dada la siguiente definición para los Pares:

-- data (a , b ) = (a , b )

-- Definir las siguientes funciones:

-- c) swap :: (a,b) -> (b,a)
swap' :: (a,b) -> (b,a)
swap' (x, y) = (y, x)

-------------------------------------------------

-- 3. Maybe
-- Dada la siguiente definición para los Maybe:

-- Maybe a = Nothing | Just a

-- Definir las siguientes funciones:

-- c) liftMaybe :: a -> Maybe a
liftMaybe' :: a -> Maybe a
liftMaybe' x = Just x

-- e) maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f Nothing  = Nothing
maybeApply f (Just x) = Just (f x)

-------------------------------------------------

-- 4 Números naturales
-- Dada la siguiente definición para los Nat:

data Nat = Zero | Suc Nat deriving Show

-- Definir las siguientes funciones:

-- c) sub :: Nat -> Nat -> Maybe Nat
sub :: Nat -> Nat -> Maybe Nat
sub x Zero          = Just x
sub (Suc n) (Suc m) = sub n m
sub _ _             = Nothing

-------------------------------------------------

-- 5. Listas
-- Dada la siguiente definición para las listas:

-- data [ a ] = [] | a : [ a ] -- notacion especial de Haskell

-- Definir las siguientes funciones:

-- e) (++) :: [a] -> [a] -> [a]
append :: [a] -> [a] -> [a]
append [] ys      = ys
append (x: xs) ys = x: (append xs ys)

-------------------------------------------------

-- 6. Patrones
--Indicar si los siguientes patterns son correctos:

-- d) ('a',('a',b)) -- Es correcto.Just
-- e) (a,(a,b))     -- No es correcto, porque la variable "a" aparece dos veces.
-- f) ([]:[4])      -- No es correcto, ya que la primer lista no es del tipo de los elementos de la segunda.

-------------------------------------------------

-- 7.Reducción
--Reduzca las siguientes expresiones hasta alcanzar la forma normal (indique la regla utilizada en cada paso):

-- c) fst (swap (True, False))

-- fst (swap (True, False))
-- swap.1 (Regla 1 de swap)
-- fst (False, True)
-- fst.1
-- False
