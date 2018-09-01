-- Descomentar para poder compilar ChurchBool and Church numerals
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (succ, concat, and, or, not, sum)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (n-1) f

type Nat = (Int -> Int)

zero :: Nat
zero = id

succ :: Nat
succ = (+1)

sum :: Nat -> Nat -> Nat
sum n1 n2 = n1 . n2
--toInt (sum (sum succ succ) succ)

mult :: Nat -> Nat -> Nat
mult = applyN . toInt
--toInt (mult (sum succ succ) (sum succ succ))

toInt :: Nat -> Int
toInt n = n 0

fromInt :: Int -> Nat
fromInt x = applyN x succ 
--toInt (fromInt 6)

-------------------------------------------------------------

type Map k v = (k -> Maybe v)

--lookupM :: Eq k => k -> Map k v -> Maybe v

--emptyM :: Map k v

--assocM :: Eq k => k -> v -> Map k v -> Map k v  

--deleteM :: Eq k => k -> Map k v -> Map k v

-- domM :: Map k v -> [k]
-- domM m = ??

-------------------------------------------------------------

type DList a = ([a] -> [a])

--nil :: DList a

--toList :: DList a -> [a]

--fromList :: [a] -> DList a

--cons :: a -> DList a -> DList a

--snoc :: DList a -> a -> DList a

--singleton :: a -> DList a

--append :: DList a -> DList a -> DList a

--concat :: [DList a] -> DList a

--head' :: DList a -> a

--tail' :: DList a -> DList a

-------------------------------------------------------------

-- type ChurchBool = forall a. a -> a -> a

-- true :: ChurchBool
-- true = \t -> \f -> t

-- false :: ChurchBool
-- false = \t -> \f -> f

-- ifelse :: ChurchBool -> a -> a -> a
-- ifelse = \p -> \a -> \b -> p a b

-- and :: ChurchBool -> ChurchBool -> ChurchBool
-- and = \a -> \b -> a b false

-- or :: ChurchBool -> ChurchBool -> ChurchBool
-- or = \a -> \b -> a true b

-- not :: ChurchBool -> ChurchBool
-- not = \p -> \a -> \b -> p b a

-- xor :: ChurchBool -> ChurchBool -> ChurchBool
-- xor = \a -> \b -> a (not b) b

-- unchurch_bool :: (Bool -> Bool -> a) -> a
-- unchurch_bool = (\a -> \b -> \c -> c a b) True False

-------------------------------------------------------------

-- Sólo para valientes a modo de desafío
-- type ChurchNum = forall a. (a -> a) -> a -> a

-- zero' :: ChurchNum
-- zero' = \f -> \x -> x

-- one' :: ChurchNum
-- one' = \f -> \x -> f x

-- two' :: ChurchNum
-- two' = \f -> \x -> f (f x)

-- three' :: ChurchNum
-- three' = \f -> \x -> f (f (f x))

-- num' :: Int -> ChurchNum
-- num' n =  \f -> \x -> applyN n f x

-- succ' :: ChurchNum -> ChurchNum
-- succ' n = \f -> \x -> f (n f x)

-- add' :: ChurchNum -> ChurchNum -> ChurchNum
-- add' n m = \f -> \x -> m f (n f x)

-- mult' :: ChurchNum -> ChurchNum -> ChurchNum
-- mult' = \m -> \n -> \f -> m (n f)

-- is_zero :: ChurchNum -> ChurchBool
-- is_zero = \n -> n (\x -> false) true

-- Desafío supremo
-- pred' :: ChurchNum
-- pred' = ...

-- sub' :: ChurchNum -> ChurchNum -> ChurchNum
-- sub' = \m -> \n -> n pred m

-- unchurch_int :: ChurchNum -> Int
-- unchurch_int = \a -> a (\b -> b + 1) 0