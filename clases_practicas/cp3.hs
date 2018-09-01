-- Descomentar para poder compilar ChurchBool and Church numerals
-- {-# LANGUAGE RankNTypes #-}

import Prelude hiding (succ, concat, and, or, not)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f = id
applyN n f = f . applyN (n-1) f

type Nat = (Int -> Int)

zero :: Nat
zero = (+0)
-- zero = id

succ :: Nat
succ = (+1)

sum :: Nat -> Nat -> Nat
sum s1 s2 = s1 . s2

-- mult :: Nat -> Nat -> Nat
-- mult = ???

toInt :: Nat -> Int
toInt n = n 0

mult :: Nat -> Nat -> Nat
mult s1 s2 = applyN (toInt s1) s2
-- observacion:: applyN (toInt s1) s2 = applyN (toInt s2) s1

fromInt :: Int -> Nat
fromInt n = applyN n succ

-------------------------------------------------------------

type Map k v = (k -> Maybe v)

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k m = m k

emptyM :: Map k v
emptyM = (\k -> Nothing)

assocM :: Eq k => k -> v -> Map k v -> Map k v  
assocM k v m = (\k' -> if k == k' then Just v else m k')

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k m = (\k' -> if k == k' then Nothing else m k')

-- domM :: Map k v -> [k]
-- domM m = ??

-------------------------------------------------------------

type DList a = ([a] -> [a])

nil :: DList a
nil = (\xs -> xs)
-- nil = id

toList :: DList a -> [a]
toList xs = xs []

fromList :: [a] -> DList a
fromList xs = (++ xs)

cons :: a -> DList a -> DList a
cons x xs   = (x:) . xs

snoc :: DList a -> a -> DList a
snoc xs x   = xs . (x:)

singleton :: a -> DList a
singleton x = (x:)

append :: DList a -> DList a -> DList a
append xs ys = xs . ys

concat :: [DList a] -> DList a
concat [] = nil
concat (x:xs) = append x (concat xs)

head' :: DList a -> a
head' xs = head (toList xs)

tail' :: DList a -> DList a
tail' xs = fromList (tail (toList xs))

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