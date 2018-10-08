{-# LANGUAGE RankNTypes #-}

import Prelude hiding (length, map, filter, elem, take)

length :: [a] -> Int
length = foldr (\_ r -> 1 + r) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse :: [a] -> [a]
reverse = foldr snoc []

elem :: Eq a => a -> [a] -> Bool
elem x = foldr ((||) . (x==)) False
--elem x = foldr (\e r -> x==e || r) False

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []
--map f = foldr (\x r -> f x : r) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x r -> if f x then x:r else r) []

take :: Int -> [a] -> [a]
take n xs = foldr step (const []) xs n
  where
    step x g 0 = []
    step x g n = x:g (n-1)

fold1 :: (a -> a -> a) -> [a] -> a
fold1 f   [x]  = x
fold1 f (x:xs) = f x (fold1 f xs)

maximum1 :: Ord a => [a] -> a
maximum1 = fold1 max

minimum1 :: Ord a => [a] -> a
minimum1 = fold1 min

-- Matrix

type Matrix a = [[a]]

repokm :: Matrix a -> Bool
repokm xss = foldr lengths (const True) xss (length (head xss))
    where
      lengths x r l = length x == l && r l

getM :: Matrix a -> Int -> Int -> a
getM xss c f = foldr mn (error "error") (foldr mn (error "s") xss c) f
    where
      mn xs r 0 = xs 
      mn xs r c = r (c - 1)

-- TipTree

data TipTree a = Tip a | Join (TipTree a) (TipTree a)

foldTT :: (a -> b) -> (b -> b -> b) -> TipTree a -> b
foldTT f g (Tip x)    = f x
foldTT f g (Join l r) = g (foldTT f g l) (foldTT f g r)

sizeTT :: TipTree a -> Int
sizeTT = foldTT (const 1) (\l r -> 1 + l + r) 

heightTT :: TipTree a -> Int
heightTT = foldTT (const 1) (\l r -> max (1 + l) (1 + r))

walkthroughTT :: TipTree a -> [a]
walkthroughTT = foldTT (:[]) (++)

mirrorTT :: TipTree a -> TipTree a
mirrorTT = foldTT Tip (\l r -> Join r l)

mapTT :: (a -> b) -> TipTree a -> TipTree b
mapTT f = foldTT (Tip . f) Join

-- Poli

data Poli = Cte Int | VarP | Add Poli Poli | Mul Poli Poli

foldPoli :: (Int -> b) -> b -> (b -> b -> b) -> (b -> b -> b) -> Poli -> b
foldPoli f b g h (Cte x)   = f x
foldPoli f b g h   VarP    = b
foldPoli f b g h (Add l r) = g (foldPoli f b g h l) (foldPoli f b g h r)
foldPoli f b g h (Mul l r) = h (foldPoli f b g h l) (foldPoli f b g h r)

evalP :: Poli -> Int -> Int
evalP p n = foldPoli id n (+) (*) p
--evalP (Add (Mul VarP VarP) (Add (Mul (Cte 3) VarP) (Cte 5))) 2 

mEscalar :: Poli -> Int -> Poli
mEscalar p n = foldPoli (\x -> Mul (Cte n) (Cte x)) (Mul (Cte n) VarP) Add Mul p

--sOptimize :: Poli -> Poli


-- Parcial Mapa

data Dir = L | R | S

data Mapa a = Cofre [a] | Nada (Mapa a) | B [a] (Mapa a) (Mapa a)

foldMapa :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldMapa f g h (Cofre xs) = f xs
foldMapa f g h (Nada m)   = g (foldMapa f g h m) 
foldMapa f g h (B xs l r) = h xs (foldMapa f g h l) (foldMapa f g h r)

objects :: Mapa a -> [a]
objects = foldMapa id id (\xs l r -> xs ++ l ++ r) 
--objects (B [1,2] (B [3,4,5] (Cofre [6,7]) (Nada (Cofre [8,9]))) (Cofre [10]))

mapMapa :: (a -> b) -> Mapa a -> Mapa b
mapMapa f = foldMapa (Cofre . (map f)) Nada (B . (map f))
--mapMapa (+100) (B [1,2] (B [3,4,5] (Cofre [6,7]) (Nada (Cofre [8,9]))) (Cofre [10]))

hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f (Cofre xs)   []   = any f xs
hasObjectAt f (B xs l r)   []   = any f xs
hasObjectAt f (Nada m)   (S:ds) = hasObjectAt f m ds 
hasObjectAt f (B xs l r) (L:ds) = hasObjectAt f l ds
hasObjectAt f (B xs l r) (R:ds) = hasObjectAt f r ds
--hasObjectAt (9==) (B [1,2] (B [3,4,5] (Cofre [6,7]) (Nada (Cofre [8,9]))) (Cofre [10])) [L,R,S]

--hasObjectAtFold :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
--hasObjectAtFold f = foldMapa (const (any f ds)) h i
	--where
	--	h r (S:ds) = r ds



