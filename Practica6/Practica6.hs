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

data LTree a = L [a] | B a (LTree a) (LTree a)

foldLT :: ([a] -> b) -> (a -> b -> b -> b) -> LTree a -> b
foldLT fl fb (L xs) = fl xs
foldLT fl fb (B x t1 t2) = fb x (foldLT fl fb t1) (foldLT fl fb t2)

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

