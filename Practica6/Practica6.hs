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
repokm xs = foldr lengths (const True) xs xs
    where
      lengths x r [] = True
      lengths x r xs = length x == length (head xs) && r xs

getM :: Matrix a -> Int -> Int -> a
getM xss c f = foldr mn (error "error") (foldr mn (error "s") xss c) f
    where
      mn xs r 0 = xs 
      mn xs r c = r (c - 1)




