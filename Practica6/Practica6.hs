{-# LANGUAGE RankNTypes #-}

import Prelude hiding (length)

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
