import Prelude hiding (map, filter, iterate)

mapSucesores :: [Int] -> [Int]
mapSucesores [] = []
mapSucesores (x:xs) =
	(x+1) : mapSucesores xs

mapDoble :: [Int] -> [Int]
mapDoble [] = []
mapDoble (x:xs) =
	(x*2) : mapDoble xs

-- esquema recursivo llamado map
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

longitudes :: [[a]] -> [Int]
longitudes = map length

-- fusion
-- map f . map g = map (f . g)0

-- (map (+1) . map (*2)) [1,2,3]

-- map (\x -> x*2 + 1) [1,2,3]

losMenoresA :: Ord a => a -> [a] -> [a]
losMenoresA n [] = []
losMenoresA n (x:xs) = 
    if x < n
       then x : losMenoresA n xs
       else losMenoresA n xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = 
	if p x
	   then x : filter p xs
	   else filter p xs

-- filter ...
losMayoresA ::  Ord a => a -> [a] -> [a]
losMayoresA n = filter (>n)

-- Vale?
-- filter f . filter g = filter (f . g)

-- NO VALE
-- map g . filter f = filter f . map g

maybe :: b -> (a -> b) -> Maybe a -> b
maybe z f Nothing  = z
maybe z f (Just x) = f x

-- Dar un ejemplo de uso de maybe

-- data Maybe a = Nothing | Just  a
data Either a b = Left b  | Right a

-- Más descriptivo que sólo devolver Nothing
-- minimo [] = Left "no hay minimo"

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- Desafio usar applyN para definir
-- iterate