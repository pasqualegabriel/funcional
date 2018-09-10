1.2.a
-------------------------------------------------------
-------------------------------------------------------
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)
-------------------------------------------------------

map (+1) [1,2,3] == [2] ++ [3] ++ [4]

map (+1) [1,2,3] 

-> (def map)
1 + 1 (map (+1) [2,3]) 

-> (def map)
1 + 1 : 2 + 1 : map (+1) [3] 

-> (def map)
1 + 1 : 2 + 1 : 3 + 1 : map (+1) [] 

-> (def map)
1 + 1 : 2 + 1 : 3 + 1 : [] 

-> (aritmética)
[2,3,4]

[2] ++ [3] ++ [4] = [2] ++ ([3] ++ [4]) 

-> (def ++)
2 : ([] ++ ([3] ++ [4]))

-> (def ++)
2 : ([3] ++ [4]) 

-> (def ++)
2 : (3 : [] ++ [4]) 

-> (def ++)
2 : (3 : [4]) 

= [2,3,4]

[2,3,4] == [2,3,4] 
-> (por igualdad)
True
(estoy ejecutando muchos pasos, debería hacerlos, pero no importa)

-------------------------------------------------------
1.2.d
-------------------------------------------------------
-------------------------------------------------------

factorial 3 == product [1,2,3]

factorial 3 
-> (def factorial)
3 * factorial 2 
-> (def factorial)
3 * 2 * factorial 1 
-> (def factorial)
3 * 2 * 1 * factorial 0 
-> (def factorial)
3 * 2 * 1 * 1 
-> (def factorial)
6

product [1,2,3] 

-> (def product)
1 * product [2,3]

-> (def product)
1 * 2 * product [3] 

-> (def product)
1 * 2 * 3 * product [] 

-> (def product)
1 * 2 * 3 * 1 

-> (aritmética)
6

-------------------------------------------------------
1.2.e
-------------------------------------------------------
-------------------------------------------------------
(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) (n-1) xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

replicate :: Int -> a -> [a]
replicate n = take n . repeat

take :: Int -> [a] -> [a]
take 0 xs = []
take n [] = []
take n (x:xs) = x : take (n-1) xs

repeat :: a -> [a]
repeat = iterate id
-------------------------------------------------------

(iterate (1:) []) !! 3 == replicate 3 1

(iterate (1:) []) !! 3 

-> (def iterate (por pattern matching de (!!), sólo para que se entienda))
([] : iterate (1:) [1]) !! 3 

-> (def !!)
(iterate (1:) [1]) !! 2 

-> (def iterate)
([1] : iterate (1:) [1,1]) !! 2 

-> (def !!)
(iterate (1:) [1,1]) !! 1 

-> (def iterate)
([1,1] : iterate (1:) [1,1,1]) !! 1 

-> (def !!)
(iterate (1:) [1,1,1]) !! 0 

-> (def iterate)
([1,1,1] : iterate (1:) [1,1,1,1]) !! 0 

-> (def !!)
[1,1,1]


replicate 3 1 

-> (def replicate)
(take 3 . repeat) 1

(esto es importante, no es (take 3 . repeat 1))

(take 3 . repeat) 1 

-> (def .)
take 3 (repeat 1) 

-> (def repeat)
take 3 (iterate id 1) 

-> (def iterate)
take 3 (id 1 : iterate id (id 1)) 

-> (def take)
id 1 : take 2 (iterate id (id 1)) 

-> (def iterate)
id 1 : take 2 (id (id 1) : iterate id (id (id 1))) 

-> (def take)
id 1 : id (id 1) : take 1 (iterate id (id (id 1))) 

-> (def iterate)
id 1 : id (id 1) : take 1 (id (id (id 1)) : iterate id (id (id (id 1)))) 

-> (def take)
id 1 : id (id 1) : id (id (id 1)) : take 0 (iterate id (id (id (id 1)))) 

-> (def take)
id 1 : id (id 1) : id (id (id 1)) : [] 

-> (def id)
1 : id (id 1) : id (id (id 1)) : [] 

-> (def id dos veces)
1 : 1 : id (id (id 1)) : [] 

-> (def id tres veces)
1 : 1 : 1 : []

= [1,1,1]

-------------------------------------------------------
1.2.f
-------------------------------------------------------
-------------------------------------------------------

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) = 
    if p x then x : takeWhile p xs else []

takeWhile (<3) [1,2,3] == map (+1) [0,1]

takeWhile (<3) [1,2,3]

-> (def takeWhile)
if 1 < 3 then 1 : takeWhile (<3) [2,3] else [] 

-> (def if, <)
1 : takeWhile (<3) [2,3] 

-> (def takeWhile)
1 : if 2 < 3 then 2 : takeWhile (<3) [3] else [] 

-> (def if, <)
1 : 2 : takeWhile (<3) [3] -> (def takeWhile)

1 : 2 : if 3 < 3 then 3 : takeWhile (<3) [] else [] 

->
(def if, <)
1 : 2 : []

= [1,2]

map (+1) [0,1] 

->
0 + 1 : map (+1) 1 

->
0 + 1 : 1 + 1 : map (+1) [] 

->
0 + 1 : 1 + 1 : [] 

= [1,2]

-------------------------------------------------------
2.4
-------------------------------------------------------
-------------------------------------------------------

swap . swap = id

Principio de extensionalidad

(swap . swap) (x,y) = id (x,y)

id (x,y) 

= (def id)
(x,y)

(swap . swap) (x,y) 

= (def .)
swap (swap (x,y)) 

= (def swap)
swap (y,x) 

= (def swap)
(x,y)

QED

-------------------------------------------------------
2.5
-------------------------------------------------------
-------------------------------------------------------

twice id = id . id

twice id 

= (def twice)

id . id

QED

-------------------------------------------------------
2.6
-------------------------------------------------------
-------------------------------------------------------

applyN 2 = twice

Principio de extensionalidad

applyN 2 f = twice f

applyN 2 f 

= (def applyN)
f . apply 1 f 

= (def applyN)
f . f . applyN 0 f 

= (def applyN)
f . f . id 

= (id neutro de .)
f . f

twice f = (def twice)
f . f

QED

-------------------------------------------------------
2.7
-------------------------------------------------------
-------------------------------------------------------

twice twice = applyN 4

Principio de extensionalidad

(twice twice) f = applyN 4 f

applyN 4 f 

= (def applyN)
f . applyN 3 f 

= (def applyN)
f . f . applyN 2 f 

= (demostracion 2.6 y def twice)
f . f . (f . f)

= (asociatividad)
f . f . f . f

(twice twice) f = (def twice)
(twice . twice) f = (def .)
twice (twice f) = (def twice)
twice (f . f) = (def twice)
(f . f) . (f . f) = (asociatividad de .)
f . f . f . f

QED

-------------------------------------------------------
2.8
-------------------------------------------------------
-------------------------------------------------------

(\x -> maybe x id Nothing) = head . (:[])

def maybe

(\x -> x) = head . (:[])

def id

id = head . (:[])

Principio de extensionalidad

id x = (head . (:[])) x

(def id)

x = (head . (:[])) x

(def .)

x = head ([x])

(def head)

x = x

QED

-------------------------------------------------------
2.9
-------------------------------------------------------
-------------------------------------------------------

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

curry (uncurry f) = f

Principio de extensionalidad

curry (uncurry f) x y = f x y

curry (uncurry f) x y 

= (def curry)
(uncurry f) (x,y) 

= (def uncurry)
f x y

QED

-------------------------------------------------------
2.10
-------------------------------------------------------
-------------------------------------------------------

uncurry (curry f’) = f’

f' = f (no quiero escribir f')

uncurry (curry f) = f

Principio de extensionalidad

uncurry (curry f) (x,y) = f (x,y)

uncurry (curry f) (x,y) 

= (def uncurry)
(curry f) x y 

= (def curry)
f (x,y)

QED