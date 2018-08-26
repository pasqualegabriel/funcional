-- PARCIAL

data Dir = Left' | Right' | Straight deriving (Show, Eq)

data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a) deriving (Show)

-- 1) Definir las sig. funciones usando recursión explicita sobre la est. de mapa (se pueden ver ejemplos en el anexo):

objects :: Mapa a -> [a]
objects (Cofre xs) = xs
objects (Nada m) = objects m
objects (Bifurcacion xs m1 m2) = xs ++ (objects m1) ++ (objects m2)

mapM' :: (a -> b) -> Mapa a -> Mapa b
mapM' f (Cofre xs) = Cofre (map f xs)
mapM' f (Nada m) = Nada (mapM' f m)
mapM' f (Bifurcacion xs m1 m2) = Bifurcacion (map f xs) (mapM' f m1) (mapM' f m2)

hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f (Cofre xs) [] = any f xs   
hasObjectAt f (Nada m) (Straight:ds) = hasObjectAt f m ds
hasObjectAt f (Bifurcacion xs m1 m2) (Left':ds) = hasObjectAt f m1 ds
hasObjectAt f (Bifurcacion xs m1 m2) (Right':ds) = hasObjectAt f m2 ds

longestPath :: Mapa a -> [Dir]
longestPath (Cofre xs) = []
longestPath (Nada m) = Straight : longestPath m
longestPath (Bifurcacion xs m1 m2) = 
    let r1 = longestPath m1
        r2 = longestPath m2
     in if length r1 >= length r2 
           then Left'  : r1 
           else Right' : r2

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath (Cofre xs) = xs
objectsOfLongestPath (Nada m) = objectsOfLongestPath m 
objectsOfLongestPath (Bifurcacion xs m1 m2) = 
  if length (longestPath m1) >= length (longestPath m2)
     then xs ++ objectsOfLongestPath m1
    else xs ++ objectsOfLongestPath m2

allPaths :: Mapa a -> [[Dir]]
allPaths (Cofre xs) = [[]]
allPaths (Nada m) = map (Straight:) (allPaths m)
allPaths (Bifurcacion xs m1 m2) = (map (Left':) (allPaths m1)) ++ 
                                  (map (Right':) (allPaths m2))

-- 2) Dar tipo y definir foldM y recMm una versión de fold y recursión primitiva, respectivamente para la estrcutura de Mapa.

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM f g h (Cofre xs) = f xs
foldM f g h (Nada m) = g (foldM f g h)
foldM f g h (Bifurcacion xs m1 m2) = h xs (foldM f g h m1) (foldM f g h m2)
 

recM :: ([a] -> b) -> (b -> Mapa a -> b) -> ([a] -> Mapa a -> Mapa a -> b  -> b -> b) -> Mapa a -> b
recM f g h (Cofre xs) = f xs
recM f g h (Nada m) = g m (recM f g h)
recM f g h (Bifurcacion xs m1 m2) = h xs m1 m2 (recM f g h m1) (recM f g h m2)

-- 3) Definir las funciones del primer punto usando foldM y recM, según corresponda.

objects' :: Mapa a -> [a]
objects' = foldM id id (\ xs r1 r2 -> xs ++ r1 ++ r2)

mapM'' :: (a -> b) -> Mapa a -> Mapa b
mapM'' f = foldM (\ xs -> Cofre (map f xs)) (\ r -> Nada r) (\ xs r1 r2 -> Bifurcacion (map f xs) r1 r2)

hasObjectAt' :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt' f m ds = foldM fc fn fb m ds
 where
    fc xs [] = any f xs
    fn r (Straight:ds) = r ds
    fb r1 r2 (Left':ds) = r1 ds
    fb r1 r2 (Right':ds) = r2 ds

longestPath' :: Mapa a -> [Dir]
longestPath' = foldM fc fn fb
  where
    fc xs = []
    fn r  = Straight : r
    fb xs r1 r2 = (Left : r1) ++ (Right : r2)
                                                                         
objectsOfLongestPath' :: Mapa a -> [a]
objectsOfLongestPath' = recrM fc fn fb
  where
    fc xs = xs
    fn m r = r
    fb xs m1 m2 r1 r2 = 
        if length (longestPath m1) > length (longestPath m2) 
           then xs ++ r1
           else xs ++ r2

allPaths' :: Mapa a -> [[Dir]]
allPaths' = foldM fc fn fb
  where
    fc xs = [[]]
    fn r  = map (Straight:) r
    fb xs r1 r2 = map (Left:) r1 ++ map (Right:) r2

-----------------------------------------------------------------------------------------------------------------------------------

-- DEMOSTRACION 1

a) length . objects = countObjects

Principio de extensionalidad y def (.)

length (objects m) = countObjects m

Demostración por inducción en la estructura de m

Caso base) m = Cofre xs

length (objects (Cofre xs))
= (def objects)
length xs
= (def countObjects)
countObjects (Cofre xs)

Caso inductivo 1) m = Nada hm

HI) length (objects hm) = countObjects hm
TI) ¿length (objects (Nada hm)) = countObjects (Nada hm)?

length (objects (Nada hm))
= (def objects)
length (objects hm)
= HI
countObjects hm
= (def countObjects)
countObjects (Nada hm)

Caso inductivo 2) m = Bifurcacion xs hm1 hm2

HI 1) length (objects hm1) = countObjects hm1
HI 2) length (objects hm2) = countObjects hm2
TI) ¿length (objects (Bifurcacion xs hm1 hm2)) = countObjects (Bifurcacion xs hm1 hm2)?

length (objects (Bifurcacion xs hm1 hm2))
= (def objects)
length (xs ++ objects hm1 ++ objects hm2)
= LEMA visto en clase
length xs + length (objects hm1) + length (objects hm2)
= HI 1 y 2
length xs + countObjects hm1 + countObjects hm2
= (def countObjects)
countObjects (Bifurcacion xs hm1 hm2)

-- DEMOSTRACION 2

b) elem x . objects = hasObject (==x)

Principio de extensionalidad y definicion de (.)

elem x (objects m) = hasObject (==x) m

Demostración por inducción en la estructura de m

Caso base) m = Cofre xs

elem x (objects (Cofre xs))
= (def objects)
elem x xs
= LEMA
hasObject (==x) xs
= (def hasObject)
hasObject (==x) (Cofre xs)

Caso inductivo 1) m = Nada hm

HI) elem x (objects hm) = hasObject (==x) hm
TI) ¿elem x (objects (Nada hm)) = hasObject (==x) (Nada hm)?

elem x (objects (Nada hm))
= (def objects)
elem x (objects hm)
= HI
hasObject (==x) hm
= (def hasObject)
hasObject (==x) (Nada hm)

Caso inductivo 2) m = Bifurcacion xs hm1 hm2

HI 1) elem x (objects hm1) = hasObject (==x) hm1
HI 2) elem x (objects hm2) = hasObject (==x) hm2
TI) ¿elem x (objects (Bifurcacion xs hm1 hm2)) = hasObject (==x) (Bifurcacion xs hm1 hm2)?

-- DEMOSTRACION 3

b) length . map f . map g . objects = countObjects . mapM (f . g)

length . map f . map g . objects
= Demostrado Practica 2  map f . map g = map (f . g)
length . map (f . g) . objects
= Demostrado en Practica 2  length . map f = length
length . objects
= Demostrado en el examen  length . objects = countObjects
countObjects

Me queda

countObjects = countObjects . mapM (f . g)

Defino h = f . g

Entonces en realidad puedo demostrar

countObjects = countObjects . mapM h

Principio de extensionalidad y def de (.)

countObjects m = countObjects (mapM h m)

Demostración en la estructura de m

Caso base) m = Cofre xs

¿ countObjects (Cofre xs) = countObjects (mapM h (Cofre xs)) ?

countObjects (Cofre xs)
= (def countObjects)
length xs

countObjects (mapM h (Cofre xs))
= (def mapM)
countObjects (Cofre (map h xs))
= (def countObjects)
length (map h xs)
= LEMA
length xs

Caso inductivo 1) m = Nada hm

HI) countObjects hm = countObjects (mapM h hm)
TI) ¿countObjects (Nada hm) = countObjects (mapM h (Nada hm))?

countObjects (mapM h (Nada hm))
= (def mapM)
countObjects (Nada (mapM h hm))
= (def countObjects)
countObjects (mapM h hm)
= HI
countObjects hm
= (def countObjects)
countObjects (Nada hm)

Caso inductivo 2) m = Bifurcacion xs hm1 hm2

HI 1) countObjects hm1 = countObjects (mapM h hm1)
HI 2) countObjects hm2 = countObjects (mapM h hm2)
TI) ¿countObjects (Bifurcacion xs hm1 hm2) = countObjects (mapM h (Bifurcacion xs hm1 hm2))?

countObjects (mapM h (Bifurcacion xs hm1 hm2))
= (def mapM)
countObjects (Bifurcacion (map h xs) (mapM h hm1) (mapM h hm2))
= (def countObjects)
length (map h xs) + countObjects (mapM h hm1) + countObjects (mapM h hm2)
= LEMA y HI 1 y 2
length xs + countObjects hm1 + countObjects hm2
= (def countObjects)
countObjects (Bifurcacion xs hm1 hm2)