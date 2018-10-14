{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFoldable #-}

fib' :: Int -> Int
fib' 0 = 1
fib' 1 = 1
fib' n = fib' (n -1) + fib' (n -2)

fib :: Int -> Int
fib n = fibAux n 1 1 

fibAux :: Int -> Int -> Int -> Int
fibAux 0 f s = f
fibAux n f s = fibAux (n-1) s (f+s)  

-- Arboles
-- Dada la siguiente definición para árboles (que sólo contiene datos en las hojas):
-- Definir y dar el tipo de las siguientes funciones:

data TipTree a = Tip a | Join ( TipTree a ) ( TipTree a )

-- a) heightTip que devuelve la longitud del camino más largo desde la raíz hasta una hoja.
heightTip :: TipTree a -> Int
heightTip  (Tip _)   = 1
heightTip (Join x y) = max (1 + heightTip x) (1 + heightTip y)
--heightTip (Join (Join (Join (Tip 3) (Tip 3)) (Tip 1)) (Join (Tip 5) (Tip 6)))

-- b) leaves que calcula el número de hojas.
leavesTip :: TipTree a -> Int
leavesTip  (Tip _)   = 1
leavesTip (Join x y) = leavesTip x + leavesTip y
--leavesTip (Join (Join (Join (Tip 3) (Tip 3)) (Tip 1)) (Join (Tip 5) (Tip 6)))

-- c) nodes que calcula en número de nodos que no son hojas.
nodesTip :: TipTree a -> Int
nodesTip  (Tip _)   = 0
nodesTip (Join x y) = 1 + (nodesTip x) + (nodesTip y)
--nodesTip (Join (Join (Join (Tip 3) (Tip 3)) (Tip 1)) (Join (Tip 5) (Tip 6)))

-- d) walkover que devuelve la lista de las hojas de un árbol, leídas de izquierda a derecha.
walkoverTip :: TipTree a -> [a]
walkoverTip  (Tip x)   = [x] 
walkoverTip (Join x y) = walkoverTip x ++ walkoverTip y
--walkoverTip (Join (Join (Join (Tip 1) (Tip 2)) (Tip 3)) (Join (Tip 4) (Tip 5)))

-- e) mirrorTip que calcula la imagen especular del árbol, o sea, el árbol obtenido intercambiando
-- los subárboles izquierdo y derecho de cada nodo.
mirrorTip :: TipTree a -> TipTree a
mirrorTip (Join x y) = Join (mirrorTip y) (mirrorTip x)
mirrorTip t = t
--mirrorTip (Join (Join (Join (Tip 1) (Tip 2)) (Tip 3)) (Join (Tip 4) (Tip 5)))

-- f) mapTip que toma una función y un árbol, y devuelve el árbol que se obtiene del dado al
-- aplicar la función a cada nodo.
mapTip :: (a -> b) -> TipTree a -> TipTree b
mapTip f  (Tip x)   = Tip (f x)
mapTip f (Join x y) = Join (mapTip f x) (mapTip f y)
--mapTip (10+) (Join (Join (Join (Tip 1) (Tip 2)) (Tip 3)) (Join (Tip 4) (Tip 5)))

-- Polinomios
-- Considere la siguiente representación de polinomios con coeficientes enteros:
-- Con la que por ejemplo el polinomio P(x) = x^2 + 3x + 5 puede representarse de la siguiente manera:
-- Add (Mul VarP VarP) (Add (Mul (Cte 3) VarP) (Cte 5))
-- Escriba las siguientes funciones:

data Poli = Cte Int | VarP | Add Poli Poli | Mul Poli Poli

foldPoli :: (Int -> a) -> a -> (a -> a -> a) -> (a -> a -> a) -> Poli -> a
foldPoli f z g h (Cte x) = f x
foldPoli f z g h VarP = z
foldPoli f z g h (Add l r) = g (foldPoli f z g h l) (foldPoli f z g h r)
foldPoli f z g h (Mul l r) = h (foldPoli f z g h l) (foldPoli f z g h r)

-- retorna el resultado de evaluar un polinomio P con un valor x dado (P(x)).
evalP :: Poli -> Int -> Int
evalP (Cte x)   n = x
evalP  VarP     n = n
evalP (Add x y) n = evalP x n + evalP y n
evalP (Mul x y) n = evalP x n * evalP y n
--evalP (Add (Mul VarP VarP) (Add (Mul (Cte 3) VarP) (Cte 5))) 2

-- retorna el polinomio obtenido luego de multiplicar cada constante y variable por un valor entero.
mEscalar :: Poli -> Int -> Poli
mEscalar (Cte x)   n = Mul (Cte x) (Cte n)
mEscalar  VarP     n = Mul VarP (Cte n)
mEscalar (Add x y) n = Add (mEscalar x n) (mEscalar y n)
mEscalar (Mul x y) n = Mul (mEscalar x n) (mEscalar y n)
--mEscalar (Add (Mul VarP VarP) (Add (Mul (Cte 3) VarP) (Cte 5))) 2

-- retorna un polinomio equivalente al tomado como parámetro pero donde las operaciones
-- de suma y multiplicación entre constantes ya fueron resueltas, es decir, un polinomio en donde
-- no existen constructores de la forma Add (Cte _) (Cte _) ni Mul (Cte _) (Cte _)
sOptimize :: Poli -> Poli
sOptimize (Add x y) = h (sOptimize x) (sOptimize y)
sOptimize (Mul x y) = i (sOptimize x) (sOptimize y)
sOptimize x = x

h (Cte x) (Cte y) = Cte (x + y)
h x y = Add x y

i (Cte x) (Cte y) = Cte (x * y)
i x y = Mul x y
--sOptimize (Add (Mul (Cte 3) (Cte 3)) (Add (Cte 5) (Mul (Cte 2) (Cte 3))))
--sOptimize (Add (Mul (Cte 3) (Cte 3)) (Add (Cte 5) (Mul VarP (Cte 3))))

sOptimize2 :: Poli -> Poli
sOptimize2 = foldPoli Cte VarP h i
    where
        h (Cte x) (Cte y) = Cte (x + y)
        h x y = Add x y

        i (Cte x) (Cte y) = Cte (x * y)
        i x y = Mul x y

canOptimize :: Poli -> Bool
canOptimize (Add (Cte x) (Cte y)) = True
canOptimize (Mul (Cte x) (Cte y)) = True
canOptimize (Add x y) = canOptimize x || canOptimize y
canOptimize (Mul x y) = canOptimize x || canOptimize y
canOptimize x = False

-- Fórmulas lógicas
-- Considere la siguiente representación de expresiones lógicas:

-- Por ejemplo la expresión (x1 ∨ x2) ∧ (¬x3 ∨ x4) se puede escribir como:
-- And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 4))
-- Observe que estas expresiones no contienen constantes, para evaluarlas es necesario contar con
-- una valuación:

-- Una valuación es una función de Variable en Bool. Es decir, una valuación asigna a cada
-- variable de una expresión el valor verdadero o falso.
-- Por ejemplo sea v una función tal que:
-- v 1 = True
-- v 2 = True
-- v x = False -- para todo x distinto de 1 y 2
-- Al evaluar la expresión anterior con esta valuación se obtiene verdadero pues,
-- (x1 ∨ x2) ∧ (¬x3 ∨ x4 ) → v
-- (True ∨ True) ∧ (¬ False ∨ False) → ¬
-- (True ∨ True) ∧ (True ∨ False) → ∨
-- True ∧ True → ∧ True
-- Escriba las siguientes funciones:

type Variable = Int -- Identificadores enteros para variables

type Valuation = Variable -> Bool

data Logical = Var Variable        | -- una variable con un id dado
               Not Logical         | -- la negacion de una expresion
               And Logical Logical | -- la conjuncion de expresiones
               Or  Logical Logical   -- la disyuncion de expresiones

-- retorna el resultado de resolver la expresión lógica con la valuación dada.
eval :: Logical -> Valuation -> Bool
eval (Var v)   f = f v
eval (Not l)   f = not (eval l f)
eval (And x y) f = eval x f && eval y f
eval (Or  x y) f = eval x f || eval y f
--eval (And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 4))) (==1)

-- retorna la lista de todas las variables con ocurrencias en una expresión dada.
vars :: Logical -> [Int]
vars (Var v)   = [v]
vars (Not l)   = vars l
vars (And x y) = vars x ++ vars y 
vars (Or  x y) = vars x ++ vars y
--vars (And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 4)))

-- simplifica las expresiones eliminando operaciones triviales, más específicamente la doble
-- negación y la conjunción o disyunción de variables iguales.
simp :: Logical -> Logical
simp l@(Var v)     = l 
simp (Not (Not l)) = simp l
simp (Not l) = Not (simp l)
simp l@(And x y) = if areEqual x y then simp x else
    if canSimp x || canSimp y then simp (And (simp x) (simp y)) else l
simp l@(Or  x y) = if areEqual x y then simp x else
    if canSimp x || canSimp y then simp (Or  (simp x) (simp y)) else l
--simp (And (Or (Var 1) (Var 1)) (Or (Not (Not (Not (Not (Var 1))))) (Var 1)))
--simp (And (Or (Var 1) (Var 1)) (Or (Not (Not (Not (Var 1)))) (Var 1)))

areEqual :: Logical -> Logical -> Bool
areEqual (Var x) (Var y) = x == y
areEqual (Not x) (Not y) = areEqual x y
areEqual (And x y) (And x' y') = areEqual x x' && areEqual y y'
areEqual (Or x y)  (Or  x' y') = areEqual x x' && areEqual y y'
areEqual _ _ = False

canSimp :: Logical -> Bool
canSimp (And (Var x) (Var y)) = x == y 
canSimp (Or  (Var x) (Var y)) = x == y 
canSimp (And x y) = canSimp x || canSimp y  
canSimp (Or  x y) = canSimp x || canSimp y 
canSimp (Not (Not _)) = True
canSimp _ = False

-- Secuencias
-- Considere la siguiente representación de secuencias:

data Seq a = Nil | Unit a | Cat (Seq a) (Seq a)

-- El constructor Nil representa una secuencia vacía. Unit x representa una secuencia unitaria,
-- cuyo único elemento es x . Finalmente, Cat x y representa una secuencia cuyos elementos son
-- todos los de la secuencia x , seguidos por todos los de la secuencia y .
-- Definir las siguientes operaciones:

-- toma dos secuencias y devuelve su concatenación.
appSeq :: Seq a -> Seq a -> Seq a
appSeq Nil x = x
appSeq x Nil = x
appSeq x  y  = Cat x y

-- toma un elemento y una secuencia y devuelve la secuencia que tiene al elemento dado
-- como cabeza y a la secuencia dada como cola.
conSeq :: a -> Seq a -> Seq a
conSeq x Nil = Unit x
conSeq x  y  = Cat (Unit x) y

-- calcula la cantidad de elementos de una secuencia.
lenSeq :: Seq a -> Int
lenSeq   Nil     = 0
lenSeq (Unit _)  = 1
lenSeq (Cat x y) = lenSeq x + lenSeq y
--lenSeq (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil))))

-- toma una secuencia e invierte sus elementos.
revSeq :: Seq a -> Seq a
revSeq (Cat x y) = Cat (revSeq y) (revSeq x) 
revSeq    s      = s
--revSeq (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil))))

-- toma una secuencia y devuelve su primer elemento (es decir el de más a la izquierda).
-- precondicion: hay al menos un elemento
headSeq :: Seq a -> a
headSeq (Unit x)    = x
headSeq (Cat Nil y) = headSeq y
headSeq (Cat x _)   = headSeq x
-- headSeq (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil))))
-- headSeq (revSeq (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil)))))

-- remueve la cabeza de una secuencia.
tailSeq :: Seq a -> Seq a
tailSeq   Nil            = Nil
tailSeq (Unit _)         = Nil
tailSeq (Cat (Unit _) y) = y
tailSeq (Cat Nil y)      = tailSeq y
tailSeq (Cat x y)        = Cat (tailSeq x) y
-- tailSeq (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil))))
-- tailSeq (revSeq (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil)))))

-- elimina todos los Nil s innecesarios de una secuencia.
-- Por ejemplo, normSeq (Cat (Cat Nil (Unit 1)) Nil) = Unit 1
normSeq :: Seq a -> Seq a
normSeq (Cat Nil y) = normSeq y
normSeq (Cat x Nil) = normSeq x
normSeq (Cat x y)   = Cat (normSeq x) (normSeq y)
normSeq    x        = x
-- normSeq (Cat (Cat Nil (Unit 1)) Nil)
-- normSeq (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil))))

-- toma dos secuencias y devuelve True si ambas contienen los mismos valores, en el mismo
-- orden y en la misma cantidad.
eqSeq :: Eq a => Seq a -> Seq a -> Bool
eqSeq   Nil         Nil       = True 
eqSeq (Unit x)    (Unit y)    = x == y
eqSeq (Cat x1 y1) (Cat x2 y2) = eqSeq x1 x2 && eqSeq y1 y2
eqSeq _ _ = False
-- eqSeq (Cat (Cat Nil (Unit 1)) Nil) (Cat (Cat Nil (Unit 1)) Nil)

-- toma una secuencia y devuelve una lista con los mismos elementos, en el mismo orden.
seq2List :: Seq a -> [a]
seq2List    Nil    = []
seq2List (Unit x)  = [x]
seq2List (Cat x y) = seq2List x ++ seq2List y
-- seq2List (Cat (Cat (Unit 1) Nil) (Cat (Cat Nil (Unit 2)) (Cat (Unit 3) (Cat (Unit 4) Nil))))

-- ¿Qué ventajas y desventajas encuentra sobre (Seq a) respecto a las listas de Haskell ([a])?

-- Árboles Generales
-- Dado el siguiente tipo para árboles generales (árboles con una cantidad arbitraria de hijos en cada nodo):
-- Definir las siguientes funciones:

data GenTree a = GNode a [GenTree a] deriving (Show)

foldGT :: (a -> [b] -> b) -> GenTree a -> b
foldGT f (GNode x gs) = f x (map (foldGT f) gs)

-- retorna la cantidad de elementos en el árbol.
sizeGT :: GenTree a -> Int
sizeGT = foldGT (\x gs -> 1 + sum gs) 
--sizeGT (GNode e xs) = 1 + sizeGTAux xs
--sizeGT (GNode 1 [(GNode 2 [(GNode 3 [])]), (GNode 4 [(GNode 5 []), (GNode 6 [(GNode 7 []), (GNode 8 [])])]), (GNode 9 [])])

sizeGTAux :: [GenTree a] -> Int
sizeGTAux   []   = 0
sizeGTAux (x:xs) = sizeGT x + sizeGTAux xs

-- retorna la altura del árbol.
heightGT :: GenTree a -> Int
heightGT = foldGT h
    where
        h x [] = 0
        h x gs = 1 + maximum gs
--heightGT (GNode e xs) = 1 + heightGTxs xs
--heightGT (GNode 1 [(GNode 2 [(GNode 3 [])]), (GNode 2 [(GNode 3 []), (GNode 3 [(GNode 4 [(GNode 5 [])]), (GNode 4 [])])]), (GNode 2 [])])

heightGTxs :: [GenTree a] -> Int
heightGTxs   []   = 0
heightGTxs (x:xs) = max (heightGT x) (heightGTxs xs)

-- calcula la imagen especular del árbol.
mirrorGT :: GenTree a -> GenTree a
mirrorGT = foldGT h
    where
        h x gs = GNode x (reverse gs)
--mirrorGT (GNode e xs) = GNode e (mirrorGTxs xs)
--mirrorGT (GNode 1 [(GNode 2 [(GNode 3 [])]), (GNode 4 [(GNode 5 []), (GNode 6 [(GNode 7 [(GNode 8 [])]), (GNode 9 [])])]), (GNode 10 [])])

mirrorGTxs :: [GenTree a] -> [GenTree a]
mirrorGTxs   []   = []
mirrorGTxs (x:xs) = (mirrorGTxs xs) ++ [mirrorGT x]

-- retorna una lista con los elementos en el árbol.
toListGT :: GenTree a -> [a]
toListGT = foldGT (\x gs -> x : concat gs) 
--toListGT (GNode e xs) = e : foldr ((++) . toListGT) [] xs
--toListGT (GNode e xs) = e : listGT xs
--toListGT (GNode 1 [(GNode 2 [(GNode 3 [])]), (GNode 4 [(GNode 5 []), (GNode 6 [(GNode 7 [(GNode 8 [])]), (GNode 9 [])])]), (GNode 10 [])])

listGT :: [GenTree a] -> [a]
listGT   []   = []
listGT (x:xs) = toListGT x ++ listGT xs

-- aplica una función dada a cada elemento en el árbol retornando uno estructuralmente equivalente.
mapGT :: (a -> b) -> GenTree a -> GenTree b
mapGT f = foldGT (GNode . f)
-- mapGT f (GNode e xs) = GNode (f e) (mapGTAux f xs)
--mapGT (10+) (GNode 1 [(GNode 2 [(GNode 3 [])]), (GNode 4 [(GNode 5 []), (GNode 6 [(GNode 7 [(GNode 8 [])]), (GNode 9 [])])]), (GNode 10 [])])

mapGTAux :: (a -> b) -> [GenTree a] -> [GenTree b]
mapGTAux f   []   = []
mapGTAux f (x:xs) = mapGT f x : mapGTAux f xs

-- retorna todos los elementos en un nivel dado del árbol.
-- precondicion: Existe al menos un GenTree a en el nivel
levelNGT :: Int -> GenTree a -> [a]
levelNGT 0 (GNode e _ ) = [e]
levelNGT n (GNode e xs) = levelNGTAux n xs
--levelNGT 3 (GNode 1 [(GNode 2 [(GNode 3 [])]), (GNode 2 [(GNode 3 []), (GNode 3 [(GNode 4 [(GNode 5 [])]), (GNode 4 [])])]), (GNode 2 [])])

levelNGTAux :: Int -> [GenTree a] -> [a]
levelNGTAux n   []   = []
levelNGTAux n (x:xs) = levelNGT (n-1) x ++ levelNGTAux n xs

-- FileSystem

data FileSystem = File String String | Folder String [FileSystem]

cantFiles :: FileSystem -> Int
cantFiles (File   _ _)  = 1
cantFiles (Folder _ xs) = cantFilesAux xs
--cantFiles (Folder "Folder1" [(File "File11" "11"), (Folder "Folder11" [(Folder "Folder111" [(File "File1111" "1111")]), (Folder "Folder112" [(File "File1112" "1112")])]), (Folder "Folder12" [(File "File121" "121")])])

cantFilesAux :: [FileSystem] -> Int
cantFilesAux   []   = 0
cantFilesAux (x:xs) = cantFiles x + cantFilesAux xs

findFile :: String -> FileSystem -> Maybe String
findFile n (File   x y)  = if n == x then Just y else Nothing
findFile n (Folder x xs) = findFileAux n xs
--findFile "f2" (Folder "F1" [(Folder "F2" [(File "f1" "1")]), (File "f2" "2"), (Folder "F3" [(File "f3" "3"), (Folder "F4" [])])])

findFileAux :: String -> [FileSystem] -> Maybe String
findFileAux n   []   = Nothing
findFileAux n (x:xs) = isTheFile (findFile n x) (findFileAux n xs)

isTheFile :: Maybe String -> Maybe String -> Maybe String
isTheFile j@(Just x) _ = j
isTheFile _ j@(Just x) = j
isTheFile   _    _     = Nothing

-- Precondición: el archivo existe
route :: String -> FileSystem -> [Int]
route n (File   x y)  = []
route n (Folder x xs) = routeAux 0 n xs
--route "f5" (Folder "F1" [(Folder "F2" [(File "f1" "1")]), (File "f2" "2"), (Folder "F3" [(File "f3" "3"), (Folder "F4" [(File "f4" "4"), (File "f5" "5")])])])

routeAux :: Int -> String -> [FileSystem] -> [Int]
routeAux i n  [x]   = i : route n x
routeAux i n (x:xs) = if isNothing (findFile n x) then routeAux (i + 1) n xs else i : route n x

isNothing Nothing = True
isNothing    _    = False

-- Precondición: el archivo existe
routeTo :: String -> FileSystem -> [String]
routeTo n (File   x y)  = []
routeTo n (Folder x xs) = routeToAux x n xs
--routeTo "f5" (Folder "F1" [(Folder "F2" [(File "f1" "1")]), (File "f2" "2"), (Folder "F3" [(File "f3" "3"), (Folder "F4" [(File "f4" "4"), (File "f5" "5")])])])

routeToAux :: String -> String -> [FileSystem] -> [String]
routeToAux y n  [x]   = y : routeTo n x
routeToAux y n (x:xs) = if isNothing (findFile n x) then routeToAux y n xs else y : routeTo n x

mapContents :: [(String, String -> String)] -> FileSystem -> FileSystem
mapContents   []   y = y
mapContents (x:xs) y = applyContent x (mapContents xs y)
--mapContents [("f2", ("f2"++)), ("f3", ("f3"++)), ("f5", ("f5"++))] (Folder "F1" [(Folder "F2" [(File "f1" "1")]), (File "f2" "2"), (Folder "F3" [(File "f3" "3"), (Folder "F4" [(File "f4" "4"), (File "f5" "5")])])])

applyContent :: (String, String -> String) -> FileSystem -> FileSystem
applyContent (n, f) a@(File   x y)  = if n == x then File x (f y) else a
applyContent (n, f)   (Folder x xs) = Folder x (applyContentAux n f xs)

applyContentAux :: String -> (String -> String) -> [FileSystem] -> [FileSystem]
applyContentAux n f  [x]   = [applyContent (n, f) x]
applyContentAux n f (x:xs) = if isNothing (findFile n x)
                             then x : applyContentAux n f xs
                             else (applyContent (n, f) x) : xs

