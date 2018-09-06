{-# LANGUAGE RankNTypes #-}

fib' :: Int -> Int
fib' 0 = 1
fib' 1 = 1
fib' n = fib' (n -1) + fib' (n -2)

fib :: Int -> Int
fib n = fibAux n 1 1 

fibAux :: Int -> Int -> Int -> Int
fibAux 0 f s = f
fibAux n f s = fibAux (n-1) s (f+s)  

-- Listas
-- Para las funciones del ejercicio 5 de la práctica 1 (funciones sobre listas), determine:
-- a) Cuáles definió por recursión estructural.
-- b) Cuáles definió por recursión a la cola.
-- c) Cuáles definió por recursión lineal.
-- d) Cuáles siempre terminan (justifique).

-- Arboles
-- Dada la siguiente definición para árboles (que sólo contiene datos en las hojas):
-- Definir y dar el tipo de las siguientes funciones:

data TipTree a = Tip a | Join ( TipTree a ) ( TipTree a )

-- a) heightTip que devuelve la longitud del camino más largo desde la raíz hasta una hoja.
-- b) leaves que calcula el número de hojas.
-- c) nodes que calcula en número de nodos que no son hojas.
-- d) walkover que devuelve la lista de las hojas de un árbol, leídas de izquierda a derecha.
-- e) mirrorTip que calcula la imagen especular del árbol, o sea, el árbol obtenido intercambiando
-- los subárboles izquierdo y derecho de cada nodo.
-- f) mapTip que toma una función y un árbol, y devuelve el árbol que se obtiene del dado al
-- aplicar la función a cada nodo.

-- Polinomios
-- Considere la siguiente representación de polinomios con coeficientes enteros:
-- Con la que por ejemplo el polinomio P(x) = x^2 + 3x + 5 puede representarse de la siguiente manera:
-- Add (Mul Var' Var') (Add (Mul (Cte 3) Var') Cte 5)
-- Escriba las siguientes funciones:

data Poli = Cte Int | Var' | Add Poli Poli | Mul Poli Poli

-- retorna el resultado de evaluar un polinomio P con un valor x dado (P(x)).
-- eval :: Poli -> Int -> Int

-- retorna el polinomio obtenido luego de multiplicar cada constante y variable por un valor entero.
-- mEscalar :: Poli -> Int -> Poli

-- retorna un polinomio equivalente al tomado como parámetro pero donde las operaciones
-- de suma y multiplicación entre constantes ya fueron resueltas, es decir, un polinomio en donde
-- no existen constructores de la forma Add (Cte _) (Cte _) ni Mul (Cte _) (Cte _)
-- sOptimize :: Poli -> Poli

-- Fórmulas lógicas
-- Considere la siguiente representación de expresiones lógicas:

type Variable = Int -- Identificadores enteros para variables

data Logical = Var Variable        | -- una variable con un id dado
               Not Logical         | -- la negacion de una expresion
               And Logical Logical | -- la conjuncion de expresiones
               Or  Logical Logical   -- la disyuncion de expresiones

-- Por ejemplo la expresión (x1 ∨ x2 ) ∧ (¬x3 ∨ x4 ) se puede escribir como:
-- And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 4))
-- Observe que estas expresiones no contienen constantes, para evaluarlas es necesario contar con
-- una valuación:

type Valuation = Variable -> Bool

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

-- retorna el resultado de resolver la expresión lógica con la valuación dada.
-- eval :: Logical -> Valuation -> Bool

-- retorna la lista de todas las variables con ocurrencias en una expresión dada.
-- vars :: Logical -> [Int]

-- simplifica las expresiones eliminando operaciones triviales, más específicamente la doble
-- negación y la conjunción o disyunción de variables iguales.
-- simp :: Logical -> Logical

-- Secuencias
-- Considere la siguiente representación de secuencias:

data Seq a = Nil | Unit a | Cat (Seq a) (Seq a)

-- El constructor Nil representa una secuencia vacía. Unit x representa una secuencia unitaria,
-- cuyo único elemento es x . Finalmente, Cat x y representa una secuencia cuyos elementos son
-- todos los de la secuencia x , seguidos por todos los de la secuencia y .
-- Definir las siguientes operaciones:

-- toma dos secuencias y devuelve su concatenación.
-- appSeq :: Seq a -> Seq a -> Seq a

-- toma un elemento y una secuencia y devuelve la secuencia que tiene al elemento dado
-- como cabeza y a la secuencia dada como cola.
-- conSeq :: a -> Seq a -> Seq a

-- calcula la cantidad de elementos de una secuencia.
-- lenSeq :: Seq a -> Int

-- toma una secuencia e invierte sus elementos.
-- revSeq :: Seq a -> Seq a

-- toma una secuencia y devuelve su primer elemento (es decir el de más a la izquierda).
-- headSeq :: Seq a -> a

-- remueve la cabeza de una secuencia.
-- tailSeq :: Seq a -> Seq a

-- elimina todos los Nil s innecesarios de una secuencia.
-- Por ejemplo, normSeq (Cat (Cat Nil (Unit 1)) Nil) = Unit 1
-- normSeq :: Seq a -> Seq a

-- toma dos secuencias y devuelve True si ambas contienen los mismos valores, en el mismo
-- orden y en la misma cantidad.
-- eqSeq :: Seq a -> Seq a -> Bool

-- toma una secuencia y devuelve una lista con los mismos elementos, en el mismo orden.
-- seq2List :: Seq a -> [a]

-- ¿Qué ventajas y desventajas encuentra sobre (Seq a) respecto a las listas de Haskell ([a])?

-- Árboles Generales
-- Dado el siguiente tipo para árboles generales (árboles con una cantidad arbitraria de hijos en cada nodo):
-- Definir las siguientes funciones:

data GenTree a = GNode a [ GenTree a ]

-- retorna la cantidad de elementos en el árbol.
-- sizeGT :: GenTree a -> Int

-- retorna la altura del árbol.
-- heightGT :: GenTree a -> Int

-- calcula la imagen especular del árbol.
-- mirrorGT :: GenTree a -> GenTree a

-- retorna una lista con los elementos en el árbol.
-- toListGt :: GenTree a -> [a]

-- aplica una función dada a cada elemento en el árbol retornando uno estructuralmente equivalente.
-- mapGT :: (a -> b) -> GenTree a -> GenTree b

-- retorna todos los elementos en un nivel dado del árbol.
-- levelNGT :: GenTree a -> Int -> [a]