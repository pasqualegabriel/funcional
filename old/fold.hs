
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sum [] = 0
sum (x:xs) = x + sum xs

sum' = foldr (\x r -> x + r) 0

agrupar [] = []
agrupar (x:xs) = 
    case agrupar xs of
        [] -> [[x]]
        (ys:yss) -> 
            let (ys:yss) = agrupar xs
            in if x == head ys
                 then (x:ys) : yss
                 else [x] : ys : yss

agrupar' = foldr g []
   where g x [] = [[x]]
         g x (ys:yss) = 
            let (ys:yss) = r
            in if x == head ys
                 then (x:ys) : yss
                 else [x] : ys : yss

last (x:[]) = x
last (x:xs) = last xs

last' = recr g (error "...")
   where g x [] r = x
         g x xs r = r

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)

last'' = foldr1 (\x r -> r)

init [x] = []
init (x:xs) = x : init xs

init = recr g (error "...")
   where g x [] r = []
         g x xs r = x : r

snoc [] y = [y]
snoc (x:xs) y = x : snoc xs y

snoc' xs y = foldr (:) [y] xs

subset [] ys = True
subset (x:xs) ys = elem x ys && subset xs ys

subset' xs ys = foldr (\x r -> elem x ys && r) True xs

(!!) :: [a] -> Int -> a
(!!) [] n = error "out of index"
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n-1)

(!!!) xs n = foldr g (\_ -> error "out of index") xs n
  where g x r 0 = x
        g x r n = r (n-1)
-- r :: Int -> a

zip :: [a] -> [b] -> [(a,b)]
zip [] _  = []
zip (x:xs)  [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zip' xs ys = foldr g (\_ -> []) xs ys
    where g x r [] = []
          g x r (y:ys) = (x,y) : r ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (x:xs) = drop (n-1) xs

drop' n xs = recr g (\_ -> []) xs n
   where g x xs r 0 = xs
         g x xs r n = r (n-1)

splitAt 0 xs = ([], xs)
splitAt n [] = ([], [])
splitAt n (x:xs) = 
   let (ys, zs) = splitAt (n-1) xs
       in (x:ys, zs)

splitAt' n xs = recr g (\_ -> ([], [])) xs n
   where g x xs r 0 = ([], xs)
         g x xs r n = 
               let (ys, zs) = r (n-1)
                   in (x:ys, zs)

-- Posibles con unfoldr ---------------

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : replicate (n-1) x

repeat x = x : repeat x

cycle xs = xs ++ cycle xs

----------------------------------------

data NonEmptyList a = 
     Unit a 
   | NECons a (NonEmptyList a)

foldNE :: (a -> b) -> (a -> b -> b) -> NonEmptyList a -> b
foldNE f g (Unit x) = f x
foldNE f g (NEConst x xs) =
   g x (foldNE f g xs)

sumNE :: NonEmptyList Int -> Int
sumNE = foldNE id (+)

lenNE :: NonEmptyList a -> Int
lenNE = foldNE (const 1) (+)

data AppendList a = 
     Nil 
   | Unit a 
   | Append (AppendList a) (AppendList a)

foldAPP ::
    b ->
    (a -> b) ->
    (b -> b -> b) ->
    AppendList a -> b

foldAPP z f g Nil = z
foldAPP z f g (Unit x) = f x
foldAPP z f g (Append xs ys) = 
   g (foldAPP z f g xs) (foldPP z f g xs)

sumE :: AppendList Int -> Int
sumE = foldAPP 0 id (+)

data IntList = IntNil | IntCons Int IntList

foldInt :: 
   b -> (Int -> b -> b) -> IntList -> b
foldInt z f IntNil = z
foldInt z f (IntCons n xs) =
    f n (foldInt z f xs)

data Exp = 
     Const Int
   | Var String
   | Sum Exp Exp
   | Prod Exp Exp

foldE ::
foldE fc fv fs fp (Const n) = fc n
foldE fc fv fs fp (Var v) = fv v
foldE fc fv fs fp (Sum e1 e2) = 
  fs (foldE fc fv fs fp e1)
     (foldE fc fv fs fp e2)
foldE fc fv fs fp (Prod e1 e2) =
  fp (foldE fc fv fs fp e1)
     (foldE fc fv fs fp e2)

variables :: Exp -> [String]
variables = foldE (const []) (:[]) (++) (++)

getVar :: Map String Int -> String -> Int
getVar m s = 
   case lookup s m of
        Nothing -> 0
        (Just n) -> n

--  getVar m s = maybe 0 id (lookup s m)

data Exp = 
     Const Int
   | Var String
   | Sum Exp Exp
   | Prod Exp Exp

eval :: Exp -> Map String Int -> Int
eval (Const n)   m  = n
eval (Var v)     m  = getVar v m
eval (Sum e1 e2) m  = eval e1 m + eval e2 m
eval (Prod e1 e2) m = eval e1 m * eval e2 m

eval e m = foldE id (getVar m) (+) (*) e

data Maybe a = Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe z f Nothing = z
maybe z f (Just x) = f x

data Nat = Zero | Succ Nat

foldNat :: b -> (b -> b) -> Nat -> b
foldNat z f Zero = z
foldNat z f (Succ n) = f (foldNat z f n)

data T a = A a | B (T a) | C (T a) (T a)

-- Lo hicimos mentalmente, tarea: hacerlo

data LTree a = L [a] | B a (LTree a) (LTree a)

foldLT :: ([a] -> b) -> (a -> b -> b -> b) -> LTree a -> b
foldLT fl fb (L xs) = fl xs
foldLT fl fb (B x t1 t2) =
  fb x (foldLT fl fb t1)
       (foldLT fl fb t2)

sumLT :: LTree Int -> Int
sumLT = foldLT sum (\x r1 r2 -> x + r1 + r2)

levelN :: Int -> LTree a -> [a]
levelN 0 (L xs) = xs
levelN 0 (B x t1 t2) = [x]
levelN n (L xs) = []
levelN n (B x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

levelNF n t = foldLT g h t n
   where casoL xs 0 = xs
         casoL xs n = []
         casoB x r1 r2 0 = [x]
         casoB x r1 r2 n = r1 (n-1) ++ r2 (n-1)

heightT :: LTree a -> Int
heightT = foldLT (const 1) (\x r1 r2 -> 1 + max r1 r2)

mirrorT :: LTree a -> LTree a
mirrorT = foldLT (\xs -> L xs) (\x r1 r2 -> B x r2 r1)

-- También Alan planteó hacer reverse sobre la lista

longestPath :: LTree a -> [a]
longestPath = foldLT id g
   where g x r1 r2 = if length r1 > length r2
                        then x : r1
                        else x : r2

data Objeto = Tesoro | Chatarra

data Mapa = Cofre Objeto
          | Bifurcacion Objeto Mapa Mapa

foldM :: (Objeto -> b) -> (Objeto -> b -> b -> b)
         Mapa -> b
foldM fc fb (Cofre obj) = fc obj
foldM fc fb (Bifurcacion obj m1 m2) = 
   fb obj (foldM fc fb m1) (foldM fc fb m2)

esTesoro Tesoro = True
esTesoro _      = False

hayTesoro :: Mapa -> Bool
hayTesoro = foldM esTesoro (\obj r1 r2 -> esTesoro obj || r1 || r2)

data Dir = Izq | Der

hayTesoroEn :: [Dir] -> Mapa -> Bool        
hayTesoroEn [] (Cofre obj) = esTesoro obj
hayTesoroEn (d:ds) (Cofre obj) = False
hayTesoroEn [] (Bifurcacion obj m1 m2) = esTesoro obj
hayTesoroEn (d:ds) (Bifurcacion obj m1 m2) =
     if d == Izq
        then hayTesoroEn ds m1
        else hayTesoroEn ds m2

hayTesoroEn' ds m = foldT fc fb m ds
  where fc obj [] = esTesoro obj
        fc obj (d:ds) = False
        fb obj r1 r2 [] = esTesoro obj
        fb obj r1 r2 (d:ds) =
                 if d == Izq
                    then r1 ds
                    else r2 ds


caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Cofre obj) = []
caminoAlTesoro (Bifurcacion obj m1 m2) =
    if esTesoro obj
       then []
       else if hayTesoro m1
               then Izq : caminoAlTesoro m1
               else Der : caminoAlTesoro m2

recM fc fb (Cofre obj) = fc obj
recM fc fb (Bifurcacion obj m1 m2) = 
   fb obj m1 m2 (recM fc fb m1) (recM fc fb m2)

-- Igual a fold pero pasando los mapas de la recursión sin procesar

caminoAlTesoro' = recT fc fb
   where fc obj = []
         fb obj m1 m2 r1 r2 = 
            if esTesoro obj
               then []
               else if hayTesoro m1
                       then Izq : r1
                       else Der : r2
