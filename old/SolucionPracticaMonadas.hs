f' = 
  do
   xs <- tailM [1..5]
   xs <- initM xs
   xs <- tailM xs
   initM xs

f' = tailM [1..5] >>= initM >>= tailM >>= initM

-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]

-- específicamente para esta mónada
-- filterM :: (a -> [Bool]) -> [a] -> [[a]]

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

data Env = Map String Int
data Exp = Sum Exp Exp | Var String | Const Int

eval :: Exp -> Reader Env Int
eval (Const n)   = return n
eval (Sum e1 e2) = 
    do
      x <- eval e1
      y <- eval e2
      return (x+y)
eval (Var s) = lookup s

mayoresA :: Int -> [Int] -> Writer [String] [Int]
mayoresA n = foldr g (return [])
  where g x r = if x > n
                   then tell ["Me quedo con: " ++ show x] >> r
                        -- es lo mismo que
                        -- do 
                        --  tell ["Me quedo con: " ++ show x]
                        --  r
                   else r

gcd :: Int -> Int -> Writer [Int] Int
gcd a b =
  do
    tell [a, b]
    if b == 0 then return a else gcd b (a `mod` b)

data Color = Azul | Negro
data Dir = Izq | Der
data Celda = C Int Int
data Tablero = T [(Int,Celda)] Int

poner :: Color -> State Tablero ()
poner c = modify (poner' c)

poner' c (T xs n) n = T (map g xs) n
  where g (i, celda) = if i == n
                          then (i, poner'' c celda) 
                          else (i, celda)

poner'' Negro (C n a) = C (n+1) a
poner'' Azul  (C n a) = C n (a+1) 

mover :: Dir -> State Tablero ()
mover d = modify (mover' d)

mover' Izq (T xs n) = T xs (n-1)
mover' Der (T xs n) = T xs (n+1)

nroBolitas :: Color -> Tablero -> Int
nroBolitas c (T xs n) = 
    let [celda] = filter (\(i,celda) -> i == n) xs
        in nroBolitas' c celda

nroBolitas' Azul (C n a) = a
nroBolitas' Negro (C n a) = n

puedeMover :: Dir -> Tablero -> Bool
puedeMover Izq (T xs n) = n > 0
puedeMover Der (T xs n) = n < (length xs - 1)

program =
  do
    ponerN 10 Negro
    ponerN 5 Azul
    moverM 3 Der
    when (puedeMover Der) $ mover Der

ponerN n c = replicateM_ n (poner c)
moverN n d = replicateM_ n (mover d)

void :: Monad m => m a -> m ()
void m = m >> return ()

when :: Monad f => Bool -> f () -> f ()
when b m = if b then m else return ()

liftM :: Monad m => (a -> b) -> m a -> m b
lifM = fmap

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = 
  do
    x <- m1
    y <- m2
    return $ f x y

appM :: (Monad m) => m (a -> b) -> m a -> m b
appM mf mx =
  do
    f <- mf
    x <- mx
    return $ f x

join :: (Monad m) => m (m a) -> m a
join m =
  do
    m' <- m
    m'

sequence :: Monad m => [m a] -> m [a]
sequence = foldr g (return [])
  where g m r =
      do
        x <- m
        xs <- r
        return (x:xs)

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = foldr g (return [])
  whre g y r =
      do
        x  <- f y
        xs <- r
        return (x:xs)


mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = foldr (\x r -> f x >> r) (return ())

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM f = foldr g (return [])
  where g x r =
      do
        b <- f x
        if b
           then fmap (x:) r
           else r

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = f x >>= g

forever :: Monad m => m a -> m b
forever m = m >> forever m

zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM f z = foldr g (return z)
  where g rm x = rm >>= (\r -> f r x)

foldM_ :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
foldM_ f z xs = void (foldM f z xs)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n m = sequence_ (replicate n m)