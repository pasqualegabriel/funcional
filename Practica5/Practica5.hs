
--Demostraciones simples

--Demostrar las siguientes equivalencias entre funciones (pueden ser falsas):

-- 1
last [x] = head [x]
       x = head [x] -- last 
       x = x        -- head
-- 2
swap . swap = id

-- 3
twice id = id

-- 4
applyN 2 = twice

-- 5
twice twice = applyN 4

-- 6
(\x -> maybe x id Nothing) = head . (:[])

-- 7 
curry (uncurry f) = f

-- 8
uncurry (curry f’) = f’

-- 9
maybe Nothing (Just . const 1) = const (Just 1)

-- 10
curry snd = curry (fst . swap)

-- 11
(\xs -> null xs || not (null xs)) = const True

-- 12
flip (curry f) = curry (f . swap)

-- 13
fst = uncurry const

-- 14
snd = uncurry (flip const)

-- 15
swap = uncurry (flip (,))

-- 16
or [x] = x || not x

-- 17
not (x && y) = not x || not y

-- 18
not (x || y) = not x && not y



