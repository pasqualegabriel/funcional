
--Demostraciones simples

--Demostrar las siguientes equivalencias entre funciones (pueden ser falsas):

-- 1
last [x] = head [x]
       x = head [x] -- def last 
       x = x        -- def head

-- 2
         swap . swap = id
(swap . swap) (x, y) = id (x, y) -- Principio de extensionalidad
  swap (swap (x, y)) = id (x, y) -- def (.)
         swap (y, x) = id (x, y) -- def swapp
              (x, y) = id (x, y) -- def swapp
              (x, y) = (x, y)    -- def id
 
-- 3
   twice id = id
 twice id x = id x -- Principio de extencionalidad 
(id . id) x = id x -- def twice
  id (id x) = id x -- def (.)
       id x = id x -- def id

-- 4
          applyN 2 = twice
        applyN 2 f = twice f -- Principio de extencionalidad x2
    f . applyN 1 f = twice f -- def applyN
f . f . applyN 0 f = twice f -- def applyN
        f . f . id = twice f -- def applyN
             f . f = twice f -- (id neutro de .)
             f . f = f . f   -- def twice

-- 5
      twice twice = applyN 4                     
    twice . twice = applyN 4           -- def twice
(twice . twice) f = applyN 4 f         -- Principio de extencionalidad  
  twice (twice f) = applyN 4 f         -- def (.)
    twice (f . f) = applyN 4 f         -- def twice
(f . f) . (f . f) = applyN 4 f         -- def twice
    f . f . f . f = applyN 4 f         -- asociatividad de (.)
    f . f . f . f = f. applyN 3 f      -- def applyN
    f . f . f . f = f . f . applyN 2 f -- def applyN
    f . f . f . f = f . f . twice f    -- def ejercicio 4 (applyN 2 = twice) 
    f . f . f . f = f . f . (f . f)    -- def twice
    f . f . f . f = f . f . f . f      -- asociatividad de (.)

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



