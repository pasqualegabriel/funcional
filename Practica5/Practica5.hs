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
curry (uncurry f) x y = f x y -- Principio de extencionalidad  
     uncurry f (x, y) = f x y -- def curry
                f x y = f x y -- def uncurry

-- 8
       uncurry (curry f) = f
uncurry (curry f) (x, y) = f (x, y) -- Principio de extencionalidad 
             curry f x y = f (x, y) -- def uncurry
                f (x, y) = f (x, y) -- def curry

-- 9
maybe Nothing (Just . const 1) = const (Just 1)

-- 10
    curry snd = curry (fst . swap)
curry snd x y = curry (fst . swap) x y -- Principio de extencionalidad 
   snd (x, y) = curry (fst . swap) x y -- def curry
   snd (x, y) = (fst . swap) (x, y)    -- def curry
            y = (fst . swap) (x, y)    -- def snd
            y = fst (swap (x, y))      -- def (.)
            y = fst (y, x)             -- def swap
            y = y                      -- def fst

-- 11
(\xs -> null xs || not (null xs)) = const True

-- 12
    flip (curry f) = curry (f . swap)
flip (curry f) x y = curry (f . swap) x y -- Principio de extencionalidad 
       curry f y x = curry (f . swap) x y -- def flip
          f (y, x) = curry (f . swap) x y -- def curry
          f (y, x) = (f . swap) (x, y)    -- def curry
          f (y, x) = f (swap (x, y))      -- def (.)
          f (y, x) = f (y, x)             -- def swapp

-- 13
       fst = uncurry const         
fst (x, y) = uncurry const (x, y) -- Principio de extencionalidad 
         y = uncurry const (x, y) -- def fst
         y = const y x            -- def uncurry
         y = y                    -- def const

-- 14
       snd = uncurry (flip const)
snd (x, y) = uncurry (flip const) (x, y) -- Principio de extencionalidad 
         y = uncurry (flip const) (x, y) -- def snd
         y = flip const x y              -- def uncurry
         y = const y x                   -- def flip
         y = y                           -- def const

-- 15
       swap = uncurry (flip (,))
swap (x, y) = uncurry (flip (,)) (x, y) -- Principio de extencionalidad 
     (y, x) = uncurry (flip (,)) (x, y) -- def swap
     (y, x) = flip (,) x y              -- def uncurry
     (y, x) = (y, x)                    -- def flip

-- 2 Inducción sobre los naturales

EORO :: even n || odd n ≡ True

-- Caso Base: EORO(Z)
even Z || odd Z = True
  True || odd Z = True -- even .1
           True = True -- (||).1

-- Caso Inductivo:
I Hipotesis inductiva: EORO(m)
I Tesis inductiva: EORO(S m)

even (S m) || odd (S m) = True
     odd m || odd (S m) = True -- EVEN .2
        odd m || even m = True -- ODD .2
        even m || odd m = True -- CONMUT
                  True = True  -- HI

-- Ejercicio D
gauss 0 = 0
gauss n = n + gauss (n -1)

gauss n ≡ div (n*(n+1)) 2
gauss n ≡ div (n*(n+1)) 2 -- 



