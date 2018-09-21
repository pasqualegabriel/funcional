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

gauss n = div (n*(n+1)) 2

-- Caso Base: gauss(Z)
gauss 0 = div (0*(0+1)) 2
gauss 0 = 0 -- arit
      0 = 0 -- gauss .1       

-- Caso Inductivo:
I Hipotesis inductiva: gauss(n)
I Tesis inductiva: gauss(S n)

gauss (n+1) = div ((n+1)*((n+1)+1)) 2

-- 3. Inducción sobre listas

-- Indique claramente el esquema inductivo de las listas. Luego, demuestre las siguientes propiedades sobre listas:

-- A
3A) length (xs ++ ys) = length xs ++ length ys

EI [a] = 3A([]) ^ (Vxs: [a]) (3A(xs) => (Vx: a) (3A(x:xs)))

-- Caso Base: 3A([])
length ([] ++ ys) = length [] + length ys
        length ys = length [] + length ys -- def (++)
        length ys = 0 + length ys         -- def lenght .1
        length ys = length ys             -- arit

-- Caso Inductivo:
I Hipotesis inductiva: 3A(xs)
I Tesis inductiva: 3A(x:xs)

length ((x:xs) ++ ys) = length (x:xs) + length ys
length (x:(xs ++ ys)) = length (x:xs) + length ys -- def (++)
1 + length (xs ++ ys) = length (x:xs) + length ys -- def length .2
1 + length (xs ++ ys) = 1 + length xs + length ys -- def length .2
1 + length xs ++ length ys = 1 + length xs + length ys -- HI

-- B
3B) reverse (xs ++ ys) = reverse ys ++ reverse xs

EI [a] = 3B([]) ^ (Vxs: [a]) (3B(xs) => (Vx: a) (3A(x:xs)))

-- Caso Base: 3B([])
reverse ([] ++ ys) = reverse ys ++ reverse []
        reverse ys = reverse ys ++ reverse [] -- def (++)
        reverse ys = reverse ys ++ []         -- def reverse .1
        reverse ys = reverse ys               -- def (++)

-- Caso Inductivo:
I Hipotesis inductiva: 3B(xs)
I Tesis inductiva: 3B(x:xs)

         reverse ((x:xs) ++ ys) = reverse ys ++ reverse (x:xs)
         reverse (x:(xs ++ ys)) = reverse ys ++ reverse (x:xs)    -- def (++)
      reverse (xs ++ ys) ++ [x] = reverse ys ++ reverse (x:xs)    -- def reverse .2
      reverse (xs ++ ys) ++ [x] = reverse ys ++ reverse xs ++ [x] -- def reverse .2
reverse ys ++ reverse xs ++ [x] = reverse ys ++ reverse xs ++ [x] -- HI

-- C
3C)      reverse . reverse = id
    (reverse . reverse) xs = id xs -- Principio de extencionalidad
    (reverse . reverse) xs = xs    -- def id
      reverse (reverse xs) = xs    -- def (.)

EI [a] = 3C([]) ^ (Vxs: [a]) (3C(xs) => (Vx: a) (3C(x:xs)))

-- Caso Base: 3C([])
reverse (reverse []) = []
          reverse [] = [] -- def reverse .1
                  [] = [] -- def reverse .1

-- Caso Inductivo:
I Hipotesis inductiva: 3C(xs)
I Tesis inductiva: 3C(x:xs)

   reverse (reverse (x:xs)) = (x:xs)
reverse (reverse xs ++ [x]) = (x:xs) -- def reverse .2

