import Prelude hiding (map)

promedio xs = div (sum xs) (length xs)

promedio' xs = 
	let (s, l) = sl xs
	    in div s l

sl [] = (0, 0)
sl (x:xs) = 
	let (s, l) = sl xs
	    in (s+x, l+1)

isl s l []     = (s, l)
isl s l (x:xs) = isl (s+x) (l+1) xs

promedio''' = slp 0 0

slp s l [] = div s l
slp s l (x:xs) = slp (s+x) (l+1) xs

succs :: [Int] -> [Int]
succs [] = []
succs (x:xs) = x+1 : succs xs

succs' xs = isuccs xs []

isuccs [] rs = rs
isuccs (x:xs) rs = isuccs xs (x+1 : rs)

-- isuccs [] rs = rs
-- isuccs (x:xs) rs = isuccs xs (rs ++ [x+1])

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

map' f xs = imap f xs []

imap f [] rs = rs
imap f (x:xs) rs = imap f xs (rs ++ [f x])