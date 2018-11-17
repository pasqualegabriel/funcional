
type Board= [String]

main = do
	src       <- readFile "C-small-practice.in"
	let fileLines = (lines src)
	let nCases    = read (head (fileLines)) :: Int 	

	let cases 	  = intmap (tail fileLines)
	
	let game      = aux cases 1

	writeFile "MinesweeperOutput.in" game

	putStrLn game

	
-------------------------------------------------------------------------------------

aux::[[Int]]->Int-> String
aux = foldr (\c rs i -> ("Case #"++ show i ++":" ++ "\n" ++ boardToStr (main1 c)) ++ rs (i+1)) (const []) 

intmap :: [String] -> [[Int]]
intmap x = map (\l -> map read (words l)) x

boardToStr::Board->String
boardToStr b = concatMap (flip (++) "\n") b

main1 :: [Int] -> Board
main1 xs = let r = head xs;
			   c = head (tail xs);
			   m = last xs;
		 	   n = r*c - m in
			if (checkData r c m n) 
			then replaceC (makeGralBoard r c m n (div n c) (mod n c))
			else ["Impossible"]
				 
replaceC :: Board -> Board
replaceC [] 	= []
replaceC (s:ss) = if s=="Impossible"
				  then s:ss
				  else ("c" ++ tail s):ss



--D= N/C
--res = N % C
makeGralBoard:: Int -> Int -> Int -> Int -> Int -> Int -> Board
makeGralBoard 1 1 0 _ _ _   = ["."]
makeGralBoard r c m 1 d res = buildRows r c m 1 d res
makeGralBoard 1 c m n d res = [(row n '.') ++ (row m '*')] 
makeGralBoard r 1 m n d res = rows n '.' 1 ++ rows m '*' 1
makeGralBoard 2 c m n d res = if (n==2||n==3||odd m)
							  then ["Impossible"]
							  else (row (max 0 (c- (div m 2))) '.'++ row (div m 2) '*') : [(row (max 0 (c- (div m 2))) '.'++ row (div m 2) '*')]
makeGralBoard r 2 m n d res = if (n==2|| odd m)
							  then ["Impossible"]
							  else buildRows r 2 m n d res
makeGralBoard r c m n d 1   = rows (d-1) '.' c ++ [row (c-1) '.' ++ "*"] ++ [row 2 '.' ++ row (c-2) '*'] ++ rows (r-(d+1)) '*' c
makeGralBoard r c m n d res = makeGralBoardAux r c m n d res

---------------------------------------------------------------------------------------------------

--A desarrollar...........................
checkData :: Int -> Int -> Int -> Int -> Bool
checkData r c m n= (m < r*c) && (n/=0)

rows::Int -> Char -> Int -> Board
rows 0 ch c = []
rows d ch c = row c ch : rows (d-1) ch c

row::Int -> Char-> String
row 0 c = []
row n c= c:row (n-1) c


makeGralBoardAux r c m n d res 
	| even n    = evenNBoard r c m n d res (buildRows r c m n d res)
	| otherwise = oddNBoard r c m n d res (buildRows r c m n d res)


buildRows :: Int -> Int -> Int -> Int -> Int -> Int -> Board
buildRows r c m n d res = (rows d '.' c) ++ [(row res '.') ++ (row (c-res) '*')] ++ (rows (max (r-(d+1)) 0) '*' c)



evenNBoard r c m n d res f 
	|n >=4     = if (n<c*2)
			     then [row (div n 2) '.' ++ row  (c-(div n 2))'*'] ++ [row (div n 2) '.' ++ row  (c-(div n 2))'*'] ++ rows (r-2) '*' c
			     else f
	|otherwise = ["Impossible"]

oddNBoard r c m n d res f 
	|(n>=9 && r >=3 && c>=3) = if (n<c*2+3)
		 	  				   then (row (div (n-3) 2) '.' ++ row (c-(div (n-3) 2)) '*') :
		 	  				        [row (div (n-3) 2) '.' ++ row (c-(div (n-3) 2)) '*'] ++
		 	  				        (row 3 '.' ++ row (c-3) '*') : rows (r-3) '*' c 
		 	  				   else f
	|otherwise = ["Impossible"]





