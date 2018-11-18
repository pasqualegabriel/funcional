
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
makeGralBoard r c 0 n d res = rows r '.' c
makeGralBoard r c m 1 d res = buildRows r c m 1 d res
makeGralBoard 1 c m n d res = [(row n '.') ++ (row m '*')] 
makeGralBoard r 1 m n d res = rows n '.' 1 ++ rows m '*' 1
makeGralBoard 2 c m n d res = if (n==2||n==3||odd m)
							  then ["Impossible"]
							  else (row (max 0 (c- (div m 2))) '.'++ row (div m 2) '*') : [(row (max 0 (c- (div m 2))) '.'++ row (div m 2) '*')]
makeGralBoard r 2 m n d res = if (n==2||n==2||odd m)
							  then ["Impossible"]
							  else rows d '.' 2 ++ rows (max 0 (r-d)) '*' 2






							  
makeGralBoard r c m n d res = if (n>9) && (n>= 3*c) ----cambiar 9 por 4.
							  then haySolucion1 r c m n d res
							  else 	if (n>=9) && (n < 3 * c)
							  		then haySolucion2 r c m n d res
							  		else nMenorANueve r c m n d res


{-
Ahora nos queda como mucho 9 celdas vacias localizadas en la punta derecha inferior en tablero de 3x3
En este caso, podemos chequear a MANO que IF n==5||n==7, es IMPOSIBLE.
Sino, hardcodea una configuracion valida para cada numero de celda vacia en 3x3 cuadrados.

-}



nMenorANueve:: Int->Int->Int->Int->Int->Int->Board
nMenorANueve r c m 5 d res = ["Impossible"]
nMenorANueve r c m 7 d res = ["Impossible"]
nMenorANueve r c m n d res = if  haySolucion2 r c m n d res


haySolucion1::Int->Int->Int->Int->Int->Int->Board
haySolucion1 r c m n d res = rows (d-1) '.' c ++ [row (c-1) '.' ++ "*"] ++
							 [row 2 '.' ++ row (c-2) '*'] ++ rows (r-(d+1)) '*' c --Puede fallar el ultimo


--R=5 C=6 M=11 N=13 D=2 R=1
haySolucion2::Int->Int->Int->Int->Int->Int->Board
haySolucion2 r c m n d res =  if mod m c == 0 
							  then rows (r - (div m c)) '.' c ++ rows (div m c) '*' c
							  else if (div m c) + 3 == r
	 							   then [row (c-1) '.' ++ "*"] ++ [row (c-1) '.' ++ "*"] ++ [row (c-3) '.' ++ "***"] ++ (rows (r-3) '*' c)
	 							   else rows (r - (div m c) + 3 ) '.' c ++ [row (c-1) '.' ++ "*"] ++ [row (c-1) '.' ++ "*"] ++ rows (div m c) '*'  c ++ (rows (r-3) '*' c)


--minasQueFaltan = m-((r-3)*c)
--minasQuePusiste = (r-3)*c




{-HAYSOLUCION3 XXXXXXXXXXX
n<3*c && n>=9 => llena r-3 de '*'
tres filas restantes => llenar las minas q faltan columna x columna de der a izq. 
SI mnq en c ==2, la ultima mina se pone en la proxima columna.

c....*
.....*
...***



c.....
******
******


-}


{-
TABLEROS AL MENOS 3X3
En este caso, n>9 =====> HAY SOLUCION
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-}

{- HAYSOLUCION2 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
si n=> 3*C => llenar de minas izq a der, arriba a abajo. XXXXX 
nmq >= c || nmq < c-2 => coloca nmq izq a deren esa fila. 
nmq == c-1 => la ultima mina en la fila que sigue.

-}







--makeGralBoardAux r c m n d res


--makeGralBoard r c m n d 1   = rows (d-1) '.' c ++ [row (c-1) '.' ++ "*"] ++ [row 2 '.' ++ row (c-2) '*'] ++ rows (r-(d+1)) '*' c
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





