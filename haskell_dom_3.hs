rowBuild s e r = [ s!!i | i <- [0..e], i /= r ]

colBuild s e c = [ s!!i!!c | i <- [0..e], i /= c ]

colsize :: [Int] -> Int -> Int
colsize row size = if null row 
                   then (size-1)
				   else colsize (tail row) (size+1)

rowsize :: [[Int]] -> Int -> Int
rowsize mat size = if null mat 
                   then (size-1) 
				   else rowsize (tail mat) (size+1)

minEl :: Int -> [Int] -> Bool
minEl el s
	| null s         = True
	| el >= (head s) = False
	| otherwise      = minEl el (tail s)
	
maxEl :: Int -> [Int] -> Bool
maxEl el s
	| null s         = True
	| el <= (head s) = False
	| otherwise      = maxEl el (tail s)

valid :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
valid s i j row col = if ((minEl (s!!i!!j) (rowBuild (s!!i) col i))    == True
                         && (maxEl (s!!i!!j) (colBuild s row j))       == True)
                         || ((maxEl (s!!i!!j) (rowBuild (s!!i) col i)) == True
                         && (minEl (s!!i!!j) (colBuild s row j))       == True) 
			             then True else False
					  
	
saddles mat row col = [ (i,j) | 
                         i <- [0..row], j <- [0..col], valid mat i j row col]
						 
allSaddles mat = saddles mat (rowsize mat 0) (colsize (mat!!0) 0)
							 
hasSaddle mat = if (null (allSaddles mat))
                then False
				else True