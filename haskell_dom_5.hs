data DFA = DFA Int [Int] (Int -> Char -> Int)

count :: DFA -> Int
count (DFA n _ _) = n

ends :: DFA -> [Int]
ends (DFA _ l _) = l

f :: DFA -> (Int -> Char -> Int)
f (DFA _ _ f) = f

alpha = ['a'..'z']

trans :: Int -> Char -> Int
trans 0 'a' = 0
trans 0 'b' = 0 
trans 0 'c' = 0 
trans 1 'a' = 0 
trans 1 'b' = 1 
trans 1 'c' = 1 
trans _  _  = -1

dfa1 :: DFA
dfa1 = DFA 2 [0] trans

dfa2 :: DFA
dfa2 = DFA 5 [4] (const . min 4 . (+1))

walk :: DFA -> String -> Int -> Bool
walk a s move 
	| null s = (move `elem` (ends a))
	| ((head s) `elem` alpha) && move > -1 && move < (count a) = 
	walk a (tail s) (f a move (head s))
	| otherwise = False

match :: DFA -> String -> Bool
match a s = walk a s 0
	 
	
	