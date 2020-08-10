import Data.List

sublists :: (Eq t) => [t] -> [[t]]
sublists [] = [[]]
sublists list = nub (foldr1 (++) [let l1 = sublists (delete x list) in l1 ++ (map ((:) x) l1) | x<-list])

makesub l x = [ l!!i | i <- x ]

dupls :: (Eq a, Num a, Foldable t) => [a -> a] -> t a -> [Int]
dupls fs nums = [ length x | let s = sublists [0..((length fs) - 1)], not (null s), 
                  x <- s, y <- fs, 
                  not (null x) && 
                  all (\z -> y z == compose (makesub fs x) z) nums ]

maxFL :: Ord a => [a] -> a -> a
maxFL lens m 
    | null lens         = m
    | m < head lens     = maxFL (tail lens) (head lens)
    | otherwise         = maxFL (tail lens) m

check :: (Eq a, Num a, Foldable t) => [a -> a] -> t a -> Int
check funcs nums = maxFL fLs (head fLs) where fLs = dupls funcs nums

compose :: (Num a, Eq a) => [a -> a] -> (a -> a)
compose = foldr (.) id 