divs :: Int -> [Int]
divs x = [ y | y <- [1..x-1], x `mod` y == 0, y /= 1]

primes :: Int -> [Int]
primes n = [ x | x <- [1..n], null (divs x)]

isums i = [ (x,y) | x <- (primes i), y <- (primes i), (x+y) == i ]

reprs = [ i | j <- [0..], let i = length (isums j) ]