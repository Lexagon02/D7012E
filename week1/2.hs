import Data.Char (ord,chr)


-- 3

-- fst and snd are declared in Prelude

mk3 :: a -> (a, a, a)
mk3 c = (c, c, c)

addPair1 :: (Int, Int) -> Int
addPair1 p = fst p  +  snd p

addPair2 :: (Int, Int) -> Int
addPair2 (n, m) = n  +  m


-- 4

fib :: Int-> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1)  +  fib (n - 2)

fibStep :: (Int,Int) -> (Int,Int)
fibStep (u,v) = (v,u+v)

fibPair :: Int -> (Int,Int)
fibPair n
  | n==0        = (0,1)
  | otherwise   = fibStep (fibPair (n-1))

fastFib :: Int -> Int
fastFib = fst . fibPair


-- 11

ex :: [Int] -- used in doubleEven below
ex = [2, 4, 7, 9, 10]

doubleEven :: [Int]
doubleEven = [ 2*n | n <- ex, n `mod` 2 == 0]

pairs :: [(Int, Int)] -- used to demonstrate addPairs below
pairs = [(2,4),(7,6),(1,3),(0,2),(8,7),(6,1),(5,5)]

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [ m + n | (m, n)  <-  pairList ]

elem :: Int -> [Int] -> Bool
elem v xs = length [x | x <- xs, x == v] > 0


-- 16


sum' ::  [ Int ]  ->  Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

mystery1  ::  Int  ->  Int  ->  Int
mystery1 0 y = y
mystery1 x _ = x

mystery2  ::  Int  ->  Int  ->  Int
mystery2 x y 
  | x==0        = y
  | otherwise   = x


-- 17

mystery3  ::  Int  ->  Int  ->  Int
mystery3 x y =  case (x) of
              0  ->  y
              _  ->  x


-- 18


sumSquares1  :: Int -> Int -> Int
sumSquares1 n m = sqN + sqM
  where
    sqN = n * n
    sqM = m * m

sumSquares2  :: Int -> Int -> Int
sumSquares2 n m = sq n + sq m
  where
    sq  ::  Int -> Int
    sq x = x * x


-- 19

allPairs  ::  [v]  ->  [w]  ->  [ (v,w) ]
allPairs xs ys  =  [ (x,y) | x <- xs, y <- ys ]

heads  ::  [ [u] ]  -> [u]
heads ls  =  [ x | ( x : xs )  <-  ls ]

zip2 :: [a] -> [b] -> [(a,b)]
zip2 (a:as) (b:bs) = (a,b) : zip as bs
zip2 _      _      = []

mulPairs :: [ (Int, Int) ] -> [ Int ]
mulPairs pairList = [ m * n | (m, n)  <-  pairList ]

dotProduct  ::  [ Int ]  ->  [ Int ]  ->   Int 
dotProduct xs ys  =  ( sum' . mulPairs ) ( zip2 xs ys )


-- Example on the white board - extra candy

ones  ::  [ Int ]
ones = 1 : ones

--   take 100 ones   gives a list with 100 ones

-- ones is the same as  [1,1 .. ], that is the list
-- that starts with 1 and 1 and then goes on forever

nat :: [Int]
nat = 0 : [ x+1 | x <- nat ]

--   take 100 nat   gives the 100 first natural
-- numbers 0,1,2,..,99

-- nat is the same as  [0,1 .. ] because we start with a 0
-- and then we append all natural numbers (=nat) after
-- having added 1 to all of them.

-- Extra 2019:
-- checks if there is a number that occurs in both argument lists or not

unique xs ys = length [ 0 | x <- xs, y<-ys, x==y ] == 0