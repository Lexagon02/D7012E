-- 9.2
lengthOfList :: [Double] -> Double
lengthOfList [] = 0
lengthOfList a = sum (map (\x -> if x /= 1 then 1 else x) a)

-- 9.4
{-
Map addOne (map addOne ns) would take a list ns and add 1 each element and then add 1 again to each element.
map f (map g xs) would apply g to xs and then apply f to the result. and is equivalent to map (g . f) xs. 
-}

-- 9.6
squareList :: [Int] -> [Int]
squareList [] = []
squareList ns = map (\x -> if x /= 0 then fromIntegral (x * x) else 0) ns

sumList :: [Int] -> Int
sumList [] = 0
sumList ns = sum ns

-- 9.7 
minValue :: [Int] -> Int
minValue [] = 0
minValue ns = foldl min (head ns) ns

equalValuesInList :: [Int] -> Bool
equalValuesInList [] = True
equalValuesInList ns = foldl (\acc x -> (x == head ns) && acc) True ns

accendingListGreaterThanZero :: [Int] -> Bool
accendingListGreaterThanZero [] = True
accendingListGreaterThanZero [_] = True
accendingListGreaterThanZero (x:y:ns) = x > 0 && y > x && accendingListGreaterThanZero (y:ns)

-- 9.9
iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 f x = x
iter n f x = iter (n-1) f (f x)

-- 9.10
squareNTimes :: Int -> Int
squareNTimes 0 = 1
squareNTimes n = iter n (*2) 1

-- 9.11
sumOfSquares :: Int -> Int
sumOfSquares 0 = 0
sumOfSquares n = sum (map (\x -> x * x) [1..n])

-- 9.16
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs)
    | p x       = x : filterFirst p xs
    | otherwise = xs

-- returnLoan function
returnLoan :: Eq a => a -> [a] -> [a]
returnLoan book loans = filterFirst (/= book) loans