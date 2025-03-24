module Main where

add :: Int -> Int -> Int
add x y = x + y

absolute :: Int -> Int
absolute x = if x < 0 then -x else x

infixl 6 +++
(+++) :: Int -> Int -> Int
x +++ y = x + y

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main = do
    putStrLn "Fibonacci function defined."
    print (fibonacci 0)
    print (fibonacci 1)
    print (fibonacci 5)
    print (fibonacci 10)
    print (add 5 10)
    print (absolute (-5))
    print (5 +++ 10)
    print (factorial 5)