-- create a list of tuples with the index and the value
indexList :: [a] -> [(Int, a)]
indexList l = zip [1..] l

sublists  :: [a] -> [[(Int, a)]]
sublists l = 
    let indexed = indexList l
    in [take n (drop m indexed) | m <- [0..length l - 1], n <- [1..length l - m]]

sumValues :: [(Int, Int)] -> Int -- sum of values in a list of tuples
sumValues = sum . map snd

quicksort :: (Ord a) => [a] -> [a] -- Sorting function
quicksort [] = []
quicksort (x:xs) = quicksort [s | s <- xs, s <= x] ++ [x] ++ quicksort [u | u <- xs, u > x]

findKSmallest :: Int -> [(Int, [(Int, Int)])] -> [[(Int, Int)]]
findKSmallest 0 _ = []  -- Base case: Stop when k elements are collected
findKSmallest _ [] = []  -- Base case: Stop if list is empty
findKSmallest k ((_, sub):rest) = sub : findKSmallest (k-1) rest  -- Take sublist and recurse

smallestKSums :: Int -> [Int] -> [[(Int, Int)]]
smallestKSums k l =
    let allSubs = sublists l
        sumWithSubs = [(sumValues sub, sub) | sub <- allSubs]  -- Pair sums with sublists
        sortedSubs = quicksort sumWithSubs  -- Sort using quicksort
    in findKSmallest k sortedSubs  -- Recursively find k smallest sublists

printSublist :: [[(Int, Int)]] -> IO ()
printSublist [] = return ()  -- Base case: Stop when list is empty
printSublist (s:subs) = do
    let values = map snd s  -- Extract values from (index, value) tuples
    let firstIndex = fst (head s)  -- First index
    let lastIndex = fst (last s)  -- Last index
    putStr $ show (sumValues s) ++ "\t" ++ show firstIndex ++ "\t" ++ show lastIndex ++ "\t" ++ show values ++ "\n"  -- Print sum, first index, last index and values
    printSublist subs  -- Recursively print remaining sublists
