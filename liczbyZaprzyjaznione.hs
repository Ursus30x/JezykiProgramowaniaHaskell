find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs)
    | p x       = Just x
    | otherwise = find p xs

sumOfDivisors :: Int -> Int
sumOfDivisors n = sum [x | x <- [1..n-1], n `mod` x == 0]

areAmicable :: Int -> Int -> Bool
areAmicable a b = sumOfDivisors a == b && sumOfDivisors b == a

listOfSumOfDivisors :: Int -> [(Int, Int)]
listOfSumOfDivisors n = [(a, sumOfDivisors a) | a <- [1..n], a /= sumOfDivisors a, sumOfDivisors a < n]

findMaxTuple :: Ord a => [(a, b)] -> (a, b)
findMaxTuple [] = error "findMaxTuple: empty list"
findMaxTuple (x:xs) = foldl maxBy x xs
  where
    maxBy acc y = if fst y > fst acc then y else acc

findLargestAmicableNumber :: Int -> Maybe (Int, Int)
findLargestAmicableNumber n = 
    let amicablePairs = filter (uncurry areAmicable) (listOfSumOfDivisors n)
    in if null amicablePairs
        then Nothing
        else Just (findMaxTuple amicablePairs)


--- mozemy sprowbowac tez to zrobic szukajac powtarzajacych sie krotek bo to dziala giga wolno, a tamto moze bedzie lepsze???

main :: IO()
main = do
    putStrLn "Podaj n:"
    input <- getLine
    let n = read input :: Int
    print (findLargestAmicableNumber n)