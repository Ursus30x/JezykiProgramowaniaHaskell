digitsOfNum :: Int -> [Int]
digitsOfNum n = map (read . (:[])) (show (abs n))


isMonotonic :: (Ord a) => [a] -> Bool
isMonotonic xs = ascending xs || descending xs
  where
    ascending (x:y:rest) = x <= y && ascending (y:rest)
    ascending _ = True
    descending (x:y:rest) = x >= y && descending (y:rest)
    descending _ = True

numOfMonotoinAndNotMonotonic :: Int -> (Int,Int)
numOfMonotoinAndNotMonotonic n = (monotonic,notMonotonic)
    where
        monotonic = length [x | x <- [0..n], isMonotonic (digitsOfNum x) == True]
        notMonotonic = length [x | x <-[0..n], isMonotonic (digitsOfNum x) == False]

checkProcentage :: (Int,Int) -> Double
checkProcentage (a,b) =  fromIntegral b / fromIntegral (a + b)

smallestPrecentage :: Double -> Int
smallestPrecentage p = findN 0
  where
    findN n
      | checkProcentage (numOfMonotoinAndNotMonotonic n) > p = n
      | otherwise = findN (n + 1)


main :: IO()
main = do
    putStrLn "Podaj p:"
    input <- getLine
    let liczba = read input :: Double
    print (smallestPrecentage liczba)

