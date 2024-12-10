import Data.List (find, maximumBy)
import Data.Ord (comparing)

sumaDzielnikow :: Int -> Int
sumaDzielnikow n = sum [x | x <- [1..n-1], n `mod` x == 0]


czySaZaprzyjaznione :: Int -> Int -> Bool
czySaZaprzyjaznione a b = sumaDzielnikow a == b && sumaDzielnikow b == a

listaSumDzielnikow :: Int -> [(Int, Int)]
listaSumDzielnikow n = [(a, sumaDzielnikow a) | a <- [1..n], a /= sumaDzielnikow a, sumaDzielnikow a < n]

czyDrugaMniejszOdN :: (Int,Int) -> Int -> Bool
czyDrugaMniejszOdN (a,b) n = b < n

znajdzNajwiekszaZaprzyjaznionaLiczbe :: Int -> Maybe (Int,Int)
znajdzNajwiekszaZaprzyjaznionaLiczbe n = 
    let zaprzyjaznione = filter (uncurry czySaZaprzyjaznione) (listaSumDzielnikow n)
    in if null zaprzyjaznione
        then Nothing
        else Just (maximumBy(comparing fst) zaprzyjaznione)

main :: IO()
main = do
    let n = 2000
    print (znajdzNajwiekszaZaprzyjaznionaLiczbe n)