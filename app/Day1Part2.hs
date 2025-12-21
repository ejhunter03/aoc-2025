module Day1Part2 (
    solvePuzzle
) where

import Data.Maybe
import Day1Part1 (parseText)

numZeros :: Int -> [Int] -> Int
numZeros start lst = numZeros' 0 start lst where
    numZeros' n total (x:xs) = numZeros' n' total' xs where 
        total' = (x+total) `mod` 100
        --this is a correction term bc flipping from positive to negative should count but not 0 to negative
        correction = if ((x+total) <= 0) && (total /= 0) then 1 else 0
        n' = n + correction +  abs ((x+total) `quot` 100) 
    numZeros' n _ [] = n

solvePuzzle :: String -> Maybe Int
solvePuzzle blob = 
    let parsedInput = parseText blob
        filteredInput = catMaybes parsedInput
        partNumZeros = numZeros 50
        totalZeros = partNumZeros filteredInput
        x = if (all isJust parsedInput) then Just totalZeros else Nothing
    in x
