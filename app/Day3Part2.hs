module Day3Part2 (
    solvePuzzle
) where

import Day3Part1 (parseInput)
import qualified Data.Vector.Unboxed as VU

findMaxJoltage :: VU.Vector Int -> Int
findMaxJoltage arr = findMaxJoltage' arr 0 12 0 where
    findMaxJoltage' x i limit total = if i >= limit then total
        else let
            xslice = VU.slice 0 (VU.length x - limit + i + 1) x
            loc = VU.maxIndex xslice
            maxDigit = xslice VU.! loc
            total' = 10*total + maxDigit
            i' = i+1
            x' = VU.slice (loc+1) (VU.length x - (loc+1)) x 
        in findMaxJoltage' x' i' limit total'

solvePuzzle :: String -> Int
solvePuzzle x = sum $ map findMaxJoltage (parseInput x)
