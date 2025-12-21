module Day3Part1 (
    solvePuzzle,
    parseInput
) where

import qualified Data.Vector.Unboxed as VU
import Data.Char

findMaxJoltage :: VU.Vector Int -> Int
findMaxJoltage x = 10*num1 + num2 where
    index1 = VU.maxIndex (VU.init x :: VU.Vector Int)
    num1 = x VU.! index1
    remSlice = VU.slice (index1+1) (VU.length x -(index1+1)) x
    num2 = VU.maximum remSlice

parseInput :: String -> [VU.Vector Int]
parseInput x = parsed where
    inLines = lines x
    lineLists = map (map digitToInt) inLines
    parsed = map VU.fromList lineLists

solvePuzzle :: String -> Int
solvePuzzle x = sum $ map findMaxJoltage (parseInput x)
