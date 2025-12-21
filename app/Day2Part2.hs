module Day2Part2 (
    solvePuzzle,
    isBad
) where

import Day2Part1 (parseInput, genRange)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Search as Search


isBad :: Int -> Bool
isBad x = let
        str = show x
        doubledStr = str ++ str
        byteStr = Char8.pack doubledStr
        reps = Search.indices (Char8.pack str) byteStr
        ret = length reps > 2
    in ret

findBadsInPair :: (Int, Int) -> [Int]
findBadsInPair x = bads where
    range = genRange x
    bads = filter isBad range

solvePuzzle :: String -> Maybe Int
solvePuzzle x = ret where
    listOfTuples = parseInput x
    badsPerPair = traverse (fmap findBadsInPair) listOfTuples
    bads = fmap concat badsPerPair
    ret  = fmap sum bads
