module Day5Part2 (
    solvePuzzle,
    parseInput
) where

import qualified Day5Part1 as Part1
import qualified Data.Vector as V

parseInput :: String -> Maybe (V.Vector (Int, Int))
parseInput blob = case Part1.parseInput blob of
    Just (rangeVec, _) -> Just rangeVec
    Nothing -> Nothing

solvePuzzle :: String -> Maybe Int
solvePuzzle x = case parseInput x of
    Just vec -> Just (V.sum $ V.map (\(x,y) -> y-x+1) vec)
    _ -> Nothing
