module Day4Part2 (
    solvePuzzle,
) where

import Data.Graph
import Data.Maybe
import Day4Part1

--NOTE: We make a simplifying assumption that the input array is not jagged

solvePuzzle :: String -> Maybe Int
solvePuzzle x = do
    (g, inspect, dict) <- parseInput x
    let vs = vertices g
    let goods = map (isVertexGood inspect dict) vs
    mGoods <- sequence goods
    let n = length $ filter id mGoods
    return n
