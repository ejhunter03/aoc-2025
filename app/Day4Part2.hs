module Day4Part2 (
    solvePuzzle
) where

import Data.Graph
import Data.Maybe
import Day4Part1 (parseInput, isVertexGood)

--NOTE: We make a simplifying assumption that the input array is not jagged
--the adjacency list, then the keys, then the new node values, 
probSpecMask :: Int -> Bool -> Int
probSpecMask x b
    | (x==1) && b = 0
    | (x==1) && not b = 1
    | otherwise = 0

solvePuzzle' :: Graph -> (Vertex -> (Int, Int, [Int])) -> (Int -> Maybe Vertex) -> Int -> Maybe Int
solvePuzzle' g inspect dict acc = do
        let vs = vertices g
        let goods = map (isVertexGood inspect dict) vs
        mGoods <- sequence goods --maybe list of bool
        let increment = length $ filter id mGoods
        let edgeList = [inspect x | x <- vs]
        let edgeList' = [(probSpecMask x b,y,z) | ((x,y,z),b) <- zip edgeList mGoods]
        let (g', inspect', dict') = graphFromEdges edgeList'
        let acc' = acc + increment
        (if increment > 0 then solvePuzzle' g' inspect' dict' acc' else Just acc')

solvePuzzle :: String -> Maybe Int
solvePuzzle x = do
    (g, inspect, dict) <- parseInput x
    solvePuzzle' g inspect dict 0
