module Day4Part1 (
    solvePuzzle,
    parseInput,
    parseText
) where

import Data.Graph
import Data.Maybe

--NOTE: We make a simplifying assumption that the input array is not jagged

--These two functions assume we index
--  0 1 2 3
--  4 5 6 7
--  8 9 10 11
--One might index like a snake as in the proof that the cartesian product of the naturals with itself is countable
posToKeyNCols :: Int -> (Int,Int) -> Int
posToKeyNCols ncols (x,y) = x*ncols + y

posToKeyNRows :: Int -> (Int,Int) -> Int
posToKeyNRows nrows (x,y) = x + y*nrows

posToKey :: Int -> (Int,Int) -> Int
posToKey = posToKeyNCols
posToKey' :: Int -> (Int,Int) -> Int
posToKey' = posToKeyNRows

--Without knowing the size of the array we can only accept the number of columns here
keyToPos :: Int -> Int -> (Int,Int)
keyToPos ncols key = (key `div` ncols, key `mod` ncols)

--second parameter is the number of columns
buildEdgeList :: [Int] -> Int -> Maybe [[Int]]
buildEdgeList keys ncols = let
        nrows = length keys `div` ncols
        rightShape = (length keys `mod` ncols) == 0
        edgeList = map (edgeListFromKey ncols nrows) keys
    in if rightShape then Just edgeList else Nothing

edgeListFromKey :: Int -> Int -> Int -> [Int]
edgeListFromKey ncols nrows key = let 
        pos = keyToPos ncols key
        posEdgeList = edgeListFromPos nrows ncols pos
        keyEdgeList = map (posToKey ncols) posEdgeList
    in keyEdgeList

--first param rows, second param columns, third is pos(x,y)
edgeListFromPos :: Int -> Int -> (Int,Int) -> [(Int,Int)]
edgeListFromPos nrows ncols (x,y)
    | (x==0) && (y==0) = [(1,0), (1,1), (0,1)]
    | (x==0) && (y==(ncols-1)) = [(1,y), (1,y-1), (0,y-1)]
    | (x==(nrows-1)) && (y==0) = [(x-1,y), (x-1,y+1), (x,y+1)]
    | (x==(nrows-1)) && (y==(ncols-1)) = [(x-1, y), (x-1,y-1), (x,y-1)]
    | x == 0 = [(x,y-1), (x,y+1), (x+1, y-1), (x+1, y), (x+1,y+1)]
    | y == 0 = [(x-1, y), (x+1, y), (x-1,y+1), (x,y+1), (x+1, y+1)]
    | x == (nrows-1) = [(x,y-1), (x,y+1), (x-1,y-1), (x-1,y), (x-1,y+1)]
    | y == (ncols-1) = [(x-1,y), (x+1,y) , (x-1,y-1), (x,y-1), (x+1,y-1)]
    | otherwise = [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y+1), (x+1,y+1), (x+1,y), (x+1,y-1), (x,y-1)]

symbolToInt :: Char -> Maybe Int
symbolToInt x
    | x == '.' = Just 0
    | x == '@' = Just 1
    | otherwise = Nothing

parseText :: String -> Maybe ([Int], Int)
parseText blob = let
        l = lines blob
        ls = map (map symbolToInt) l
        lsclean = map catMaybes ls
        maxlen = maximum (map length ls)
        minlen = minimum (map length ls)
        flat = concat lsclean
    in if maxlen == minlen then Just (flat, maxlen) else Nothing

parseInput :: String -> Maybe (Graph, Vertex -> (Int, Int, [Int]), Int -> Maybe Vertex)
parseInput blob = case parseText blob of
    Just (vals, ncols) -> do
        let keys = [0..(length vals - 1)]
        adjLists <- buildEdgeList keys ncols
        let graphData = zip3 vals keys adjLists
        let g = graphFromEdges graphData
        return g
    _ -> Nothing

--takes the inspect, lookup functions and then the key for a node in the graph
getNodeVal :: (Vertex -> (Int, Int, [Int])) -> (Int -> Maybe Vertex) -> Int -> Maybe Int
getNodeVal inspect dict key = case dict key of 
    Just v -> Just node where
        (node, _, _) = inspect v
    _ -> Nothing

--we now need a function to inspect a vertices neighbors and determine if it's good
--we need both the inspect function, the lookup function  and the vertex key itself
isVertexGood :: (Vertex -> (Int, Int, [Int])) -> (Int -> Maybe Vertex) -> Int -> Maybe Bool
isVertexGood inspect dict key = case dict key of
    Just v -> Just isGood where
        (val, _, adjList) = inspect v
        isValidTarget = val == 1
        neighborVals = map (getNodeVal inspect dict) adjList
        --we drop the Nothing values bc there shouldn't be any!
        neighborTotal = sum $ catMaybes neighborVals
        isGood = isValidTarget && (neighborTotal < 4)
    _ -> Nothing

solvePuzzle :: String -> Maybe Int
solvePuzzle x = do
    (g, inspect, dict) <- parseInput x
    let vs = vertices g
    let goods = map (isVertexGood inspect dict) vs
    mGoods <- sequence goods
    let n = length $ filter id mGoods
    return n
