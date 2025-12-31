module Day7Part1 (
    parseInput,
    solvePuzzle,
    inputToArr,
    buildGrData
) where

import Data.Maybe
import qualified Data.HashSet as Set
import Debug.Trace (trace)
import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as FGL

type D7Gr = Gr Char Char
type GrBuildState = Set.HashSet Int
type Vector2DChar = V.Vector (UV.Vector Char)
type NodeData = (Int, Char)
type EdgeData = (Int, Int, Char)

solvePuzzle :: String -> Maybe Int
solvePuzzle blob = do
    g <- parseInput blob
    let gfiltered = FGL.labfilter (=='^') g
    return $ FGL.noNodes gfiltered

parseInput :: String -> Maybe D7Gr
parseInput x = do
    vec2d <- inputToArr x
    (lEdges, lNodes) <- buildGrData vec2d
    return $ FGL.mkGraph lNodes lEdges

buildGrData :: Vector2DChar -> Maybe ([EdgeData], [NodeData])
buildGrData vec = do
    vec0 <- vec V.!? 0
    startIdx <- UV.elemIndex 'S' vec0
    let (lEdgeData, lNodeData) = evalState (buildGrData' vec startIdx) Set.empty
    return (lEdgeData, lNodeData)

secEle :: (a,b,c) -> b
secEle (_,y,_) = y

-- the state is just a hashset, this works because each stateful comp returns the needed data
buildGrData' :: Vector2DChar -> Int -> State GrBuildState ([EdgeData], [NodeData])
buildGrData' vec key = do
    visited <- get
    if Set.member key visited
        then return ([],[])
        else do
            put (Set.insert key visited)

            case buildNodeData vec key of
                Just ([e0, e1], n) -> do
                    (edgesL, nodesL) <- buildGrData' vec (secEle e0)
                    (edgesR, nodesR) <- buildGrData' vec (secEle e1)
                    return (e0:e1:(edgesL ++ edgesR), n:(nodesL ++ nodesR))

                Just ([e], n) -> do
                    (edges, nodes) <- buildGrData' vec (secEle e)
                    return (e:edges, n:nodes)

                Just ([], n) -> return ([], [n])
                
                _ -> return([],[])

--we need to add a visited set to prevent recomputing everything
--I abandoned this because it was getting ugly and learned how to use a state monad to make this less terrible
--buildGrData' :: Vector2DChar -> [EdgeData] -> [NodeData] -> Int -> ([EdgeData], [NodeData])
--buildGrData' vec lEdges lNodes key = trace("Visiting node: " ++ show key) $ case buildNodeData vec key of
    --Just ([e0, e1], n) -> let
            --lNodes' = n:lNodes
            --lEdges' = e0:e1:lEdges
            --(lEdges'', lNodes'') = buildGrData' vec lEdges' lNodes' (secEle e0)
        --in buildGrData' vec lEdges'' lNodes'' (secEle e1)
    --Just ([e], n) -> let
            --lNodes' = n:lNodes
            --lEdges' = e:lEdges
        --in buildGrData' vec lEdges' lNodes' (secEle e)
    --Just ([], n) -> let
            --lNodes' = n:lNodes
        --in (lEdges, lNodes')
    --_ -> (lEdges, lNodes)

buildNodeData :: Vector2DChar -> Int -> Maybe ([EdgeData],NodeData)
buildNodeData vec2d key = do
    (x,y) <- keyToPos vec2d key
    ele <- vec2d `safeIdx` (x,y)
    targets <- case ele of
        'S' -> Just [(x+1,y)]
        '.' -> Just [(x+1,y)]
        '^' -> Just [(x,y-1), (x,y+1)]
        _ -> Nothing
    let edges = mapMaybe (mkEdge key) targets
    return (edges, (key, ele))
    where 
        mkEdge :: Int -> (Int, Int) -> Maybe EdgeData
        mkEdge fromId targetPos = do
            toId <- posToKey vec2d targetPos
            return (fromId, toId, '0')


--helper functions for dealing with the 2d array of char we have as input
safeIdx :: Vector2DChar -> (Int, Int) -> Maybe Char
safeIdx vec2d (x,y) = case vec2d V.!? x of
    Just subVec -> subVec UV.!? y
    Nothing -> Nothing

--first arg is (nrows, ncols) second arg is pos
--we do this funny business bc I can't be bothered w/ arrays today and I don't wanna index into a linked list a bunch
posToKey :: Vector2DChar -> (Int, Int) -> Maybe Int
posToKey vec (x,y) = case vec V.!? x of
    Just subVec -> case subVec UV.!? y of
        Just _ -> Just (offset*x + y)
        Nothing -> Nothing
        where offset = UV.length subVec
    Nothing -> Nothing

keyToPos :: Vector2DChar -> Int -> Maybe (Int, Int)
keyToPos vec key = do
    vec0 <- vec V.!? 0
    let ncols = UV.length vec0
    let x = key `div` ncols
    let y = key `mod` ncols
    mX <- vec V.!? x
    mY <- mX UV.!? y
    return (x,y)

inputToArr :: String -> Maybe Vector2DChar
inputToArr blob = let
        strs = lines blob
        ls = map length strs
        isRect = ((minimum ls) == (maximum ls))
        vec = V.fromList [UV.fromList line | line <- strs]
    in if isRect then Just vec else Nothing
