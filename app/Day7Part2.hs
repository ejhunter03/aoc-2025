module Day7Part2 (
    solvePuzzle
) where

import qualified Day7Part1 as D7P1
import qualified Data.IntMap.Strict as IntMap
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as FGL

type D7Gr = Gr Char Char
type GrAccState = IntMap.IntMap Int
--in hindsigh fgl was way overkill and prob makes this slow af

--node id
--this function can crash and I'm a bit lazy to do some state transformer lifting business
solvePuzzle' :: D7Gr -> Int -> State GrAccState Int
solvePuzzle' g nodeId = do
    visited <- get 
    case IntMap.lookup nodeId visited of 
        Just x -> return x
        Nothing -> do
            let lSuc = FGL.suc g nodeId
            result <- if null lSuc
                then return 1
                else do
                    sums <- mapM (solvePuzzle' g) lSuc
                    let s = sum sums
                    return s
            modify (IntMap.insert nodeId result)
            return result


solvePuzzle :: String -> Maybe Int
solvePuzzle blob = do
    g <- D7P1.parseInput blob
    let start = FGL.labfilter (=='S') g
    let ns = FGL.nodes start
    guard (length ns < 2)
    startNode <- listToMaybe ns
    let result = evalState (solvePuzzle' g startNode) IntMap.empty
    return result


