module Day5Part1 (
    solvePuzzle,
    parseInput,
    parseInput',
    solvePuzzle'
) where

import qualified Data.HashSet as HashSet
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Merge as MergeSort
import Data.Maybe
import Text.Read
import Data.List.Split
import Control.Monad.ST
import Day2Part1 (parsePair)

customOrder :: (Int, Int) -> (Int, Int) -> Ordering
customOrder (start1, stop1) (start2,stop2) 
    | (start1 < start2) = LT
    | (start1 > start2) = GT
    | otherwise = EQ

parseToHashSet :: String -> Maybe (HashSet.HashSet Int)
parseToHashSet blob = let
        ls = lines blob
        mPairs = map parsePair ls
        pairs = catMaybes mPairs
        set = foldl' (\acc (x,y) -> foldl' (flip HashSet.insert) acc [x..y]) HashSet.empty pairs
    in if all isJust mPairs then Just set else Nothing

sortVecByLB :: V.Vector (Int, Int) -> V.Vector (Int, Int)
sortVecByLB vec = runST $ do
    mvec <- V.thaw vec
    MergeSort.sortBy customOrder mvec
    V.freeze mvec

--Note we only ever have to compare adjacent positions
--we return the stateful context and then the length of the vector (we will slice after the fact)
--this is a read write pointer implementation EXACTLY like what would be done in C
cleanSearchVec' :: (MV.PrimMonad m) => MV.MVector (MV.PrimState m) (Int,Int) -> m Int
cleanSearchVec' mvec = do
    let len = MV.length mvec
    if len < 2
        then return len
        else do
            compact 0 1 where 
                compact writeIdx readIdx
                    | readIdx >= MV.length mvec = return (writeIdx+1) --fixed some weird off by one error
                    | otherwise = do
                        (currLB, currUB) <- MV.read mvec writeIdx
                        (nextLB, nextUB) <- MV.read mvec readIdx
                        if currUB > nextLB
                            then do
                                let newUB = max currUB nextUB
                                let newLB = currLB
                                MV.write mvec writeIdx (newLB, newUB)
                                compact writeIdx (readIdx+1)
                        --we should probably add a check for writeIdx + 1 = readIdx but I'm lazy
                        else do 
                            MV.write mvec (writeIdx+1) (nextLB, nextUB)
                            compact (writeIdx+1) (readIdx+1)

cleanSearchVec :: V.Vector (Int, Int) -> V.Vector (Int, Int)
cleanSearchVec vec = runST $ do
    mvec <- V.thaw vec
    newLen <- cleanSearchVec' mvec
    let mvec' = MV.take newLen mvec
    V.freeze mvec'

parseToSearchVec :: String -> Maybe (V.Vector (Int, Int))
parseToSearchVec blob = let
        ls = lines blob
        mPairs = map parsePair ls
        pairs = catMaybes mPairs
        vecPairs = V.fromList pairs
        orderedPairs = sortVecByLB vecPairs
        searchVec = cleanSearchVec orderedPairs
    in if all isJust mPairs then Just searchVec else Nothing

parseToVec :: String -> Maybe (VU.Vector Int)
parseToVec blob =  let
        ls = lines blob
        mNums = map readMaybe ls :: [Maybe Int]
        nums = catMaybes mNums
        vec = VU.fromList nums
    in if all isJust mNums then Just vec else Nothing

parseInput' :: String -> Maybe (HashSet.HashSet Int, VU.Vector Int)
parseInput' blob = case splitOn "\n\n"  blob of
    [rules, vals] -> y where
        setOfRules = parseToHashSet rules
        vecNums = parseToVec vals
        y = case (setOfRules, vecNums) of
            (Just rs, Just ns) -> Just (rs, ns)
            _ -> Nothing
    _ -> Nothing

parseInput :: String -> Maybe (V.Vector (Int, Int), VU.Vector Int)
parseInput blob = case splitOn "\n\n" blob of
    [rules, vals] -> y where
        setOfRules = parseToSearchVec rules
        vecNums = parseToVec vals
        y = case (setOfRules, vecNums) of
            (Just rs, Just ns) -> Just (rs, ns)
            _ -> Nothing
    _ -> Nothing


--This hashset solve is slow and runs OOM! WOULD have been optimal if ranges were small (they aren't)
solvePuzzle' :: String -> Maybe Int
solvePuzzle' x = case parseInput' x of
    Just (set, vec) -> Just (VU.length $ setSolveAlgo set vec)
    _ -> Nothing

solvePuzzle :: String -> Maybe Int
solvePuzzle x = case parseInput x of
    Just (rangeVec, vec) -> Just (VU.length $ searchSolveAlgo rangeVec vec)
    _ -> Nothing


binSearchRangeVec :: V.Vector (Int, Int) -> Int -> Bool
binSearchRangeVec vec num =
    case vec V.!? idx of
        Just (lb, ub)
            | (lb <= num) && (num <= ub) -> True
            | len <= 1 -> False
            | num < lb -> binSearchRangeVec (V.slice 0 idx vec) num
            | ub < num -> binSearchRangeVec (V.slice (idx+1) (len - (idx+1)) vec) num
        Nothing -> False
    where
        len = V.length vec
        idx = len `div` 2

searchSolveAlgo :: V.Vector (Int,Int) -> VU.Vector Int -> VU.Vector Int
searchSolveAlgo rangeVec numVec = VU.filter (\x -> binSearchRangeVec rangeVec x) numVec


setSolveAlgo :: HashSet.HashSet Int -> VU.Vector Int -> VU.Vector Int
setSolveAlgo set vec = VU.filter (\x -> HashSet.member x set) vec
