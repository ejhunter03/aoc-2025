module Day6Part1 (
    solvePuzzle,
    parseInput,
    postprocessInput,
    lookupAgg,
    lookupId
) where

import Data.Maybe
import Text.Read
import Data.List

--Is this code efficient? No. Why? because I didn't think optimizing it to use arrays would be interesting.

parseInput :: String -> Maybe ([[Int]], [String])
parseInput blob = let
        ls = lines blob --[String]
        instructions = map words ls --[[String]]
        ops = last instructions -- [String]
        sNums = init instructions -- [[String]]
        lmNums = map (map readMaybe) sNums :: [[Maybe Int]]
        mNums = sequence $ map sequence lmNums 
    in case mNums of 
        Just l -> Just (l, ops) 
        Nothing -> Nothing

postprocessInput :: ([[Int]], [String]) -> Maybe [(Int -> Int -> Int, Int, [Int])]
postprocessInput (ls, ops) = do
    mOps <- sequence $ map lookupAgg ops
    mIds <- sequence $ map lookupId ops
    let tLs = transpose ls
    let x = zip3 mOps mIds tLs
    return x

lookupAgg :: String -> Maybe (Int -> Int -> Int)
lookupAgg "*" = Just (\x y -> x*y)
lookupAgg "+" = Just (\x y -> x+y)
lookupAgg _  = Nothing

lookupId :: String -> Maybe Int
lookupId "*" = Just 1
lookupId "+" = Just 0
lookupId _ = Nothing

solvePuzzle :: String -> Maybe Int
solvePuzzle blob = do
    parsedInput <- parseInput blob
    cleanInput <- postprocessInput parsedInput
    let results = [foldl' op opId nums | (op, opId, nums) <- cleanInput]
    return $ sum results
