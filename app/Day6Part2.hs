module Day6Part2 (
    solvePuzzle,
    parseInput
) where

--wow do I find this problem annoying
--can't even recycle the parser from part 1

import Text.Read
import Data.Char
import Data.List
import Data.List.Split
import qualified Day6Part1 as D6P1

parseInput :: String -> Maybe([[Int]], [String])
parseInput blob = let
        ls = lines blob
        ops = words $ last ls
        sNums = init ls
        sNums' = transpose sNums
        sNums'' = splitWhen (all isSpace) sNums'
        lmNums = map (map readMaybe) sNums'' :: [[Maybe Int]]
        mNums = mapM sequence lmNums
    in case mNums of
        Just listOfNums -> Just (listOfNums, ops)
        Nothing -> Nothing

postprocessInput :: ([[Int]], [String]) -> Maybe [(Int -> Int -> Int, Int, [Int])]
postprocessInput (ls, ops) = do
    mOps <- mapM D6P1.lookupAgg ops
    mIds <- mapM D6P1.lookupId ops
    let x = zip3 mOps mIds ls
    return x

--input post processing fails due to the transposition
solvePuzzle :: String -> Maybe Int
solvePuzzle blob = do
    parsedInput <- parseInput blob
    cleanInput <- postprocessInput parsedInput
    let results = [foldl' op opId nums | (op, opId, nums) <- cleanInput]
    return $ sum results
