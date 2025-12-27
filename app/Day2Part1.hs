module Day2Part1 (
    solvePuzzle,
    parseInput,
    parsePair,
    genRange
) where

import Text.Read
import Data.List.Split (splitOn)
--import Data.Traversable (traverse)

parsePair :: String -> Maybe (Int, Int)
parsePair str = case break (\x -> (x == '-')) str of
    (part1, _:part2) -> ret where
        num1 = readMaybe part1 :: Maybe Int
        num2 = readMaybe part2 :: Maybe Int
        ret = case (num1, num2) of
            (Just a, Just b) -> Just (a,b)
            _ -> Nothing
    _ -> Nothing

isBad :: Int -> Bool
isBad x = let
        str = show x
        len = length str
        (firstHalf, secondHalf) = splitAt (len `div` 2) str
        isPallindrome = (firstHalf == secondHalf)
    in isPallindrome && (even len)

findBadsInPair :: (Int, Int) -> [Int]
findBadsInPair x = bads where
    range = genRange x
    bads = filter isBad range

parseInput :: String -> [Maybe (Int, Int)]
parseInput x = map parsePair (splitOn "," x)

genRange :: (Int,Int) -> [Int]
genRange (x,y) = [x..y]

solvePuzzle :: String -> Maybe Int
solvePuzzle x = ret where
    listOfTuples = parseInput x
    badsPerPair = traverse (fmap findBadsInPair) listOfTuples
    bads = (fmap concat) badsPerPair
    ret  = (fmap sum) bads
