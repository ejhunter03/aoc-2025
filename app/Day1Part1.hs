module Day1Part1 (
    parseText,
    solvePuzzle
) where

import Text.Read
import Data.Maybe

parseStr :: String -> Maybe Int
parseStr ('L':xs) = fmap (\x -> -1*x) (readMaybe xs :: Maybe Int)
parseStr ('R':xs) = (readMaybe xs :: Maybe Int)
parseStr _ = Nothing

parseText :: String -> [Maybe Int]
parseText blob =
    let x = lines blob
        y = map parseStr x
    in y

numZeros :: Int -> [Int] -> Int
numZeros start lst = numZeros' 0 start lst where
    numZeros' n total (x:xs) = 
        if (((total+x) `mod` 100) == 0) then numZeros' 
        (n+1) ((total+x) `mod` 100) xs else numZeros' n ((total+x) `mod` 100) xs
    numZeros' n _ [] = n

solvePuzzle :: String -> Maybe Int
solvePuzzle blob = 
    let parsedInput = parseText blob
        filteredInput = catMaybes parsedInput
        partNumZeros = numZeros 50
        totalZeros = partNumZeros filteredInput
        x = if (all isJust parsedInput) then Just totalZeros else Nothing
    in x
