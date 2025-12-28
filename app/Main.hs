module Main (main) where

--just import the correct module and alter then puzzle-input str to solve the corresponding puzzle
import Day6Part2

main :: IO ()
main = do 
    contents <- readFile "puzzle-input/day6.txt"
    print $ parseInput contents
    print $ solvePuzzle contents
