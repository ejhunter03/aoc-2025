module Main (main) where

--just import the correct module and alter then puzzle-input str to solve the corresponding puzzle
import Day3Part2

main :: IO ()
main = do 
    contents <- readFile "puzzle-input/day3.txt"
    print $ solvePuzzle contents
