module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    putStrLn $ "Input: \n" ++ input

    solution <- readFile "day11/SOLUTION_ON_PAPER.txt"

    -- Print answers
    putStrLn $ "Solution: " ++  solution
