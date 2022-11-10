module Main where

import qualified Common
import Data.List (transpose, sortBy)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve comparator =
            map
                ( fst . head
                . sortBy comparator
                . M.assocs
                . foldr
                      (\x -> M.insertWith (+) x 1)
                  M.empty)
          . transpose 
    let answer1 = solve (\(_,v1) (_,v2) -> compare v2 v1) parsedInput
    let answer2 = solve (\(_,v1) (_,v2) -> compare v1 v2) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = lines input
