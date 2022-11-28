module Main where

import qualified Common
import Data.List (tails)

main :: IO ()
main = do
    putStrLn $ "-- Solving day18 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = solve 40 parsedInput
    let answer2 = solve 400000 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = input

getNextRow :: String -> String
getNextRow xs = map go
              . filter (\x -> case x of
                                _:_:_:_ -> True
                                _ -> False)
              $ tails ("." ++ xs ++ ".")
    where go (a:_:b:_)
            | a == b = '.'
            | otherwise = '^'

solve :: Int -> String -> Int
solve numRowsRequired xs = go numRowsRequired xs
    where go 0 _ = 0
          go numRows row = (sum $ map (\x -> if x == '.' then 1 else 0) row)
                         + go (numRows -1) (getNextRow row)
