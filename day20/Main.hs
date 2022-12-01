module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day20 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solutions = solve $ Common.sortByFst parsedInput
    let answer1 = head solutions
    let answer2 = length solutions

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [(Int,Int)]
parse input = map readOne $ lines input
    where readOne x = let (low,high) = Common.splitOnceOn "-" x
                       in (read low, read high)

solve :: [(Int,Int)] -> [Int]
solve ranges = go 0 ranges
    where go n [] = [n..4294967295]
          go n ((low,high):xs)
            | n < low = [n..(low-1)] ++ (go (high+1) xs)
            | otherwise = go (max n (high+1)) xs
