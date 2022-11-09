module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ filter isValidTriangle parsedInput
    let answer2 = length . filter isValidTriangle
                . columnsToTriangles
                . flatten
                $ parsedInput
                    where columnsToTriangles [] = []
                          columnsToTriangles xs =
                            (take 3 xs):(columnsToTriangles $ drop 3 xs)
                          flatten xs = firsts ++ seconds ++ thirds
                              where (firsts,seconds,thirds) = foldr
                                      (\(a:b:c:[]) (as,bs,cs) -> (a:as,b:bs,c:cs))
                                      ([],[],[])
                                      xs

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = map
                (map read
                . filter (\x -> length x > 0)
                . splitOn " " . trim)
              $ lines input

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

isValidTriangle (a:b:c:[]) =
    a + b > c
    && a + c > b
    && b + c > a
