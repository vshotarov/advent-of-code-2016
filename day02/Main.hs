module Main where

import qualified Common
import Data.Char (intToDigit)

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = reverse
                . map button
                . init
                . foldl
                    (\acc line -> (foldl move (head acc) line):acc)
                    [(1,1)]
                $ parsedInput
    let answer2 = reverse
                . map button2
                . init
                . foldl
                    (\acc line -> (foldl move2 (head acc) line):acc)
                    [5]
                $ parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = lines input

move (x,y) 'R' = (max 0 (min 2 (x+1)), y)
move (x,y) 'L' = (max 0 (min 2 (x-1)), y)
move (x,y) 'U' = (x, max 0 (min 2 (y-1)))
move (x,y) 'D' = (x, max 0 (min 2 (y+1)))

move2 :: Int -> Char -> Int
move2 x 'R' | x `elem` [1,4,9,12,13] = x
move2 x 'L' | x `elem` [1,2,5,10,13] = x
move2 x 'U' | x `elem` [1,2,4,5,9] = x
move2 x 'D' | x `elem` [13,10,12,5,9] = x
move2 x 'R' = x+1
move2 x 'L' = x-1
move2 x 'U'
  | (x == 3 || x == 13) = x-2
  | otherwise           = x-4
move2 x 'D'
  | (x == 1 || x == 11) = x+2
  | otherwise           = x+4

button (x,y) = x + 1 + y * 3

button2 :: Int -> Char 
button2 10 = 'A'
button2 11 = 'B'
button2 12 = 'C'
button2 13 = 'D'
button2 x = intToDigit x
