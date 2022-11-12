module Main where

import qualified Common
import Data.Char (isDigit)

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = map (getLength Simple) parsedInput
    let answer2 = map (getLength Recursive) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = lines input

data Mode = Simple
          | Recursive

getLength :: Mode -> String -> Int
getLength _ [] = 0
getLength mode ('(':xs) =
    let (marker,xs') = Common.splitOnceOn ")" xs
        (numCharsStr,numRepeatsStr) = Common.splitOnceOn "x" marker
        (numChars,numRepeats) = (read numCharsStr, read numRepeatsStr) :: (Int,Int)
        xs'' = drop numChars xs'
        compressedData = take numChars xs'
     in case mode of
          Simple -> numRepeats * numChars + getLength mode xs''
          Recursive -> (numRepeats * getLength mode compressedData) + getLength mode xs''
getLength mode (x:xs) = 1 + getLength mode xs
