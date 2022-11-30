module Main where

import qualified Common
import qualified Data.Sequence as Seq

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day19 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = step parsedInput [1..parsedInput]
    let answer2 = solve2 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = read input :: Int

step _ [] = []
step _ [x] = [x]
step n (x:y:xs)
    | odd n     = step (n `div` 2) $ everyOther xs
    | otherwise = step (n `div` 2) $ x:(everyOther xs)

everyOther [] = []
everyOther [x] = [x]
everyOther (x:y:xs) = x:(everyOther xs)

solve2 n = let left = Seq.fromList $ filter (<= ((n+1) `div` 2)) [1..n]
               right = Seq.fromList $ filter (> ((n+1) `div` 2)) [1..n]
               rotate (l,r)
                 | Seq.null l || Seq.null r = (l,r)
                 | otherwise = let (l' Seq.:< ls) = Seq.viewl l
                                   (r' Seq.:< rs) = Seq.viewl r
                                in ((ls Seq.|> r'),(rs Seq.|> l'))
               step i (l,r)
                 | i == 1    = if Seq.length l > 0
                                  then Seq.index l 0
                                  else Seq.index r 0
                 | odd i     = let (ls Seq.:> l') = Seq.viewr l
                                in step (i-1) $ rotate (ls, r)
                 | otherwise = step (i-1) $ rotate (l, (Seq.drop 1 r))
            in step n (left,right)
