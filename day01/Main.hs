module Main where

import Data.List.Split (splitOn)
import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day01 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let turn 'R' (x,y) = (y,-x)
        turn 'L' (x,y) = (-y,x)
        turn _ _ = error "Unrecognized side in turn"
    let step (d,((x,y):ps)) (t:s) = (d',ps')
            where steps = read s :: Int
                  d'@(dx',dy') = turn t d
                  (x',y') = (x+dx'*steps,y+dy'*steps)
                  ps' = (reverse . tail $ zip [x,(x+dx')..x'] [y,(y+dy')..y']) ++ (x,y):ps
        step _ instruction = error $ "Malformatted instruction " ++ show instruction
    let manhattan (x,y) = abs x + abs y
    let allPositions = reverse . snd $ foldl step ((0,1),[(0,0)]) parsedInput
    let answer1 = manhattan $ last allPositions
    let answer2 = manhattan . fst $ Common.firstWhere
                                      (\(x,i) -> x `elem` (take i allPositions))
                                      $ zip allPositions [0..]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [String]
parse input = splitOn ", " input
