module Main where

import qualified Common
import qualified Data.Set as S
import qualified Data.Map as M


main :: IO ()
main = do
    putStrLn $ "-- Solving day24 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let goals = [(x,y) | ((x,y),c) <- M.toList parsedInput,
                         c /= '#' && c/= '.']
    let start = head $ filter ((=='0') . (parsedInput M.!)) goals
    let (answer1,answer2) = search start parsedInput $ S.fromList goals

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)
type Grid = M.Map Point Char
type State = (Point,S.Set Point,Int,Bool)

parse :: String -> Grid
parse input = M.fromList . concat
            . map (\(y,row) -> map (\(x,c) -> ((x,y),c)) $ zip [0..] row)
            . zip [0..] $ lines input

search :: Point -> Grid -> S.Set Point -> (Int,Int)
search start grid goals = bfs [(start,goals,0,False)] 10000 10000 S.empty
    where bfs :: [State] -> Int -> Int -> S.Set (Point, S.Set Point, Bool) -> (Int,Int)
          bfs [] best1 best2 _ = (best1,best2)
          bfs ((p@(x,y),gs,steps,returning):explore) best1 best2 visited
            | S.null gs &&     returning = bfs explore best1 (min best2 (steps-1)) visited
            | S.null gs && not returning = bfs ((p,S.fromList [start],steps,True):explore) (min best1 (steps-1)) best2 visited
            | (p,gs,returning) `S.member` visited = bfs explore best1 best2 visited
            | steps >= best2 && returning = bfs explore best1 best2 visited
            | otherwise = let visited' = S.insert (p,gs,returning) visited
                              ns = [p' | p' <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                                         case M.lookup p' grid of
                                           Just '#' -> False
                                           Nothing -> False
                                           _ -> True]
                              gs' = S.difference gs $ S.fromList [p]
                              ns' = map (\p' -> (p',gs',steps+1,returning)) ns
                              explore' = explore ++ ns'
                           in bfs explore' best1 best2 visited'
