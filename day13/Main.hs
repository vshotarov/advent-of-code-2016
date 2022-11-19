module Main where

import qualified Common
import qualified Data.Set as S

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day13 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let target = if parsedInput == 10 then (7,4) else (31,39)
    let isWall (x,y) = odd . numOnesInBinary $ parsedInput + (x*x + 3*x + 2*x*y + y + y*y)
    let isValid p@(x,y) = (not $ isWall p)
                       && (x >=0 && y >= 0)
                       && (x < 100 && y < 100)
    let answer1 = bfs target isValid
    let answer2 = fill isValid

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = read input :: Int

numOnesInBinary :: Int -> Int
numOnesInBinary num = go num 15
    where go x 0 = 0
          go x y
            | odd x = 1 + go (x-1) y
            | otherwise = case x `div` (2 ^ y) of
                            0 -> go x (y-1)
                            _ -> 1 + go (x - 2 ^ y) (y-1)

type Point = (Int,Int)
bfs :: Point -> (Point -> Bool) -> Int
bfs (targetX,targetY) isValid = go [((1,1),0)] S.empty 100000
    where go :: [(Point,Int)] -> S.Set (Point,Int) -> Int -> Int
          go [] _ bestSolution = bestSolution
          go ((p@(x,y),s):exploreList) visited bestSolution =
              let neighbours = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                  validNeighbours = filter isValid neighbours
                  newExploreList = foldr
                                     (\p' acc -> acc ++ [(p',s+1)])
                                   exploreList
                                   validNeighbours
               in if S.member (p,s) visited || s > bestSolution
                     then go exploreList visited bestSolution
                     else if (x,y) == (targetX,targetY)
                             then go newExploreList visited (min bestSolution s)
                             else go newExploreList (S.insert (p,s) visited) bestSolution

fill :: (Point -> Bool) -> Int
fill isValid = go [((1,1),0)] S.empty
    where go :: [(Point,Int)] -> S.Set Point -> Int
          go [] visited = S.size visited
          go ((p@(x,y),s):exploreList) visited =
              let neighbours = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                  validNeighbours = filter isValid neighbours
                  newExploreList = foldr
                                     (\p' acc -> acc ++ [(p',s+1)])
                                   exploreList
                                   validNeighbours
               in if S.member p visited || s > 50
                     then go exploreList visited
                     else go newExploreList $ S.insert p visited
