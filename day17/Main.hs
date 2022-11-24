module Main where

import qualified Common
import qualified Data.Set as S
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Data.List (sortBy)

main :: IO ()
main = do
    putStrLn $ "-- Solving day17 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solutions = sortBy (\a b -> compare (length a) (length b))
                  $ bfs (3,3) ((0,0),parsedInput)
    let answer1 = head solutions
    let answer2 = length $ last solutions

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = input

type Point = (Int,Int)
type State = (Point,String)

md5' :: String -> String
md5' = show . md5 . pack

isWall :: Point -> Bool
isWall (x,y) = x < 0 || y < 0 || x >= 4 || y >= 4

getMoves :: State -> [State]
getMoves ((x,y),passcode) =
    let hash = md5' passcode
     in map fst
      . filter
          (\((p,pass'),i) -> (not $ isWall p) && (hash !! i) `elem` "bcdef")
      $ zip
          [((x+1,y),passcode ++ "R"),
           ((x-1,y),passcode ++ "L"),
           ((x,y+1),passcode ++ "D"),
           ((x,y-1),passcode ++ "U")]
          [3,2..0]

bfs :: Point -> State -> [String]
bfs target startingState@(_,startPasscode) = go [startingState] S.empty []
    where go [] _ solutions = solutions
          go ((current@(p@(x,y),passcode)):exploreList) visited solutions
            | target == p = go exploreList visited
                          ((drop (length startPasscode) passcode):solutions)
            | S.member current visited = go exploreList visited solutions
            | otherwise = go
                            (exploreList ++ getMoves current)
                            (S.insert current visited)
                            solutions
