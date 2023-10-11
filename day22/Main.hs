module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day22 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $
                  [(node1,node2) | node1@(p1,used1,_) <- parsedInput,
                                   node2@(p2,_,avail2) <- parsedInput,
                                   p1 /= p2 && used1 /= 0 && used1 <= avail2]
    -- Judging by the example given, it seems like the empty disk plays
    -- a crucial role, so I thought to check if I have only one of those
    -- as well, which I did.
    let gridSize = (38,26) :: (Int, Int)
    putStrLn $ "Part 2: \n" ++ drawGrid parsedInput gridSize
    -- Then looking at the drawn grid I immediately thought I would be able to
    -- always shuffle the empty disk in front of the disk that I want to move,
    -- but of course if there is a way to move that disk without the empty,
    -- then using the empty will be taking more steps than necessary.
    --
    -- So I changed the input to NOT have that empty disk and I noticed the
    -- first part returns 0 possible moves, meaning we can only ever move any
    -- disk onto the empty disk, so I can just trace the path from the empty to
    -- my target and then the path from my target data to the exit node, the
    -- steps of which get multiplied by 5, since shuffling the empty in front
    -- of the target 4 steps.
    --
    -- The last piece of the puzzle that was also mentioned in the example is
    -- the fact that some nodes are huge, so they can't move anywhere and can
    -- effectively be treated as walls. Taking those into account when tracing
    -- the path, gives us the final answer:
    --
    -- + from empty to just west of the target - (16,23) to (37,0) = 65 steps,
    --   as we have to walk around a bunch of walls
    -- + from there the next move moves the target into that space occupied by
    --   the empty and the empty is now east of of the target. We need 4 moves
    --   to put the empty west of the target again, giving us a total of 5 steps
    --   to move the target one node west. Since we start at (37,0) with the
    --   empty at (36,0) i.e. just west, then after 36 * 5 moves, the target will
    --   be at (1,0) and the empty at (0,0), so to our 65 steps from before we add
    --   (36 * 5 + 1) = 181, arriving at 65 + 181 = 246

    let answer2 = 246 :: Int

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Node = ((Int,Int), Int, Int)

parse :: String -> [Node]
parse input = map parseOne $ drop 2 $ lines input
    where parseOne xs =
            let (name:_:used:avail:_) = filter ((>0) . length) $ splitOn " " xs
                (x:y:_) = map (read . drop 1) $ drop 1 $ splitOn "-" name
                readSize = read . init
             in ((x,y),readSize used, readSize avail) :: Node


drawGrid :: [Node] -> (Int,Int) -> String
drawGrid grid (_,sizeY) = concat . map (\line -> line ++ "\n") . transpose
                        . chunk sizeY $ map mapOne grid
    where mapOne (_,u,a)
            | u+a > 500 = '#'
            | u == 0    = '_'
            | otherwise = '.'
          chunk n xs
            | length xs == 0 = []
            | otherwise      = (take n xs):(chunk n (drop n xs))
