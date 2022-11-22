module Main where

import qualified Common
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day15 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let processDisc time (discId,numPositions,startingPosition) =
            (time + startingPosition + discId) `mod` numPositions
    let (maxDiskId,maxDiskNumPos,maxDiskStartPos) = getDiskWithMaxPeriod parsedInput
    let firstTimeDiskWithMaxPeriodAtPos0 = maxDiskNumPos - (maxDiskId + maxDiskStartPos)

    let solve discs = Common.firstWhere
                        (\x -> (sum $ map (processDisc x) discs) == 0)
                    $ [firstTimeDiskWithMaxPeriodAtPos0,(firstTimeDiskWithMaxPeriodAtPos0+maxDiskNumPos)..]

    let answer1 = solve parsedInput
    let answer2 = solve (parsedInput ++ [(length parsedInput + 1, 11, 0)])

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Disc = (Int,Int,Int)
parse :: String -> [Disc]
parse input = map parseOne $ lines input
    where parseOne line =
            let (_:hashNum:_:numPositions:_:_:_:_:_:_:_:position:[]) =
                    splitOn " " $ init line
             in (read $ tail hashNum,read numPositions,read position)

getDiskWithMaxPeriod :: [Disc] -> Disc
getDiskWithMaxPeriod [d@(_,period,_)] = d
getDiskWithMaxPeriod (d@(_,period,_):discs) = maxPeriod d $ getDiskWithMaxPeriod discs
    where maxPeriod a@(_,periodA,_) b@(_,periodB,_) =
            if periodA > periodB then a else b
