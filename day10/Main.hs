module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)
    let (v1,v2) = if length parsedInput > 10
                     then (61,17)
                     else (2,5)

    -- Solve
    let afterInputs = 
            foldr foldF M.empty $ filter isInputInstruction parsedInput
                where isInputInstruction (Input _ _) = True
                      isInputInstruction _ = False
                      foldF (Input val bin) acc = M.insertWith (++) bin [val] acc
    let process exitEarly state
            | M.null state = error "reached empty state"
            | otherwise    =
                let stateToReadyBots state
                            | M.null state = error "reached empty state"
                            | otherwise    =
                                filter (\(bin,values) ->
                                    case bin of
                                      Bot _    -> length values > 1
                                      Output _ -> False) $ M.toList state
                    readyBots = stateToReadyBots state
                    done = filter
                            (\(Bot x,vals) -> vals == [v1,v2] || vals == [v2,v1])
                           readyBots
                    processOne ((Bot i),[v1,v2]) state =
                       let (CompareInto bot low high) = head
                                       $ filter (\x -> case x of
                                                         Input _ _ -> False
                                                         CompareInto (Bot j) _ _ -> i == j)
                                       parsedInput 
                           updatedLow = M.insertWith (++) low [(min v1 v2)] state
                           updatedHigh = M.insertWith (++) high [(max v1 v2)] updatedLow 
                        in M.delete (Bot i) updatedHigh
                 in if exitEarly && length done == 1
                       then M.fromList done
                       else if length readyBots > 0
                               then process exitEarly $ processOne (head readyBots) state
                               else state

    let answer1 = process True afterInputs
    let answer2 = M.foldrWithKey
                    (\x v acc -> case x of
                                 Output i -> if i < 3
                                                then acc * (head v)
                                                else acc
                                 _        -> acc)
                    1
                $ process False afterInputs

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Bin = Bot Int
         | Output Int
         deriving (Show, Eq, Ord)

data Instruction = Input Int Bin
                 | CompareInto Bin Bin Bin
                 deriving (Show)

parse :: String -> [Instruction]
parse input = map (parseOne . splitOn " ") $ lines input
    where parseOne ("value":value:_:_:_:bot:[]) = Input (read value) (Bot (read bot))
          parseOne ("bot":bot:_:_:_:"output":low:_:_:_:"output":high:[]) =
              CompareInto (Bot (read bot)) (Output (read low)) (Output (read high))
          parseOne ("bot":bot:_:_:_:"bot":low:_:_:_:"output":high:[]) =
              CompareInto (Bot (read bot)) (Bot (read low)) (Output (read high))
          parseOne ("bot":bot:_:_:_:"bot":low:_:_:_:"bot":high:[]) =
              CompareInto (Bot (read bot)) (Bot (read low)) (Bot (read high))
          parseOne ("bot":bot:_:_:_:"output":low:_:_:_:"bot":high:[]) =
              CompareInto (Bot (read bot)) (Output (read low)) (Bot (read high))
