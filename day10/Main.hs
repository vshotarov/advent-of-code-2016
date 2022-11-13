module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(inputInstructions,compareInstructions) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)
    let (v1,v2) = if length parsedInput > 10
                     then (61,17)
                     else (2,5)

    -- Solve
    let state = foldr foldF M.empty inputInstructions
                  where foldF (Input val bin) acc = M.insertWith (++) bin [val] acc
    let answer1 = process compareInstructions (Just (v1,v2)) state
    let answer2 = foldr (\x acc -> acc * (head $ state' M.! (Output x))) 1 [0,1,2]
                    where state' = process compareInstructions Nothing state

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Bin = Bot Int
         | Output Int
         deriving (Show, Eq, Ord)

data Instruction = Input Int Bin
                 | CompareInto Bin Bin Bin
                 deriving (Show)

parse :: String -> ([Instruction],[Instruction])
parse input = foldr
                (\x (inputs,compares) ->
                    case x of
                      Input _ _ -> (x:inputs,compares)
                      _         -> (inputs,x:compares))
                ([],[])
              $ map (parseOne . splitOn " ") $ lines input
    where parseOne ("value":value:_:_:_:bot:[]) = Input (read value) (Bot (read bot))
          parseOne ("bot":bot:_:_:_:lowBinType:low:_:_:_:highBinType:high:[]) =
              CompareInto (Bot (read bot))
                          ((if lowBinType == "bot" then Bot else Output) (read low))
                          ((if highBinType == "bot" then Bot else Output) (read high))

process instructions exitValues state
    | M.null state = error "reached empty state"
    | otherwise    =
         if (not $ isNothing exitValues) && length done == 1
            then M.fromList done
            else case readyBots of
                   [] -> state
                   _  -> process
                           instructions
                           exitValues
                         $ processOne (head readyBots) state
        where (v1,v2) = fromJust exitValues
              stateToReadyBots state
                    | M.null state = error "reached empty state"
                    | otherwise    =
                        filter (\(bin,values) ->
                            case bin of
                              Bot _    -> length values > 1
                              Output _ -> False) $ M.toList state
              readyBots = stateToReadyBots state
              done = case exitValues of
                       Nothing -> []
                       _ -> filter
                              (\(Bot x,vals) -> vals == [v1,v2] || vals == [v2,v1])
                            readyBots
              processOne ((Bot i),[a,b]) state =
                 let (CompareInto bot low high) = head
                                 $ filter (\x -> case x of
                                                   Input _ _ -> False
                                                   CompareInto (Bot j) _ _ -> i == j)
                                 instructions 
                     updatedLow = M.insertWith (++) low [(min a b)] state
                     updatedHigh = M.insertWith (++) high [(max a b)] updatedLow 
                  in M.delete (Bot i) updatedHigh
