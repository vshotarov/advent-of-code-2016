module Main where

import qualified Common
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day21 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let startingStr = if length parsedInput < 15 then "abcde" else "abcdefgh"
    let answer1 = last $ scanl processInstruction startingStr parsedInput
    let answer2 = last . scanl processInstruction "fbgdceah" $ map reverseInstruction $ reverse parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Instruction = SwapPosition Int Int
                 | SwapLetter Char Char
                 | RotateLeft Int
                 | RotateRight Int
                 | RotateBasedOn Char
                 | ReverseRotateBasedOn Char
                 | ReverseSpan Int Int
                 | Move Int Int
                 deriving (Show)

parse :: String -> [Instruction]
parse input = map (parseInstruction . splitOn " ") $ lines input
    where parseInstruction ["swap","position",x,_,_,y] = SwapPosition (read x) (read y)
          parseInstruction ["swap","letter",x,_,_,y] = SwapLetter (head x) (head y)
          parseInstruction ["rotate","left",n,_] = RotateLeft (read n)
          parseInstruction ["rotate","right",n,_] = RotateRight (read n)
          parseInstruction ["rotate","based",_,_,_,_,x] = RotateBasedOn (head x)
          parseInstruction ["reverse",_,x,_,y] = ReverseSpan (read x) (read y)
          parseInstruction ["move",_,x,_,_,y] = Move (read x) (read y)
          parseInstruction x = error ("Unrecognised instruction " ++ concat x)

processInstruction :: String -> Instruction -> String
processInstruction xs (SwapPosition a b) =
    let a' = xs !! a
        b' = xs !! b
     in map (\(x,i) ->
            case () of _
                        | i == a -> b'
                        | i == b -> a'
                        | otherwise -> x)
      $ zip xs [0..]
processInstruction xs (SwapLetter a b) =
    map (\x -> case () of _
                           | x == a -> b
                           | x == b -> a
                           | otherwise -> x)
    xs
processInstruction str (RotateLeft n) = last . take (n+1) $ iterate rotateLeft str
processInstruction str (RotateRight n) = last . take (n+1) $ iterate rotateRight str
processInstruction str (RotateBasedOn a) =
    let i = Common.firstIdWhere (== a) str
        numRotations = 1 + i + (if i >= 4 then 1 else 0)
     in last . take (numRotations + 1) $ iterate rotateRight str
processInstruction str (ReverseRotateBasedOn a) =
    let i = Common.firstIdWhere (== a) str
        numRotations = case () of _
                                   | i == 0    -> 1
                                   | odd i     -> 1 + i `div` 2
                                   | otherwise -> 5 + i `div` 2
     in last . take (numRotations + 1) $ iterate rotateLeft str
processInstruction xs (ReverseSpan a b) = (take a xs)
                                       ++ (reverse . take (b-a+1) $ drop a xs)
                                       ++ (drop (b+1) xs)
processInstruction str (Move a b) = go str 0
    where go [] _ = []
          go (x:xs) i | i == a     = if a < b then go xs (i+1)
                                              else x:(go (drop 1 xs) (i+1))
          go (x:xs) i | i == b     = if b < a then (str !! a):(go (x:xs) (i+1))
                                              else x:(str !! a):(go xs (i+1))
          go (x:xs) i              = x:(go xs (i+1))

rotateLeft :: String -> String
rotateLeft [] = error "can't rotate empty string"
rotateLeft (x:xs) = xs ++ [x]

rotateRight :: String -> String
rotateRight [] = error "can't rotate empty string"
rotateRight xs = (last xs):(init xs)

reverseInstruction :: Instruction -> Instruction
reverseInstruction (SwapPosition a b) = SwapPosition b a
reverseInstruction i@(SwapLetter _ _) = i
reverseInstruction (RotateLeft n) = RotateRight n
reverseInstruction (RotateRight n) = RotateLeft n
reverseInstruction (RotateBasedOn n) = ReverseRotateBasedOn n
reverseInstruction i@(ReverseSpan _ _) = i
reverseInstruction (Move a b) = (Move b a)
reverseInstruction (ReverseRotateBasedOn _) = error "ReverseRotateBasedOn can't exist in input"
