module Main where

import qualified Common
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let registers = M.fromList . zip "abcd" $ repeat 0
    let answer1 = process parsedInput (registers,0)
    let answer2 = process parsedInput (M.insert 'c' 1 registers,0)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = (M.Map Char Int, Int)
data Instruction = CopyVal Int Char
                 | CopyReg Char Char
                 | Inc Char
                 | Dec Char
                 | JnzVal Int Int
                 | JnzReg Char Int
                 deriving (Show)

toInstruction :: String -> Instruction
toInstruction ('c':'p':'y':' ':xs) =
    if (head xs) `elem` "abcd"
       then CopyReg (head xs) (last xs)
       else let (valAsStr,register) = Common.splitOnceOn " " xs
             in CopyVal (read valAsStr) (head register)
toInstruction ('i':'n':'c':' ':register:[]) = Inc register
toInstruction ('d':'e':'c':' ':register:[]) = Dec register
toInstruction ('j':'n':'z':' ':xs) =
    if (head xs) `elem` "abcd"
       then JnzReg (head xs) (read $ drop 2 xs)
       else let (valAsStr,ofsAsStr) = Common.splitOnceOn " " xs
             in JnzVal (read valAsStr) (read ofsAsStr)

parse input = map toInstruction $ lines input

processInstruction :: Instruction -> State -> State
processInstruction (CopyVal v reg) (registers,cursor) =
    (M.insert reg v registers, cursor+1)
processInstruction (CopyReg from to) (registers,cursor) =
    (M.insert to (registers M.! from) registers,cursor+1)
processInstruction (Inc reg) (registers,cursor) =
    (M.insertWith (+) reg 1 registers, cursor+1)
processInstruction (Dec reg) (registers,cursor) =
    (M.insertWith (\a b -> b - a) reg 1 registers, cursor+1)
processInstruction (JnzVal val ofs) (registers,cursor) =
    if val == 0
       then (registers,cursor+1)
       else (registers,cursor+ofs)
processInstruction (JnzReg reg ofs) (registers,cursor) =
    if registers M.! reg == 0
       then (registers,cursor+1)
       else (registers,cursor+ofs)

process :: [Instruction] -> State -> State
process instructions state@(registers,cursor)
  | cursor >= length instructions = state
  | otherwise = process instructions
              $ processInstruction (instructions !! cursor) state
