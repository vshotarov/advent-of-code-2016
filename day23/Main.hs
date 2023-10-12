module Main where

import qualified Common
import qualified Data.Set as S
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let registers = M.insert 'a' 7 . M.fromList . zip "abcd" $ repeat 0
    let answer1 = (process parsedInput (registers,0,S.empty)) M.! 'a'
    let answer2 = (process parsedInput ((M.insert 'a' 12 registers),0,S.empty)) M.! 'a'

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = (M.Map Char Int, Int, S.Set Int)
data Instruction = CopyVal Int Char
                 | CopyReg Char Char
                 | Inc Char
                 | Dec Char
                 | JnzVal Int Int
                 | JnzReg Char Int
                 | JnzReg2 Int Char
                 | Tgl Char
                 deriving (Show)

toInstruction :: String -> Instruction
toInstruction ('c':'p':'y':' ':xs) =
    if (head xs) `elem` "abcd"
       then CopyReg (head xs) (last xs)
       else let (valAsStr,register) = Common.splitOnceOn " " xs
             in CopyVal (read valAsStr) (head register)
toInstruction ('i':'n':'c':' ':register:[]) = Inc register
toInstruction ('d':'e':'c':' ':register:[]) = Dec register
toInstruction ('t':'g':'l':' ':register:[]) = Tgl register
toInstruction ('j':'n':'z':' ':xs) =
    if (head xs) `elem` "abcd"
       then JnzReg (head xs) (read $ drop 2 xs)
       else let (valAsStr,ofsAsStr) = Common.splitOnceOn " " xs
             in if (head ofsAsStr) `elem` "abcd"
                   then JnzReg2 (read valAsStr) (head ofsAsStr)
                   else JnzVal (read valAsStr) (read ofsAsStr)

parse input = map toInstruction $ lines input

processInstruction :: Instruction -> State -> State
processInstruction (CopyVal v reg) (registers,cursor,toggled)
  | S.member cursor toggled =
    if v == 0
       then (registers,cursor+1,toggled)
       else (registers,cursor+(registers M.! reg),toggled)
  | otherwise = (M.insert reg v registers, cursor+1, toggled)
processInstruction (CopyReg from to) (registers,cursor,toggled)
  | S.member cursor toggled =
    if registers M.! from == 0
       then (registers,cursor+1,toggled)
       else (registers,cursor+(registers M.! to),toggled)
  | otherwise = (M.insert to (registers M.! from) registers,cursor+1,toggled)
processInstruction (Inc reg) (registers,cursor,toggled)
  | S.member cursor toggled =
      (M.insertWith (\a b -> b - a) reg 1 registers, cursor+1,toggled)
  | otherwise = (M.insertWith (+) reg 1 registers, cursor+1,toggled)
processInstruction (Dec reg) (registers,cursor,toggled)
  | S.member cursor toggled = 
      (M.insertWith (+) reg 1 registers, cursor+1,toggled)
  | otherwise = (M.insertWith (\a b -> b - a) reg 1 registers, cursor+1,toggled)
processInstruction (Tgl reg) (registers,cursor,toggled)
  | S.member cursor toggled = 
      (M.insertWith (+) reg 1 registers, cursor+1,toggled)
  | otherwise = (registers, cursor+1, (S.insert (cursor + registers M.! reg) toggled))
processInstruction (JnzVal val ofs) (registers,cursor,toggled)
  | S.member cursor toggled = (registers,cursor+1,toggled)
  | otherwise = 
    if val == 0
       then (registers,cursor+1,toggled)
       else (registers,cursor+ofs,toggled)
processInstruction (JnzReg reg ofs) (registers,cursor,toggled)
  | S.member cursor toggled = (registers,cursor+1,toggled)
  | otherwise = 
    if registers M.! reg == 0
       then (registers,cursor+1,toggled)
       else (registers,cursor+ofs,toggled)
processInstruction (JnzReg2 ofs reg) (registers,cursor,toggled)
  | S.member cursor toggled = (M.insert reg ofs registers, cursor+1, toggled)
  | otherwise =
    if ofs == 0
       then (registers,cursor+1,toggled)
       else (registers,cursor+(registers M.! reg),toggled)

processOneInstruction :: [Instruction] -> State -> M.Map Char Int
processOneInstruction instructions state@(registers,cursor,toggled) =
    process instructions
  $ processInstruction (instructions !! cursor) state

process :: [Instruction] -> State -> M.Map Char Int
process instructions state@(registers,cursor,toggled)
  | cursor >= length instructions = registers
  | cursor < 6 = processOneInstruction instructions state
  | otherwise = case [instructions !! (cursor-5),
                      instructions !! (cursor-4),
                      instructions !! (cursor-3),
                      instructions !! (cursor-2),
                      instructions !! (cursor-1),
                      instructions !! (cursor)] of
                  -- match adding 2 numbers - add the value from `count reg` to `reg`
                  -- the following three instructions otherwise run by increasing `reg`
                  -- with 1, once per 3 instructions, so it is way slower
                  [_,_,_,(Inc reg), (Dec countReg), (JnzReg jnzReg (-2))] ->
                      if jnzReg == countReg
                         then let count = registers M.! countReg
                                  registers' = M.insertWith (+) reg count registers 
                                  registers'' = M.insert countReg 0 registers'
                               in process instructions (registers'',cursor+1,toggled)
                         else processOneInstruction instructions state
                  -- match multiplying 2 numbers -- add the product of `srcReg` and `countReg2` to `reg`
                  -- the following 5 instructions essentially run the above described product by
                  -- increasing `reg` by one every 3 instructions and then going back to repeating those 3
                  -- instructions `countReg2` number of times
                  [(CopyReg srcReg destReg),
                   (Inc reg),(Dec countReg1),
                   (JnzReg jnzReg1 (-2)),
                   (Dec countReg2),
                   (JnzReg jnzReg2 (-5))] ->
                      if destReg == countReg1 && jnzReg1 == countReg1 && jnzReg2 == countReg2
                         then let count1 = registers M.! srcReg
                                  count2 = registers M.! countReg2
                                  registers' = M.insertWith (+) reg (count1*count2) registers 
                                  registers'' = M.insert countReg1 0 registers'
                                  registers''' = M.insert countReg2 0 registers''
                               in process instructions (registers''',cursor+1,toggled)
                         else processOneInstruction instructions state
                  _ -> processOneInstruction instructions state
