module Main where

import qualified Common
import Data.List.Split (splitOn)
import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    let (width,height) = (if length parsedInput > 10
                             then (50,6) -- real input
                             else (7,3)  -- example
                             ) :: (Int,Int)
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let states = scanl
                  (\grid instruction ->
                      case instruction of
                        Rect w h -> foldr
                                    (:)
                                    grid
                                    [(x,y) | x <- [0..w-1], y <- [0..h-1]]
                        RotateRow row ofs -> map
                                    (\(x,y) -> if y == row
                                                  then (mod (x+ofs) width,y)
                                                  else (x,y))
                                    grid
                        RotateColumn col ofs -> map
                                    (\(x,y) -> if x == col
                                                  then (x,mod (y+ofs) height)
                                                  else (x,y))
                                    grid)
                  []
                  parsedInput
    let answer1 = length $ last states
    let answer2 = "not solved yet"

    -- Print answers
    let draw xs = foldr1 (++)
                . map (++ "\n") 
                $ [ [(if (x,y) `elem` xs
                       then '#'
                       else '.') | x <- [0..width-1] ] | y <- [0..height-1] ]
     in putStrLn $ "Visualise 1 \n" ++ (foldr ((++) . (++ "\n\n")) "\n" $ (map draw states))
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: Apply OCR ;)"

data Instruction = Rect Int Int
                 | RotateRow Int Int
                 | RotateColumn Int Int
                 deriving (Show)

parse input = map parseOne $ lines input
    where parseOne ('r':'e':xs) =
            let (w:h:[]) = splitOn "x" $ (splitOn " " xs) !! 1
             in Rect (read w) (read h)
          parseOne ('r':'o':'t':'a':'t':'e':' ':'r':xs) =
            let [_,rowBit,_,ofs]= splitOn " " xs
                row = (splitOn "=" rowBit) !! 1
             in RotateRow (read row) (read ofs)
          parseOne xs =
            let [_,_,colBit,_,ofs]= splitOn " " xs
                column = (splitOn "=" colBit) !! 1
             in RotateColumn (read column) (read ofs)
