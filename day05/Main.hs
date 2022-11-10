module Main where

import qualified Common
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = take 8
                $ foldr
                  (\x acc ->
                      let hash = show . md5 . pack $ parsedInput ++ show x
                       in case () of _
                                      | "00000" == take 5 hash -> (hash !! 5):acc
                                      | otherwise              -> acc)
                  [] [0..]
    let answer2 = crack parsedInput "________" 0

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = input

crack :: String -> String -> Int -> String
crack parsedInput password i
    | not $ '_' `elem` password = password
    | otherwise =
        let hash = show . md5 . pack $ parsedInput ++ show i
         in case () of _
                        | "00000" == take 5 hash ->
                            let position = hash !! 5
                                positionInt = read [position] :: Int
                                char = hash !! 6
                                newPassword = (take positionInt password) ++ [char] ++ (drop (positionInt + 1) password)
                             in if (ord position) >= 48
                                   && (ord position) <= 55
                                   && password !! positionInt == '_'
                                   then crack parsedInput newPassword (i + 1)
                                   else crack parsedInput password (i + 1)
                        | otherwise -> crack parsedInput password (i + 1)

{-
Example input:
abc

Real input:
ffykfhsq
-}
