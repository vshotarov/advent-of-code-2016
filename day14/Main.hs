module Main where

import qualified Common
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = do
    putStrLn $ "-- Solving day14 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let salt = parsedInput
    let getHashForIndex1 i = show . md5 . pack $ salt ++ (show i) 
    let solve1 :: [String] -> Int -> [Int]
        solve1 (thisHash:next999Hashes) i =
            let newHash = getHashForIndex1 (i + 1000)
                new1000Hashes = next999Hashes ++ [newHash]
                threeOfTheSame = hasThreeOfTheSame thisHash
             in case threeOfTheSame of
                  Nothing -> solve1 new1000Hashes (i+1)
                  Just a  -> if any (hasFiveOfTheSame a) new1000Hashes
                                then i:(solve1 new1000Hashes (i+1))
                                else solve1 new1000Hashes (i+1)
    let answer1 = last . take 64 $ solve1 (take 1000 $ map getHashForIndex1 [0..]) 0
    let getHashForIndex2 i = go 2016 (show . md5 . pack $ salt ++ (show i))
            where go 0 hash = hash
                  go counter hash = go (counter-1) $ (show . md5 $ pack hash)
    let solve2 :: [String] -> Int -> [Int]
        solve2 (thisHash:next999Hashes) i =
            let newHash = getHashForIndex2 (i + 1000)
                new1000Hashes = next999Hashes ++ [newHash]
                threeOfTheSame = hasThreeOfTheSame thisHash
             in case threeOfTheSame of
                  Nothing -> solve2 new1000Hashes (i+1)
                  Just a  -> if any (hasFiveOfTheSame a) new1000Hashes
                                then i:(solve2 new1000Hashes (i+1))
                                else solve2 new1000Hashes (i+1)
    let answer2 = last . take 64 $ solve2 (take 1000 $ map getHashForIndex2 [0..]) 0

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = input

hasThreeOfTheSame :: String -> Maybe Char
hasThreeOfTheSame xs | length xs < 3 = Nothing
hasThreeOfTheSame (a:b:c:xs)
      | a == b && a == c = Just a
      | otherwise   = hasThreeOfTheSame (b:c:xs)

hasFiveOfTheSame :: Char -> String -> Bool
hasFiveOfTheSame _ xs | length xs < 5 = False
hasFiveOfTheSame char (a:b:c:d:e:xs)
      | a == char && a == b && a == c && a == d && a == e = True
      | otherwise = hasFiveOfTheSame char (b:c:d:e:xs)
