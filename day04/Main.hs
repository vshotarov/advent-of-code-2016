module Main where

import qualified Common
import Data.Char (isDigit, ord, chr)
import qualified Data.Map as M
import Data.List (sortBy)

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let validRooms = filter isValid parsedInput
    let answer1 = foldr (\(_,id,_) -> (+ id)) 0 validRooms
    let answer2 = fst . head
                . dropWhile ((/= "northpole object storage") . snd)
                $ map (\x@(name,id,checksum) -> (id, decrypt x)) validRooms

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = map parseOne $ lines input

type EncryptedRoom = (String,Int,String)
parseOne :: String -> EncryptedRoom
parseOne str = (name, read id, checksum)
    where go 0 (x:xs) (name,id,checksum)
            | isDigit x                    = go 1 xs (init name,id++[x],checksum)
            | otherwise                    = go 0 xs (name++[x],id,checksum)
          go 1 ('[':xs) (name,id,checksum) = go 2 xs (name,id,checksum)
          go 1 (x:xs) (name,id,checksum)   = go 1 xs (name,id++[x],checksum)
          go 2 (']':[]) output             = output
          go 2 (x:xs) (name,id,checksum)   = go 2 xs (name,id,checksum++[x])
          (name,id,checksum) = go 0 str ("","","")

isValid :: EncryptedRoom -> Bool
isValid (name,id,checksum) =
    (== checksum)
    . map fst
    . take 5
    . sortBy
        (\(k1,v1) (k2,v2) ->
            case () of _
                        | v1 == v2  -> compare k1 k2
                        | otherwise -> compare v2 v1)
    $ M.assocs checksumMap
      where checksumMap = foldr
                            (\x acc -> case x of
                                         '-' -> acc
                                         _   -> M.insertWith (+) x 1 acc)
                            M.empty name

decrypt :: EncryptedRoom -> String
decrypt (name,id,_) = map decryptOne name
    where decryptOne '-' = ' '
          decryptOne x = chr $ 97 + (((ord x - 97) + id) `mod` 26)
