module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day16 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve diskLength =
            getChecksum $ take diskLength fakeData
            where fakeData = dragonCurveStep
                           . last
                           . takeWhile ((< diskLength) . length)
                           $ iterate dragonCurveStep parsedInput
    let answer1 = solve $ if parsedInput == "10000" then 20 else 272
    let answer2 = solve 35651584

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = input

flipBits :: String -> String
flipBits [] = []
flipBits ('0':xs) = '1':(flipBits xs)
flipBits ('1':xs) = '0':(flipBits xs)

dragonCurveStep :: String -> String
dragonCurveStep a = a ++ "0" ++ b
    where b = reverse $ flipBits a

getChecksum :: String -> String
getChecksum bits = go . last . takeWhile (even . length) $ iterate go bits
    where go [] = []
          go [a] = error "go requires a string with minimum length of 2"
          go (a:b:xs)
            | a == b    = '1':(go xs)
            | otherwise = '0':(go xs)

