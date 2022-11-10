module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length
                . filter
                    (\(regular,hyper) ->
                        (any hasPalindrome regular)
                     && not (any hasPalindrome hyper))
                $ parsedInput
    let answer2 = length
                . filter
                    (\(regular,hyper) ->
                        let aBas = foldr (++) [] $ map getABAs regular
                            bAbs = foldr (++) [] $ map getABAs hyper
                          in any ((`elem` aBas) . invertABA) bAbs)
                 $ parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse input = map parseHypernetSequences $ lines input

parseHypernetSequences :: String -> ([String], [String])
parseHypernetSequences ip = (reverse regularReversed, reverse hyperReversed)
    where go 0 buffer (reg,hyper) [] = ((reverse buffer):reg,hyper)
          go 1 buffer (reg,hyper) [] = (reg,(reverse buffer):hyper)
          go 0 buffer (reg,hyper) ('[':xs) = go 1 "" ((reverse buffer):reg,hyper) xs
          go 1 buffer (reg,hyper) (']':xs) = go 0 "" (reg,(reverse buffer):hyper) xs
          go i buffer (reg,hyper) (x:xs) = go i (x:buffer) (reg,hyper) xs
          (regularReversed,hyperReversed) = go 0 "" ([],[]) ip

hasPalindrome :: String -> Bool
hasPalindrome xs
  | length xs < 4 = False
  | otherwise     = let (a:b:c:d:xs') = xs
                     in if [a,b] == [d,c]
                           then not ((a == b) && (a == c) && (a == d))
                           else hasPalindrome (b:c:d:xs')

getABAs :: String -> [String]
getABAs xs
  | length xs < 3 = []
  | otherwise     = let (a:b:c:xs') = xs
                     in if a == c && a /= b
                           then [a,b,c]:(getABAs (b:c:xs'))
                           else getABAs (b:c:xs')

invertABA :: String -> String
invertABA (a:b:_:[]) = [b,a,b]
