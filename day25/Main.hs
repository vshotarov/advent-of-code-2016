module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day25 --"

    -- Solve
    -- After working out what the program does on paper,
    -- I've found the following:
    --
    -- The program takes the input to `a` and adds 2538 to it,
    -- which becomes the `d` register for the rest of the program's life.
    --
    -- Then that number gets repeatedly divided by 2 where at each
    -- division we print out a 0 if the number was even and 1 if it was odd,
    -- until the number becomes a 0 itself, at which point we just reinitialise
    -- it to the value we've stored in `d` and start the repeated division again.
    --
    -- Therefore, what we're after is a number that when added 2538 to and
    -- repeatedly divided by 2, gives us the sequence 0,1,0,1...1.
    --
    -- Importantly, ending on a 1, as as soon as the number becomes 0, we
    -- restart the procedure and we want to start on a 0.
    --
    -- Since repeatedly dividing by 0 and checking whether there was a remainder
    -- or not is taking the binary representation of a number, the task
    -- is simplified to finding the first positive integer which when
    -- added 2538 to gives us a binary number cycling between 0 and 1,
    -- importantly starting on a 0, i.e. it's an even number.
    --
    -- Since we know we're adding 2538, which is an even number, we can
    let answer1 = -2538
                + Common.firstWhere (isRepeating . toBinary) [2538,2540..]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1

toBinary :: Int -> String
toBinary 0 = "0"
toBinary x = (show (rem x 2)) ++ toBinary (div x 2)

isRepeating :: String -> Bool
isRepeating [] = True
isRepeating [_] = True
isRepeating (x1:x2:xs') = (x1 /= x2) && isRepeating (x2:xs')
