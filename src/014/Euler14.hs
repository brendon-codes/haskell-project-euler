--
-- Project Euler
--
-- Problem #14
-- Longest Collatz sequence
--
--  The following iterative sequence is defined for the
--  set of positive integers:
--
--    n -> n/2 (n is even)
--    n -> 3n + 1 (n is odd)
--
--  Using the rule above and starting with 13, we generate
--  the following sequence:
--
--    13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
--  It can be seen that this sequence (starting at 13 and
--  finishing at 1) contains 10 terms. Although it has not
--  been proved yet (Collatz Problem), it is thought that
--  all starting numbers finish at 1.
--
--  Which starting number, under one million, produces the
--  longest chain?
--
--  NOTE: Once the chain starts the terms are allowed to go
--  above one million.
--
-- Answer: ??
--


module Main (
  input,
  solve,
  main
) where


--
-- Input
--
input :: String -> [Integer]
input s = map read $ lines s


--
-- Collatz
--
collatz :: Int -> Int
collatz n
  | even n = div n 2
  | otherwise = (3 * n) + 1

--
-- Solve
--
solve :: [Integer] -> Integer
solve x = read $ take z $ show $ sum x
  where z = 10

--
-- Main
--
main :: IO ()
main = do
  x <- getContents;
  let inp = input x;
      out = solve inp;
  print out;


