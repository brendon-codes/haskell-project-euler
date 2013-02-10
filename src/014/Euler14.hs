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
-- Answer: 837799
--


module Main (
  collatz,
  collatzSeq,
  seqs,
  pairs,
  getMax,
  extract,
  solve,
  main
) where


import Data.List (maximumBy)
import Data.Ord (comparing)


--
-- Collatz
--
collatz :: Int -> Int
collatz n
  | even n = div n 2
  | otherwise = (3 * n) + 1


--
-- Collatz Sequence
--
-- TODO: Can this be made non-recursive?
--       This is causing stack-overflow
--       with large numbers.
--
collatzSeq :: Int -> [Int]
collatzSeq n = reverse $ c [n]
  where c a@(1 : xs) = a
        c a@(x : xs) = c ((collatz x) : a)


--
-- Build Sequences 
--
seqs :: Int -> [[Int]]
seqs n = map collatzSeq x
  where x
          | (n == 1) = [1]
          | otherwise = [n,n-1..1]


--
-- Get starting lengths/pairs
--
pairs :: Int -> [(Int, Int)]
pairs n = map prs (seqs n)
  where prs xs = (length xs, head xs)


--
-- Get max
--
getMax :: [(Int, Int)] -> (Int, Int)
getMax prs = maximumBy (comparing fst) prs


--
-- Extract maximum
--
extract :: Int -> Int
extract n = snd (getMax $ pairs n)


--
-- Solve
--
solve :: Int
solve = extract n
  where n = 1000000


--
-- Main
--
main :: IO ()
main = do
  print solve;


