--
-- Project Euler
--
-- Problem #5
-- Smallest Multiple
--
--   2520 is the smallest number that can be divided by each
--   of the numbers from 1 to 10 without any remainder.
--
--   What is the smallest positive number that is evenly
--   divisible by all of the numbers from 1 to 20?
--
-- Answer: 232792560
--

module Main (
  solve,
  main
) where


-- middle m
--   | even m = (div m 2) + 1
--   | otherwise = (div (m + 1) 2) + 1


-- builder m s f = out
--   where checker i j = ((mod i j) /= 0)
--         items i     = [j | j <- [f..m], checker i j]
--         found i     = (length $ items i) == 0
--         out         = [i | i <- [s..], found i]


-- limitBuilder m s f = head (builder m s f)


-- prep m = limitBuilder m s f
--   where s = (m - 1) * m
--         f = middle m


-- solve :: Int
-- solve = prep m
--  where m = 18


--
-- Ended up stealing this from haskell.org.
-- Wish I came up with such a clean solution.
--
solve :: Int
solve = foldr1 lcm [1..n]
  where n = 20


main :: IO ()
main = putStrLn $ show solve


