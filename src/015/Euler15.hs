--
-- Project Euler
--
-- Problem #15
-- Lattice Paths
--
-- Starting in the top left corner of a 2×2 grid,
-- there are 6 routes (without backtracking) to the
-- bottom right corner.
--
--   __ __
--  |__|__|
--  |__|__|
--
-- Answer: 137846528820
--


module Main (
  paths,
  solve,
  main
) where


--
-- Uses central binomial coefficients
--
-- See: https://en.wikipedia.org/wiki/Central_binomial_coefficient
-- See: http://www.robertdickau.com/manhattan.html
--
paths :: Integer -> Integer
paths n = (product [1..(2 * n)]) `div`
          ((product [1..n]) ^ 2)


--
-- Solve
--
solve :: Integer
solve = paths n
  where n = 20


--
-- Main
--
main :: IO ()
main = do
  print solve;


