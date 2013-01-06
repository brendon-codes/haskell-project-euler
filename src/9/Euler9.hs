--
-- Project Euler
--
-- Problem #9
-- Special Pythagorean triplet
--
--   A Pythagorean triplet is a set of three
--   natural numbers, a < b < c, for which:
--
--     a2 + b2 = c2
--
--   For example:
--
--     32 + 42 = 9 + 16 = 25 = 52.
--
--   There exists exactly one Pythagorean triplet for
--   which a + b + c = 1000. Find the product abc.
--
-- Answer: 31875000
--

module Main (
  triplets,
  solve,
  main
) where


triplets m = [[a, b, c]
             | c <- [1..m],
               b <- [1..c],
               a <- [1..b],
               a + b + c == m,
               a^2 + b^2 == c^2]


solve :: Int
solve = product $ head $ triplets m
  where
    m = 1000


main :: IO ()
main = putStrLn $ show solve


