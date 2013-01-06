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


triplets :: Int -> [(Int, Int, Int)]
triplets m = [(x, y, z)
             | z <- [1..m],
               y <- [1..z],
               x <- [1..y],
               x + y + z == m,
               x^2 + y^2 == z^2]


tripleToList :: (Int, Int, Int) -> [Int]
tripleToList (x, y, z) = [x, y, z]


solve :: Int
solve = product $ tripleToList $ head $ triplets m
  where
    m = 1000


main :: IO ()
main = putStrLn $ show solve


