--
-- Project Euler
--
-- Problem #3
-- Largest prime factor
--
--   The prime factors of 13195 are 5, 7, 13 and 29.
--   What is the largest prime factor of the number 600851475143 ?
--
-- Answer: 6857
--

module Main (
  validator,
  modded,
  cyc,
  filtr,
  dv,
  solve,
  main
) where 


validator n x = (n `mod` x == 0)


modded n = filter (validator n) (2:[3,5..])


cyc n = scanl1 (*) (modded n)


filtr n = takeWhile (<=n) (cyc n)


dv m = (m !! 0) `div` (m !! 1)


solve = dv $ reverse $ filtr n
  where n = 600851475143


main :: IO ()
main = putStrLn $ show solve


