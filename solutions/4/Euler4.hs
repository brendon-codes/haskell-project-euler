--
-- Project Euler
--
-- Problem #4
-- Largest palindrome product
--
--   A palindromic number reads the same both ways.
--   The largest palindrome made from the product of
--   two 2-digit numbers is 9009 = 91 × 99.
--
--   Find the largest palindrome made from the product
--   of two 3-digit numbers.
--
-- Answer: 906609
--

module Main (
  nums,
  builder,
  mults,
  allMults,
  eachMult,
  filtered,
  grab,
  solve,
  main
) where 


nums 1 = [1]
nums 2 = [2]
nums n = [n,n-1..1]


builder x = map (*x) (nums x)


mults y z = map builder [z,z-1..y]


allMults y z = concat (mults y z)


eachMult m = (reverse s) == s
  where s = show m


filtered y z = filter eachMult (allMults y z)


grab y z = maximum (filtered y z)


solve = grab y z
  where y = 100
        z = 999


main :: IO ()
main = putStrLn $ show solve


