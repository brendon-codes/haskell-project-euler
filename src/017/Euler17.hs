--
-- Project Euler
--
-- Problem #16
-- Power digit sum
--
--   215 = 32768 and the sum of its digits is
--
--     3 + 2 + 7 + 6 + 8 = 26.
--
--   What is the sum of the digits of the number 2^1000?
--
-- Answer: ??
--


module Main (
  solve,
  main
) where


ones = ["zero",      "one",
        "two",       "three",
        "four",      "five",
        "six",       "seven",
        "eight",     "nine",
        "ten",       "eleven",
        "twelve",    "thirteen",
        "fourteen",  "fifteen",
        "sixteen",   "seventeen",
        "eighteen",  "nineteen"]


tens = ["twenty", "thirty",
        "forty",  "fifty",
        "sixty",  "seventy",
        "eighty", "ninety"]


hun = "onehundred"

thous = "onethousand"


build x 
  | x < 20 = ones !! x




--
-- Solve
--
solve :: Int
solve = 1


--
-- Main
--
main :: IO ()
main = do
  print solve;


