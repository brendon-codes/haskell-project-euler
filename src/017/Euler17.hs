--
-- Project Euler
--
-- Problem #17
-- Number letter counts
--
--   If the numbers 1 to 5 are written out in words:
--
--     one, two, three, four, five
--
--   then there are
--
--     3 + 3 + 5 + 4 + 4 = 19
--
--   letters used in total.
--
--   If all the numbers from 1 to 1000 (one thousand) inclusive
--   were written out in words, how many letters would be used?
--
--   NOTE: Do not count spaces or hyphens. For example, 342
--   (three hundred and forty-two) contains 23 letters and 115
--   (one hundred and fifteen) contains 20 letters. The use of
--   "and" when writing out numbers is in compliance with
--   British usage.
--
-- Answer: ??
--


module Main (
  solve,
  main
) where


ones = ["one",
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

andd = "and"
hun = "hundred"
thous = "thousand"


comp :: Int -> String
comp x
  | (x < 1) =
    error "Value must be greater than zero"
  | (x < 20) =
    ones !! (x - 1)
  | (x < 100) =
    (tens !! ((x `div` 10) - 2)) ++ (rec x)
  | (x == 100) =
    (ones !! 0) ++ hun
  | (x < 1000) =
    (ones !! ((x `div` 100) - 1)) ++ hun ++ andd ++ (rec x)
  | (x == 1000) =
    (ones !! 0) ++ thous
  | otherwise =
    error "Value must be less than or equal to 1000"
  where
    rec = (comp . read . tail . show)


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


