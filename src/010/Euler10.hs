--
-- Project Euler
--
-- Problem #10
-- Summation of primes
--
--   The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
--   Find the sum of all the primes below two million.
--
-- Answer: 142913828922
--

{-# OPTIONS_GHC -O2 #-}


module Main (
  primesToNA,
  solve,
  main
) where


import Data.Array.Unboxed
 

--
-- This was taken from:
-- http://www.haskell.org/haskellwiki/Prime_numbers#Calculating_Primes_Upto_a_Given_Value
--
-- I do not think i am in the mood to try and
-- re-invent the wheel (bad pun?) for calculating primes.
--
primesToNA :: Int -> [Int]
primesToNA n = 2: [i | i <- [3,5..n], ar ! i]
  where
    ar = f 5 $ accumArray (\ a b -> False) True (3,n) 
                        [(i,()) | i <- [9,15..n]]
    f p a | q > n = a
          | True  = if null x then a' else f (head x) a'
      where q = p*p
            a' :: UArray Int Bool
            a'= a // [(i,False) | i <- [q, q+2*p..n]]
            x = [i | i <- [p+2,p+4..n], a' ! i]


solve :: Int
solve = sum $ primesToNA n
  where
    n = 2000000


main :: IO ()
main = putStrLn $ show solve


