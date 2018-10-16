module Lab6 where

import Data.List
import System.Random
import Lecture6 hiding (exM, composites)
import Text.Printf
import System.CPUTime
import Control.Monad

-- Assignment 1
exM :: Integer -> Integer -> Integer -> Integer
exM x p n
  | n == 1 = 0
  | p == 0 && n /= 0 = 1
  | even p = exM x (p `quot` 2) n ^ 2 `mod` n
  | otherwise = exM x ((p-1) `quot` 2) n ^ 2 * (x `mod` n)

-- Assignment 2
timeExM :: Integer -> Integer -> Integer -> IO ()
timeExM x p n = do
    start <- getCPUTime
    let r1 = exM x p n
    printf "(exM) %i ^ %i mod %i = %i \n" x p n r1
    end   <- getCPUTime
    startr <- getCPUTime
    let r2 = expM x p n
    printf "(expM) %i ^ %i mod %i = %i \n" x p n r2
    endr <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    let diffr = fromIntegral (endr - startr) / (10^12)
    printf "Computation time exM function: %0.5f sec\n" (diff :: Double)
    printf "Computation time expM function: %0.5f sec\n\n" (diffr :: Double)

testExMExpM :: IO ()
testExMExpM = do
  timeExM 34 56000000 3
  timeExM 12 23000000 23
  timeExM 7000 10000000 1

{-
  TEST REPORT
  Running the testExMExpM test clearly states much higher executing times for
  the expM function∷

  (exM) 34 ^ 56000000 mod 3 = 1
  (expM) 34 ^ 56000000 mod 3 = 1
  Computation time exM function: 0.00040 sec
  Computation time expM function: 3.38628 sec

  (exM) 12 ^ 23000000 mod 23 = 12
  (expM) 12 ^ 23000000 mod 23 = 12
  Computation time exM function: 0.00026 sec
  Computation time expM function: 0.86488 sec

  (exM) 7000 ^ 10000000 mod 1 = 0
  (expM) 7000 ^ 10000000 mod 1 = 0
  Computation time exM function: 0.00023 sec
  Computation time expM function: 1.33813 sec
-}

-- Assignment 3
composites :: [Integer]
composites = [x | x <- [4..], (not . prime) x]

-- Assignment 4
{-
  Create a list of Fermant liars based on a list of n composites and
  k iterations
-}
testFermantLiar :: Int -> Int -> IO [Integer]
testFermantLiar n k = filterM (primeTestsF k) (take n composites)

{-
  Find the smallest fermant liar by applying the test multiple times
-}
minFermantLiar :: Int -> IO Integer
minFermantLiar k = do
    fl <- fermantMultiple 10000
    return (minimum fl)
  where
    fermantMultiple 0 = return []
    fermantMultiple n =
      do
        t <- testFermantLiar 100 k
        r <- fermantMultiple (n-1)
        return (t ++ r)
{-
  TEST REPORT

  Multiple applications of testFermantLiar with n=1000 and k=1:
  [35,39,45,91,112,133,187,405,451,469,561,627,663,697,717,841,1165]
  [35,95,185,247,289,341,469,561,651,703,805,871,889,1057,1105]
  [9,15,27,35,55,65,91,147,155,175,187,259,265,361,465,475,481,553,645,925,957,1105]
  [9,65,66,85,91,115,117,145,305,435,475,496,561,697,742,952,1105]

  With the minFermantLiar test, we find 9 as the smallest liar

  n=1000 and k=2:
  [49,703,781]
  [133,165]
  [57,451]
  133,561]

  With the minFermantLiar test, we find 9 as the smallest liar

  n=1000 and k=3
  [561]
  []
  [561]
  [1105]

  With the minFermantLiar test, we find 9 as the smallest liar

  When we increase k, the minFermantLiartest finds the following minimums:
  k=4     9
  k=5     9
  k=6     21
  k=7     45
  k=8     91
  k=9     91
  k=10    91

  Increasing k greatly reduces the amount of false positives.
-}

-- Assignment 5
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
              k <- [2..],
              prime (6*k+1),
              prime (12*k+1),
              prime (18*k+1) ]

testFermantLiarCarm :: Int -> Int -> IO [Integer]
testFermantLiarCarm n k = filterM (primeTestsF k) (take n carmichael)

-- Assignment 6
testMRCarm :: Int -> Int -> IO [Integer]
testMRCarm n k = filterM (primeMR k) (take n carmichael)

-- Assignment 7
{-
  Finds the first 9 mersene prime numbers.

  While researching the subject, I found that finding these numbers is very
  computational expensive. If I make the list of primes larger, the function
  will freeze since it is simply to difficult to test whether it is a mersene
  prime.
-}
mersenePrimes :: IO [Integer]
mersenePrimes = do
  bases <- filterM (\p -> primeMR 1 (2^p - 1)) (take 9 primes)
  return (map (\p -> 2^p-1) bases)

-- Assignment 8
