module Lab6 where

import Data.List
import System.Random
import Lecture6
import System.CPUTime
import Text.Printf
import Control.Applicative


-- Assignment 1 - 45 minutes
exM' :: Integer -> Integer -> Integer -> Integer
exM' b 0 m = 1
exM' b e m | even e      = (exM b (e `div` 2) m) ^ 2 `mod` m
           | otherwise   = ((exM b (e-1 `div` 2) m) ^ 2) * b `mod` m

-- Assignment 2
testEx :: Integer -> Integer -> Integer -> IO ()
testEx b e m = do
    
    start <- getCPUTime
    let result = exM b e m
    printf "Result with expM: %i\n" (result :: Integer)
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "calculated in %0.5f sec\n" (diff :: Double)
    
    start' <- getCPUTime
    let result' = exM' b e m
    end' <- getCPUTime
    let diff' = fromIntegral (end' - start') / (10^12)
    printf "Result with exM: %i\n" (result :: Integer)
    printf "calculated in %0.5f sec\n" (diff' :: Double)
    
    printf "Test finished\n"

-- Performs the actual test with some great numbers
testAssignment2 :: IO ()
testAssignment2 = do
    testEx 22 1351233839 15
    testEx 11 1348798636 32

-- Assignment 3
divisors :: Integer -> [Integer]
divisors n = [x | x <- [2..n-1], n `mod` x == 0]

isComposite :: Integer -> Bool
isComposite n | n <= 3 = False
              | otherwise = length (divisors n) > 0

composites :: [Integer]
composites = [x | x <- [1..], isComposite x]

-- Assignment 4
