
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- Assignment 1 --

-- Filters for each quartile
f1,f2,f3,f4 :: Float -> Bool
f1 x = (x <= 0.25)
f2 x = (x > 0.25 && x <= 0.5)
f3 x = (x > 0.5 && x <= 0.75)
f4 x = (x > 0.75)

-- For a given Int x, this method will create x number of random numbers and returns the distribution
-- within quartiles (the amount of numbers in each quartile)
randomQuartileDistribution :: Int -> IO [Int]
randomQuartileDistribution x = do
    list <- probs x 
    let q1 = length (filter f1 list)
    let q2 = length (filter f2 list)
    let q3 = length (filter f3 list)
    let q4 = length (filter f4 list)
    return [q1, q2, q3, q4]

-- Method to check if a certain value is within the range of a certain percentage higher or lower than another value
--     @param1        the value to check
--     @param2        the value we want it to be
--     @param3        the percentage they are allowed to differ

--     @return        true if the checked value is within the range of the desired value
withinRange :: Int -> Int -> Int -> Bool
withinRange x y p = (x <= (y + (p `div` 100) * y)) && (x >= (y - (p `div` 100) * y))

evenlyDistributed :: [Int] -> Bool
evenlyDistributed l = (foldr ((&&) . (\x -> withinRange x (sum l `div` 4) 5)) True l)



-- test1 = 

-- main :: IO ()
-- main = do
--     putStrLn "-- Lab 2 Team 6 --"
--     putStrLn "\nAssignment 1"
--     putStr "Test: " 