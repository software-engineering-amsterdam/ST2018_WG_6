
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
withinRange :: Float -> Float -> Float -> Bool
withinRange x y p = (x <= (y + (p / 100) * y)) && (x >= (y - (p / 100) * y))

-- Function to check if a list of frequencies is evenly distributed
testFrequencyErrors :: [Int] -> IO Bool
testFrequencyErrors l = 
    let average = ((fromIntegral (sum l)) / 4.0) in return (foldr ((&&) . (\x -> withinRange (fromIntegral x) average 5.0)) True l)

test1 :: a -> IO Bool
test1 a = randomQuartileDistribution 10000 >>= testFrequencyErrors

test1result = do
    result <- mapM test1 [1..100]
    let correct = show (length (filter (\x -> x) result)) in putStrLn (correct ++ " out of 100 tests are within 5% of the expected range")


-- Assignment 2 --

-- According to wikipedia: 
-- Condition on the sides 
-- The triangle inequality states that the sum of the lengths of any two sides of a triangle 
-- must be greater than or equal to the length of the third side. That sum can equal the length
-- of the third side only in the case of a degenerate triangle, one with collinear vertices.
-- It is not possible for that sum to be less than the length of the third side. 
-- A triangle with three given positive side lengths exists if and only if those side lengths satisfy the triangle inequality.

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a + b < c || a + c < b || b + c < a = NoTriangle
    | a == b && b == c = Equilateral
    | a == b || a == c || b == c = Isosceles
    | a^2 + b^2 == c^2 || a^2 + b^2 == c^2 || b^2 + c^2 == a^2 = Rectangular
    | otherwise = Other


testTriangles = [
    ([2,2,2], Isosceles),
    ([1,2,2], Equilateral),
    ([1,2,3], Other),
    ([3,4,5], Rectangular)]

-- MAIN --
main :: IO ()
main = do
    putStrLn "-- Lab 2 Team 6 --"
    putStrLn "\nAssignment 1"
    putStr "Result: "
    test1result

