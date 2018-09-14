
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Text.Show.Functions

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

-- Assignment 1: 90 minutes --

-- true if n is between x and y
between :: Ord a => a -> a -> a -> Bool
between x y n = n >= x && n < y

{-
  For a list of floats where each element is in the open interval (0..1),
  give the nth quartile. That is, n is in [1,2,3,4]. It will return an empty list
  if an invalid quartile is requested.
-}
quartile :: Int -> [Float] -> [Float]
quartile n s
  | n == 1 = filter (between 0 0.25) s
  | n == 2 = filter (between 0.25 0.5) s
  | n == 3 = filter (between 0.5 0.75) s
  | n == 4 = filter (between 0.75 1) s
  | otherwise = []

{-
  This function takes a list of floats as input, where each element is in the
  open interval (0..1). It returns the distribution for each quartile in the list
  as a list with 4 values.
-}
quartileDistribution :: Int -> IO [Int]
quartileDistribution xs = do
  x <- probs xs
  return (map (length . flip quartile x) [1..4])

{-
  This function takes a list of quartile distributions over a list of 10000
  random floats and checks whether each distribution is roughly 2500, with a
  margin of error of 5%
-}
quartileDistributionResultCheck :: [Int] -> IO Bool
quartileDistributionResultCheck xs = return (foldr ((&&) . between 2375 2625) True xs)

{-
  This function performs a quartile distribution check with a list of 10000
  random floats in the open range (0..1).
-}
performQuartileCheck :: a -> IO Bool
performQuartileCheck n = quartileDistribution 10000 >>= quartileDistributionResultCheck

{-
  This test calls performQuartileCheck 100 times and keeps track of how many
  times it succeeds. It will print the result.
-}
quartileTest :: IO ()
quartileTest = do
  result <- mapM performQuartileCheck [1..100]
  let passed = show (length (filter id result)) in
    putStrLn (passed ++ " out of 100 tests did pass")

{-
  -- TEST REPORT --
  Based on multiple runs of the quartile test, we can conclude that the function
  is correct. Most of the test pass (with the margin of error of 5%). Below are
  some results
    - 99 out of 100 tests did pass
    - 99 out of 100 tests did pass
    - 100 out of 100 tests did pass
    - 96 out of 100 tests did pass
-}

-- Assignment 2 --
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c =  a + b > c && b + c > a && a + c > b

pythagorean :: Integer -> Integer -> Integer -> Bool
pythagorean a b c = (a ^ 2 + b ^ 2 == c ^ 2)
  || (a ^ 2 + c ^ 2  == b ^ 2)
  || (b ^ 2 + c ^ 2 == a ^ 2)

-- Returns the shape based on the three sides of a triangle
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | not (isTriangle a b c) = NoTriangle
  | a == b && b == c = Equilateral
  | a == b || a == c || b == c = Isosceles
  | pythagorean a b c = Rectangular
  | otherwise = Other

-- Tests whether the list of three Integers is a Triangle of the given shape
testTriangle :: [Integer] -> Shape -> Bool
testTriangle (a:b:c) shape
  | length (a:b:c) < 3 = False
  | otherwise = triangle a b (head c) == shape

testValuesTriangles = [([3,4,5], Rectangular), ([3,3,5], Isosceles),
  ([3,3,3], Equilateral), ([10,1,1], NoTriangle), ([2,3,4], Other)]

testTriangles :: [([Integer], Shape)] -> Bool
testTriangles = foldr ((&&) . uncurry testTriangle) True

{-
  -- TEST REPORT --
  Executing the test with `testTriangles testValuesTriangles` results in the
  value true, indicating that the function is correctly classifying the
  triangles.
-}

-- Assignment 3 --

-- The properties not listed, are the same as properties already listed
p11, p12, p21, p31 :: Int -> Bool
p11 x = even x && x > 3
p12 = even
p21 x = even x || x > 3
p31 x = p11 x  || even x

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker   xs p q = stronger xs q p

pToString :: (Int -> Bool) -> String
pToString p
  | not (p 2) = "Property 1.1"
  | p 5 = "Property 2.1"
  | otherwise = "Property 1.2, 3.1"

sortStronger :: [a] -> [a -> Bool] -> [a -> Bool]
sortStronger _ [] = []
sortStronger domain (x:xs) =
  sortStronger domain [a | a <- xs, stronger domain a x]
  ++ [x]
  ++ sortStronger domain [a | a <- xs, weaker domain a x]

domain :: [Int]
domain = [-10..10]

sortedProbs :: [Int -> Bool]
sortedProbs = sortStronger domain [p11, p12, p21, p31]

{-
  Test the sortStronger function on the properties from Exercise 3, based on
  the following property.

  For a list of sorted properties on descending Strength, property pi at index i
  is stronger than all properties pj..pn-1 where j > i and n is the lenght of the list.
-}
prop_SortStrongerDesc :: Property
prop_SortStrongerAsc =
  forAll (choose (0, length sortedProbs-1)) $ \i ->
    forAll (choose (i, length sortedProbs-1)) $ \j ->
      let (p1, p2) = (sortedProbs !! i, sortedProbs !! j ) in
        collect (i,j) $ stronger domain p1 p2

{-
  Run this function to run the test
-}
sortedProbsTest = quickCheck prop_SortStrongerAsc

{-
  -- TEST REPORT --
  The test passes, proving that the list of properties is sorted for the domain
  [-10..10]. 
-}

-- Assignment 4 --
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = elem xs (permutations ys)


-- Assignment 5 --
isDerangement [] [] = True
isDerangement (x:xs) (y:ys)
  | length xs /= length ys = False
  | otherwise = x /= y && isDerangement xs ys

deran xs = filter (isDerangement xs) (permutations xs)

-- prop_SameLength xs ys = length xs == length ys
--   where types = (xs :: [Int], ys :: [Int])

-- Assignment 6 --

{-
  *Specification ROT13*
  ROT13 s changes every character in the String s by shifting it 13 places.
  Since the Latin Alphabet is 26 characters long, applying it twice will result
  in s. That is ROT13 (ROT13 s) == s
-}
isLatinChar :: Char -> Bool
isLatinChar c = c `elem` ['a'..'z'] ++ ['A'..'Z']

addToChar :: Char -> Int -> Char
addToChar c n
  | not (isLatinChar c) = c
  | isUpper c = chr (((ord c + n - ord 'A') `mod` 26) + ord 'A')
  | isLower c = chr (((ord c + n - ord 'a') `mod` 26) + ord 'a')

rot13 :: String -> String
rot13 = map (`addToChar` 13)

prop_sameLength :: String -> Bool
prop_sameLength s = length s == length (rot13 s)

prop_decode :: String -> Bool
prop_decode s = s == (rot13 . rot13) s

-- Assignment 7 --
ibanMoveChars (a:b:c:d:xs) = xs ++ [a,b,c,d]

ibanAlphaToChar [] = []
ibanAlphaToChar (x:xs)
  | isLetter x = show (ord x - ord 'A' + 10) ++ ibanAlphaToChar xs
  | otherwise = x : ibanAlphaToChar xs

iban :: String -> Bool
iban s = read (ibanAlphaToChar (ibanMoveChars s)) `mod` 97 == 1



main :: IO ()
main = quartileTest
