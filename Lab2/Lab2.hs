
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

-- Assignment 1 --

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
quartileDistribution :: Int -> IO [Integer]
quartileDistribution xs = do
  x <- probs xs
  return (map (toInteger . length . flip quartile x) [1..4])

quartileWithinError :: Float -> Integer -> Integer -> Bool
quartileWithinError e q = between lower upper
  where
    lower = q - floor (e * fromInteger q)
    upper = q + floor (e * fromInteger q)

{-
  This function takes a list of quartile distributions over a list of random
  floats and checks whether each distribution is roughly 1/4th of the total, with
  a margin of error of 5%
-}
quartileDistributionResultCheck :: [Integer] -> IO Bool
quartileDistributionResultCheck xs = return (foldr ((&&) .
  quartileWithinError 0.05 (sum xs `quot` 4)) True xs)

{-
  This function performs a quartile distribution check with a list of 10000
  random floats in the open range (0..1).
-}
performQuartileCheck :: Int -> a -> IO Bool
performQuartileCheck x n = quartileDistribution x >>= quartileDistributionResultCheck

{-
  This test calls performQuartileCheck 100 times and keeps track of how many
  times it succeeds. It will print the result.
-}
quartileTest :: IO ()
quartileTest = do
  result <- mapM (performQuartileCheck 10000) [1..100]
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

{-
  The properties not listed, are the same as properties already listed.
-}
p11, p12, p21, p31 :: Int -> Bool
p11 x = even x && x > 3
p12 = even
p21 x = even x || x > 3
p31 x = p11 x  || even x

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker   xs p q = stronger xs q p

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

  For a list of sorted properties of descending strength, property pi at index i
  is stronger than all properties pj..pn-1 where j > i and n is the length of
  the list.
-}
prop_SortStrongerDesc :: Property
prop_SortStrongerDesc =
  forAll (choose (0, length sortedProbs-1)) $ \i ->
    forAll (choose (i, length sortedProbs-1)) $ \j ->
      let (p1, p2) = (sortedProbs !! i, sortedProbs !! j ) in
        stronger domain p1 p2

{-
  Run this function to run the test
-}
sortedProbsTest = quickCheck prop_SortStrongerDesc

{-
  -- TEST REPORT --
  The test passes, proving that the list of properties is sorted for the domain
  [-10..10].
-}

-- Assignment 4 --
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = elem xs (permutations ys)

-- preconditions, sorted on Strength
prop_sameLengthPerm :: Eq a => [a] -> [a] -> Bool
prop_sameLengthPerm xs ys = length xs == length ys

-- postconditions
-- Associativety, if ys is a permutation of xs, xs is a permutation of ys
prop_associativePerm :: Eq a => [a] -> [a] -> Bool
prop_associativePerm xs ys = isPermutation xs ys --> isPermutation ys xs

{-
  "You may assume that your input lists do not contain duplicates. What does
  this mean for your testing procedure?"
  Testing without duplicates means that the property for equality on the input
  lists is not vallid. The precondition we can define is that the lists should
  be of the same length.

  Note that the input lenght is restricted to a maximum length of 10, otherwise
  the permutations function is too slow
-}

permutationsTest :: [Int] -> Property
permutationsTest xs = length xs < 10 ==>
  let perms = permutations xs in
  forAll (choose (0, length perms- 1)) $ \i ->
    let perm = perms !! i in
      prop_sameLengthPerm xs perm --> isPermutation xs perm
      && prop_associativePerm xs perm


{-
  -- TEST REPORT --
  Quickcheck passes the test. This means that for random generated lists of
  elements, for all it's permutations, the isPermutation function provides the
  correct result.
-}

-- Assignment 5 --
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys)
  | length xs /= length ys = False
  | otherwise = x /= y && isDerangement xs ys

deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) (permutations xs)

-- Preconditions of isDerangement, sorted on strength

-- All derangments contain the same elements as the original list in the same quantity
countElem :: Eq a => [a] -> [a] -> a -> Bool
countElem xs ys x = length (filter (x==) xs) == length (filter (x==) ys)

prop_containsElemDer :: Eq a => [a] -> [a] -> Bool
prop_containsElemDer xs ys = foldr ((&&) . countElem xs ys) True ys

-- All derangements have the same length as the original value
prop_sameLengthDer :: Eq a => [a] -> [a] -> Bool
prop_sameLengthDer xs ys = length xs == length ys

-- All input has at least 2 distinct items
prop_minDistinctDer :: Eq a => [a] -> Bool
prop_minDistinctDer xs = length xs > 1 &&
  length (filter (head xs ==) xs) /= length xs

-- Postcondigtion  of isDerangement
-- isDerangement should be associative
prop_associativeDer :: Eq a => [a] -> [a] -> Bool
prop_associativeDer xs ys = xs `isDerangement` ys == ys `isDerangement` xs

isDerangementTest :: [Int] -> Property
isDerangementTest xs =  length xs < 10 && prop_minDistinctDer xs ==>
  let derans = deran xs in
    forAll (choose (0, length derans - 1)) $ \i ->
      i < 0 ||                                 -- guard for negative index
      let derangment = derans !! i in
        (prop_containsElemDer xs derangment && -- Precondition 1
        prop_sameLengthDer xs derangment) -->  -- Precondition 2
        isDerangement xs derangment &&         -- The function we test
        prop_associativeDer xs derangment      -- Postcondition

{-
  TEST REPORT

  The test runs quite slow, since permutations is involved again. There are also
  quite some properties defined.

  During testing I found out that the test failed on [0,1,0], since it has no
  possible derangments, while it satisfies the preconditions. However, if we would
  design a method to find whether it would be possible to derange such a list,
  we would have the deran function again and in extend the is derangment function.
  That would be testing a function with it's own function. Therefore a guard for
  negative indices is added.

  The test does succeed now.
-}


-- Assignment 6 --

{-
  *Specification ROT13*
  ROT13 changes every character in the String s by shifting it 13 places.
  Since the Latin Alphabet is 26 characters long, applying it twice will result
  in s. That is ROT13 (ROT13 s) == s
-}

-- The default isLetter function will also be true for Chinese letters etc.
isLatinChar :: Char -> Bool
isLatinChar c = c `elem` ['a'..'z'] ++ ['A'..'Z']

addToChar :: Char -> Int -> Char
addToChar c n
  | not (isLatinChar c) = c
  | isUpper c = chr (((ord c + n - ord 'A') `mod` 26) + ord 'A')
  | isLower c = chr (((ord c + n - ord 'a') `mod` 26) + ord 'a')

rot13 :: String -> String
rot13 = map (`addToChar` 13)

 -- Properties on rot13 that can be checked using quickCheck
prop_sameLength :: String -> Bool
prop_sameLength s = length s == length (rot13 s)

prop_decode :: String -> Bool
prop_decode s = s == (rot13 . rot13) s

{-
  TEST REPORT

  The properties hold for all 100 tests
-}

-- Assignment 7 --
-- Move the first 4 characters to the end of the String
ibanMoveChars :: String -> String
ibanMoveChars (a:b:c:d:xs) = xs ++ [a,b,c,d]
ibanMoveChars x = x

-- Change all Alpha characters in the String to numeric characters, per rot13
-- e.i 'A' -> 10, 'B' -> 11, etc.
ibanAlphaToNumChar :: String -> String
ibanAlphaToNumChar [] = []
ibanAlphaToNumChar (x:xs)
  | isLetter x = show (ord x - ord 'A' + 10) ++ ibanAlphaToNumChar xs
  | otherwise = x : ibanAlphaToNumChar xs

-- True if the input is a valid IBAN number
iban :: String -> Bool
iban s
  | length s < 15  = False
  | otherwise = read (ibanAlphaToNumChar (ibanMoveChars s)) `mod` 97 == 1

-- A list of test data with valid and invalid IBAN numbers. The second argument
-- indicates whether the IBAN is valid.
ibanTestData = [("NL76RABO0301184948", True), ("NL76RABO0301184942", False),
  ("", False), ("NO8330001234567", True), ("NO833000123456", False),
  ("123", False), ("\1f643", False), ("AAAAA", False)]

{-
  Test whether the iban function is working correctly, by inserting a list of
  String-Boolean pairs, where the boolean indicates whether the string is a valid
  IBAN
-}
ibanTest :: Int -> [(String, Bool)] -> IO ()
ibanTest n [] = print (show n ++ " tests passed")
ibanTest n ((i,v) : xs)  = if iban i == v then do
    print ("Passed test on " ++ i ++ ", valid IBAN: " ++ show v)
    ibanTest (n+1) xs
  else
    print ("Test failed on " ++ i ++ ", Valid IBAN: " ++ show (not v))


{-
  TEST REPORT

  The test succeeds for the given input.
-}

main :: IO ()
main = do
  putStrLn "-- Lab 2 Team 6 --"
  putStrLn "\nAssignment 1"
  putStrLn "Quartile distribution on 10000 random generated floats ranged (0, 1)"
  quartileTest
  putStrLn "\nAssignment 2"
  putStrLn "Tests whether my triangle classifier gives the correct result for a valid list"
  putStrLn ("Passed all tests: " ++ show (testTriangles testValuesTriangles))
  putStrLn "\nAssignment 3"
  putStrLn "Tests whether the sorted properties are indeed in the correct order of strength"
  sortedProbsTest
  putStrLn "\nAssignment 4"
  putStrLn "Tests whether the isPermutation function bahaves as expected"
  quickCheck permutationsTest
  putStrLn "\nAssignment 5"
  putStrLn "Tests whether the isDerangement function behaves as expected"
  quickCheck isDerangementTest
  putStrLn "\nAssignment 6"
  putStrLn "Tests whether the iban function behaves as expected"
  ibanTest 0 ibanTestData
