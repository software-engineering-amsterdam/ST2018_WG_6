
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck hiding ((.||.), (.&&.), (.==.))
import Text.Show.Functions

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = f a && g a

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = f a || g a

(.==.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.==.) f g a = f a == g a


-- Assignment 1 (75 minutes)
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

proportionalLength :: [a] -> [a] -> Float
proportionalLength portion full = (fromIntegral (length portion)) / (fromIntegral (length full))

between :: Ord a => a -> a -> a -> Bool
between start end x = x >= start && x <= end

testProbs :: IO [Float] -> IO Bool
testProbs ioFloats = do
    floats <- ioFloats
    let quartiles = [
            filter ((> 0.00) .&&. (<= 0.25)) floats,
            filter ((> 0.25) .&&. (<= 0.50)) floats,
            filter ((> 0.50) .&&. (<= 0.75)) floats,
            filter ((> 0.75) .&&. (<= 1.00)) floats]
    return (all (between 0.23 0.27) (map (`proportionalLength` floats) quartiles))

-- TODO Report

-- Assignment 2 (60 minutes)
data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a <= 0 || b <= 0 || c <= 0 = NoTriangle
    | a + b < c || a + c < b || b + c < a = NoTriangle
    | a == b && b == c = Equilateral
    | a == b || a == c || b == c = Isosceles
    | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = Rectangular
    | otherwise = Other

testTriangle :: [Integer] -> Shape -> Bool
testTriangle (a:b:c:_) expected = triangle a b c == expected
testTriangle _ _ = False

triangleTestCases :: [([Integer], Shape)]
triangleTestCases = [
    ([-1, -1, -1], NoTriangle),
    ([1, 1, 5], NoTriangle),
    ([1, 1, 1], Equilateral),
    ([1, 1, 2], Isosceles),
    ([3, 4, 5], Rectangular),
    ([2, 3, 5], Other),
    ([2, 4, 5], Other)]

testTriangles :: Bool
testTriangles = all (uncurry testTriangle) triangleTestCases

-- TODO Report

-- Assignment 3 (30 minutes)
domain :: [Int]
domain = [(-10)..10]

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker   xs p q = stronger xs q p

p1, p2, p3, p4 :: Int -> Bool
p1 = even .&&. (> 3)
p2 = even .||. (> 3)
p3 = p1 .||. even
p4 = even

properties :: [Int -> Bool]
properties = [p1, p2, p3, p4]

sortByStrength :: (Int -> Bool) -> (Int -> Bool) -> Ordering
sortByStrength p q
    | stronger domain p q == weaker domain p q = EQ
    | stronger domain p q = GT
    | weaker domain p q = LT
    | otherwise = EQ

sortedProperties :: [Int -> Bool]
sortedProperties = sortBy (flip sortByStrength) properties

-- TODO Report

-- Assignment 4 (1.5 hour)
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = isSameLength a b && all (`elem` a) b && all (`elem` b) a

isSameLength :: [a] -> [a] -> Bool
isSameLength a b = length a == length b

permutationsTest :: [Int] -> Property
permutationsTest a = length a < 10 ==>
    all (\ b -> isSameLength a b --> isPermutation a b) (permutations a)

-- TODO Report

-- Assignment 5 (45 minutes)
hasNoSamePositions :: Eq a => [a] -> [a] -> Bool
hasNoSamePositions a b = isSameLength a b && all (\ i -> (a !! i) /= (b !! i)) [0..(length a - 1)]

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = isPermutation a b && hasNoSamePositions a b

derangementTest :: [Int] -> Property
derangementTest a = length a < 10 ==>
    all (\ b -> isPermutation a b && hasNoSamePositions a b --> isDerangement a b) (permutations a)
