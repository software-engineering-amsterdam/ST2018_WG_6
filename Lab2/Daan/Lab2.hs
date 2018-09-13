
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- even :: Int -> Bool
-- even x = mod x 2 == 0

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

------------- Assignment 1 (1h15) -----------

test1 :: IO [Int]
test1 = doTest1 (probs 10000)

test1filter :: (Float -> Bool) -> IO [Float] -> IO Int
test1filter cond flist = length . filter cond <$> flist

doTest1 :: IO [Float] -> IO [Int]
doTest1 flist = do
    q1 <- test1filter (\x -> x <= 0.25) flist
    q2 <- test1filter (\x -> x > 0.25 && x <= 0.5) flist
    q3 <- test1filter (\x -> x > 0.5 && x <= 0.75) flist
    q4 <- test1filter (\x -> x > 0.75) flist
    return (q1:q2:q3:q4:[])

--------------- Assignment 2 (30min) --------------

isNotTriangle :: Int -> Int -> Int -> Bool
isNotTriangle a b c
    | a < 1 || b < 1 || c < 1 = True
    | otherwise = False

isEquilateral :: Int -> Int -> Int -> Bool
isEquilateral a b c
    | a == b && b == c = True
    | otherwise = False

isRectangular :: Int -> Int -> Int -> Bool
isRectangular a b c
    | a^2 + b^2 == c^2 = True
    | a^2 + c^2 == b^2 = True
    | b^2 + c^2 == a^2 = True
    | otherwise = False

isIsosceles :: Int -> Int -> Int -> Bool
isIsosceles a b c
    | a == b && not (isEquilateral a  b c) = True
    | b == c && not (isEquilateral a b c) = True
    | a == c && not (isEquilateral a b c) = True
    | otherwise = False

triangle :: Int -> Int -> Int -> Shape
triangle a b c
    | isNotTriangle a b c = NoTriangle
    | isIsosceles a b c = Isosceles
    | isEquilateral a b c = Equilateral
    | isRectangular a b c = Rectangular
    | otherwise = Other

---------------- Assignment 3-1 (3h) ---------------
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

p0, p1, p2, p3 :: Integral a => (a -> Bool)
p0 = even
p1 = (\ x -> even x && x > 3)
p2 = (\ x -> even x || x > 3)
p3 = (\ x -> (even x && x > 3) || even x)

props :: Integral a => [a -> Bool]
props = [p0, p1, p2, p3]

propOrd :: Integral a => (a -> Bool) -> (a -> Bool) -> Ordering
propOrd p q
    | not (stronger [-10..10] p q) = LT
    | (stronger [-10..10] p q) && (stronger [-10..10] q p) = EQ
    | otherwise = GT

testStronger :: Integral a => [a -> Bool]
testStronger = sortBy propOrd props
-- TODO Find out how the properties can be printed

------------------ Assignment 4 (2h) ------------------
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = length x == length y && isPermutationElement x y

isPermutationElement :: Eq a => [a] -> [a] -> Bool
isPermutationElement [] y = True
isPermutationElement (x:xs) y = length (filter (==x) y) == 1 && isPermutationElement xs y

-- TODO Figure out what properties to test

------------------ Assignment 5 (20min) ---------------
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = isPermutationElement x y && isDerangementElement x y

isDerangementElement :: Eq a => [a] -> [a] -> Bool
isDerangementElement [] [] = True
isDerangementElement _ [] = False
isDerangementElement [] _ = False
isDerangementElement (x:xs) (y:ys) = (x /= y) && (isDerangementElement xs ys)

deran :: (Enum a, Num a, Eq a) => a -> [[a]]
deran n = filter (\x -> isDerangement [0..(n-1)] x) (permutations [0..(n-1)])

-- TODO again, figure out what properties to test

-------------------- Assignment 6 (2h) ---------------
{-
    SPECIFICATIONS:
    * ROT13 is an involution, thus: x == ROT13(ROT13(x))
    * All characters are flipped, which means they are 13 characters apart.
      Thus: forall c in x:
        ord c == ord(ROT13(c))+-13
    * The string length should stay the same, thus: len(x) == len(ROT13(x))
-}

inRange :: Int -> Int -> Int -> Bool
inRange min max x = x >= min && x <= max

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (c:s)
    | inRange 97 109 (ord c) || inRange 65 77 (ord c) = (chr (ord(c)+13):rot13 s)
    | inRange 110 122 (ord c) || inRange 78 90 (ord c) = (chr (ord(c)-13):rot13 s)
    | otherwise = []

-- Test whether the involution property is true
-- Input original String and Transformed String
trot1 :: [Char] -> [Char] -> Bool
trot1 s ts = s == rot13(ts)

-- Test whether the characters from both strings are 13 apart
-- Input original String and Transformed String
trot2 :: [Char] -> [Char] -> Bool
trot2 [] [] = True
trot2 _ [] = False
trot2 [] _ = False
trot2 (c:s) (tc:ts) = abs (ord(c) - ord(tc)) == 13 && trot2 s ts

-- Test whether the string length is the same, not necessary with trot2
trot3 :: [Char] -> [Char] -> Bool
trot3 s ts = length s == length ts

-- TODO properties transform to (a -> Bool)
testROT13 = quickCheckResult(\x -> (all isAsciiLower x) || (all isAsciiUpper x) --> trot1 x (rot13 x))

--------------- Assignment 7 S(15:10) -------------------