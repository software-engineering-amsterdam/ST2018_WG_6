
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

---------------- Assignment 3 S(21:55)-15 ---------------
data PropName = P0 | P1 | P2 | P3 deriving (Eq, Show)

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

doTestStronger :: Integral a => [a-> Bool] -> [[Bool]]
doTeststronger [] = []
doTestStronger (x:[]) = [map (\ y -> stronger [-10..10] x y) props]
doTestStronger (x:xs) = ((map (\ y -> stronger [-10..10] x y) props):testStronger xs)