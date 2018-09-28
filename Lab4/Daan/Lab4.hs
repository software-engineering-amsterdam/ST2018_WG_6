module Lab4 where

import Data.List
import Control.Monad.Fix
import System.Random
import Test.QuickCheck
import SetOrd

{-
    Assignment 1

    Exercise 4.8)
    I understand that "elem 1 1" will not run. Although why would
    the error be "[a] is not an instance of class "Num""?

    It shouldn't be an instance of class num, because it should be
    a list of instances of class num, right?

    30min -> Tot pagina 133 (pdf-144)
-}

{-
    Assignment 2 (1h30)
-}

-- Set generator using QuickCheck
genSet :: (Arbitrary a, Ord a) => Int -> Gen (Set a)
genSet n = fmap list2set arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized genSet

-- Set generator from scratch
genSet' :: Int -> IO (Set Int)
genSet' n = fmap list2set (genRandomIntList n)

genRandomInt :: Int -> IO Int
genRandomInt n = getStdRandom (randomR (-n, n))

genRandomIntList :: Int -> IO [Int]
genRandomIntList 0 = return []
genRandomIntList n = do
    x <- genRandomInt 20
    xs <- genRandomIntList (n-1)
    return (x:xs)

{-
    Assignment 3 (1h)
-}
-- Intersect
setIntersect :: (Eq a, Ord a) => Set a -> Set a -> Set a
setIntersect (Set a) (Set b) = list2set (intersect a b)

testIntersect :: (Eq a, Ord a) => Set a -> Set a -> Bool
testIntersect a b = testIntersectCheck (setIntersect a b) a b

testIntersectCheck :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
testIntersectCheck (Set i) a b = all (\x -> inSet x a && inSet x b) i

-- Union
setUnion :: (Eq a, Ord a) => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (union a b)

testUnion :: (Eq a, Ord a) => Set a -> Set a -> Bool
testUnion a b = testUnionCheck (setUnion a b) a b

testUnionCheck :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
testUnionCheck (Set i) a b = all (\x -> inSet x a || inSet x b) i

-- Difference
setDifference :: (Eq a, Ord a) => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set (a \\ b)

testDifference :: (Eq a, Ord a) => Set a -> Set a -> Bool
testDifference a b = testDifferenceCheck (setDifference a b) a b

testDifferenceCheck :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
testDifferenceCheck (Set i) a b = all (\x -> inSet x a && not(inSet x b)) i

-- Test function without quickcheck
noQCTest :: Int -> Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
noQCTest ct mt siz test =   if ct == mt then print (show ct ++ " tests passed")
                            else do
                                s1 <- genSet' siz
                                s2 <- genSet' siz
                                if test s1 s2 then do
                                    print("pass on: " ++ show(s1) ++ "  " ++ show(s2))
                                    noQCTest (ct+1) mt siz test
                                else error("failed test on: " ++ show(s1) ++ "  " ++ show(s2))

{-
    Assignment 4
    Read chapter five of the Haskell Road
-}

{-
    Assignment 5
    Symmetric closure
-}
type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((a, b):rs) = nub ((a, b):(b, a):(symClos(rs)))

{-
    Assignment 6
    Transitive closure
-}

-- Composition of two relations

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos r
    | r == union (r @@ r) r = sort r
    | otherwise = union (trClos (r @@ r)) r

-- rComp :: Ord a => Rel a -> Rel a
-- rComp r = union (r @@ r) r

{-
    Assignment 7
    Properties for Symmetric and Transitive
-}