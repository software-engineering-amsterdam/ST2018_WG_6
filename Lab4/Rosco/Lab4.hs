module Lab4 where

import Data.List
import Data.Set (fromList, toList)
import Data.Tuple (swap)
import Data.Char
import Test.QuickCheck
import Control.Monad
import System.Random

import SetOrd

-- Assignment 2 (1 hour)
-- Using system random
randomIntSet :: Int -> IO (Set Int)
randomIntSet n = fmap list2set (randomIntList n)

randomIntList :: Int -> IO [Int]
randomIntList n = replicateM n randomIO

-- Using quickcheck
setGen :: (Arbitrary a, Ord a) => Int -> Gen (Set a)
setGen n = fmap list2set arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized setGen

-- Assignment 3 (30 minutes)
setLength :: Ord a => Set a -> Int
setLength (Set xs) = length xs

setIntersect :: Ord a => Set a -> Set a -> Set a
setIntersect (Set a) (Set b) = list2set (a `intersect` b)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (a `union` b)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set (a \\ b)

-- TODO: Needs more test properties/testing

testSetIntersect :: Ord a => Set a -> Set a -> Bool
testSetIntersect a b = subSet i a && subSet i b where i = setIntersect a b

testSetUnion :: Ord a => Set a -> Set a -> Bool
testSetUnion a b = subSet a u && subSet b u where u = setUnion a b

testSetDifference :: Ord a => Set a -> Set a -> Bool
testSetDifference (Set a) (Set b) = all (not . (`elem` b)) d where (Set d) = setDifference (Set a) (Set b)

-- Assignment 5 (15 minutes)
type Rel a = [(a, a)]

reverseRel :: Ord a => Rel a -> Rel a
reverseRel = map swap

symClos :: Ord a => Rel a -> Rel a
symClos r = toList . fromList $ r ++ reverseRel r
