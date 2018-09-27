module Lab4 where

import Data.List
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
