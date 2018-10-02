
module Lab4 where

import Data.List
import Data.Char
import Test.QuickCheck
import Control.Monad
import System.Random

import SetOrd

----- Assignment 1 Reading time: 2 hours
-- I experienced issues with the halting problem and Russel Paradox. 
-- I did not really understand why the funny funny example halted or diverged.

-- Random integer in the range -n to n
rand :: Int -> IO Int
rand n = randomRIO (-n, n)

-- List of random integers of length l and in range -n to n 
randList :: Int -> Int -> IO [Int]
randList 0 0 = return []
randList l n = replicateM l $ rand n

-- Assignment 2 A
randSet :: (Arbitrary a, Ord a) => Int -> Gen (SetOrd.Set a)
randSet n = fmap list2set arbitrary

-- Assignment 2 B
randSet' :: Int -> Int -> IO (SetOrd.Set Int)
randSet' l n = fmap list2set (randList l n)


-- Assignment 3 --
intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set (xs `intersect` ys)

union' :: Ord a => Set a -> Set a -> Set a
union' (Set xs) (Set ys) = list2set (xs `union` ys)

difference :: Ord a => Set a -> Set a -> Set a
difference (Set set1) set2 = Set [x | x <- set1, not (x `inSet` set2)]


---- Assignment 4 Reading time: 1.5 hours
-- Chapter 5 was easier to understand and did not raise any questions.
-- Most likely due to the presence of a lot of images that clarified the text.

