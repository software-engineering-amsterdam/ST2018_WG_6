module Lab6 where

import Lecture6
import Control.Monad
import Control.Applicative
import Data.List

-- Assignment 4

findIncorrectFermats :: Int -> Int -> IO [Integer]
findIncorrectFermats k n = filterM (primeTestsF k) (take n composites)

smallestIncorrectFermat :: Int -> IO Integer
smallestIncorrectFermat k = head <$> allIncorrectFermats 1000 where
    allIncorrectFermats 0 = return []
    allIncorrectFermats n = sort . nub <$> liftM2 (++) (findIncorrectFermats k 100) (allIncorrectFermats (n-1))

{-
k -> smallest incorrect fermat
1 -> 9
2 -> 9
3 -> 9
4 -> 15
5 -> 15
6 -> 15
7 -> 91
-}

-- Assignment 5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6 * k + 1),
      prime (12 * k + 1),
      prime (18 * k + 1) ]
