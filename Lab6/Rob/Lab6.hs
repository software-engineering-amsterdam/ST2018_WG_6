module Lab6 where

import Data.List
import System.Random
import Lecture6

-- Assignment 1 - 45 minutes
exM' :: Integer -> Integer -> Integer -> Integer
exM' b 0 m = 1
exM' b e m | even e      = (exM b (e `div` 2) m) ^ 2 `mod` m
           | otherwise   = ((exM b (e-1 `div` 2) m) ^ 2) * b `mod` m
