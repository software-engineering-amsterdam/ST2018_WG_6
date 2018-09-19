
module Lab3 where

import Data.List
import Data.Set
import Data.Char
import Test.QuickCheck
import Lecture3

-- Assignment 1 (15:30-)
toUniques :: Ord a => [a] -> [a]
toUniques = toList . fromList

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

contradiction :: Form -> Bool
contradiction = not . satisfiable

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

-- TODO Test and report
