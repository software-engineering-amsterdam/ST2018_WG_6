module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Assignment 1 (2h30)
contradiction :: Form -> Bool
contradiction f = not (any (\v -> evl v f) (allVals f))

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- Logical Entailment
entails :: Form -> Form -> Bool
entails p c = all (\v -> evl v c) (entailValuations p c)

-- Chains the two filter functions below
entailValuations :: Form -> Form -> [Valuation]
entailValuations p c = foldr entailFilterConcl (allVals c) (entailFilterPrem p (allVals p))

-- Retrieves all the valuations of the premises that return true
entailFilterPrem :: Form -> [Valuation] -> [Valuation]
entailFilterPrem _ [] = []
entailFilterPrem f (v:vs)
    | evl v f = (v:entailFilterPrem f vs)
    | otherwise = entailFilterPrem f vs

-- Retrieves all the valuations of the conclusion that match those of Valuations1
entailFilterConcl :: Valuation -> [Valuation] -> [Valuation]
entailFilterConcl _ [] = []
entailFilterConcl sub (x:xs)
    | elem sub (subsequences x) = (x: (entailFilterConcl sub xs))
    | otherwise = entailFilterConcl sub xs