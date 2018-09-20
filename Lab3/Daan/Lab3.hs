module Lab3 where

import Data.List
import Data.Char
import Control.Monad
import System.Random
import Test.QuickCheck
import Lecture3

-- Assignment 1 (2h30)
contradiction :: Form -> Bool
contradiction f = not (any (\v -> evl v f) (allVals f))

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- Logical Entailment - Afterwards found out that it can be done way easier.
-- See Rosco's solution.
entails :: Form -> Form -> Bool
entails p c = all (\v -> evl v c) (entailValuations p c)

-- Chains the two filter functions below
entailValuations :: Form -> Form -> [Valuation]
entailValuations p c = foldr entailFilterConcl (allVals c) (entailFilterPrem True p (allVals p))

-- Retrieves all the valuations of the premises that return true
entailFilterPrem :: Bool -> Form -> [Valuation] -> [Valuation]
entailFilterPrem _ _ [] = []
entailFilterPrem b f (v:vs)
    | (evl v f) == b = (v:entailFilterPrem b f vs)
    | otherwise = entailFilterPrem b f vs

-- Retrieves all the valuations of the conclusion that match those of Valuations1
entailFilterConcl :: Valuation -> [Valuation] -> [Valuation]
entailFilterConcl _ [] = []
entailFilterConcl sub (x:xs)
    | elem sub (subsequences x) = (x: (entailFilterConcl sub xs))
    | otherwise = entailFilterConcl sub xs

equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Cnj [Impl f1 f2, Impl f2 f1])

-- Assignment 2 - (4h)
genCnj :: Form -> Form -> Form
genCnj p1 p2 = Cnj [p1, p2]

genDsj :: Form -> Form -> Form
genDsj p1 p2 = Dsj [p1, p2]

genForm :: Int -> Gen Form
genForm n
    | n == 0 = fmap Prop (choose(1, 5))
    | n > 12 = genForm 12
    | n > 0 = oneof [
        fmap Neg (genForm(n-1)),
        liftM2 genCnj (genForm (n-1)) (genForm (n-1)),
        liftM2 genDsj (genForm (n-1)) (genForm (n-1)),
        liftM2 Impl (genForm (n-1)) (genForm (n-1)),
        liftM2 Equiv (genForm (n-1)) (genForm (n-1))
    ]

instance Arbitrary Form where
    arbitrary = sized genForm

-- Write parse test (!)

-- Assignment 3 (1h)
val2Dsj :: Valuation -> [Form]
val2Dsj [] = []
val2Dsj ((i, b):vs)
    | b = ( (Neg (Prop i)) : val2Dsj vs)
    | otherwise = ( Prop i : val2Dsj vs)

valFalse :: Form -> [Valuation]
valFalse f = entailFilterPrem False f (allVals f)

form2CNF :: Form -> Form
form2CNF f = Cnj (map (Dsj . val2Dsj) (valFalse f))

-- Assignment 4
testForm = quickCheckResult(\f -> equiv f (form2CNF f))