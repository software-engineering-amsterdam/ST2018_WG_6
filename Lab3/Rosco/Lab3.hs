
module Lab3 where

import Data.List
import Data.Set
import Data.Char
import Test.QuickCheck
import Control.Monad

import Lecture3

-- Assignment 1 (15:30-)
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

contradiction :: Form -> Bool
contradiction = not . satisfiable

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

-- TODO Test and report

formGen :: Int -> Gen Form
formGen n
  | n <= 0 = fmap Prop arbitrary
  | n > 5 = formGen 5
  | otherwise = oneof [
      fmap Neg (formGen (n - 1)),
      -- fmap Cnj [(formGen (n - 1)), (formGen (n - 2))],
      -- fmap Dsj [(formGen (n - 1)), (formGen (n - 2))],
      liftM2 Impl (formGen (n - 1)) (formGen (n - 2)),
      liftM2 Equiv (formGen (n - 1)) (formGen (n - 2))]


instance Arbitrary Form where
  arbitrary = sized formGen
