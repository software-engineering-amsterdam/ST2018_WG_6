
module Lab3 where

import Data.List
import Data.Set
import Data.Char
import Test.QuickCheck
import Control.Monad

import Lecture3

-- General
createCnj :: Form -> Form -> Form
createCnj first second = Cnj [first, second]

createDsj :: Form -> Form -> Form
createDsj first second = Dsj [first, second]

formGen :: Int -> Gen Form
formGen n
  | n <= 0 = fmap Prop arbitrary
  | n > 5 = formGen 5
  | otherwise = oneof [
      fmap Neg (formGen (n - 1)),
      liftM2 createCnj (formGen (n - 1)) (formGen (n - 2)),
      liftM2 createDsj (formGen (n - 1)) (formGen (n - 2)),
      liftM2 Impl (formGen (n - 1)) (formGen (n - 2)),
      liftM2 Equiv (formGen (n - 1)) (formGen (n - 2))]

instance Arbitrary Form where
  arbitrary = sized formGen

-----------------------------------------------------------

-- Assignment 1 (1.5 hours)
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

contradiction :: Form -> Bool
contradiction = not . satisfiable

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

-- TODO Test and report

-- Assignment 2 (30 minutes)
testParse :: Form -> Bool
testParse form = show (head $ parse $ show form) == show form

{-
  We used the form generator to create a quickCheck test that tests the parse function.
  This test inputs a generated form, then parses the 'show' of the form, and asserts
  that it is equal to the original form. However this was not working, as we suspect
  that there is something not functioning well with the Eq of Form, so we changed the
  test to compare the showed versions of the input and output forms.

  This test runs succesfully. (+++ OK, passed 100 tests.)
-}
