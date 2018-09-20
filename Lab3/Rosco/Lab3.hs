
module Lab3 where

import Data.List
import Data.Set hiding (map)
import Data.Char
import Test.QuickCheck
import Control.Monad

import Lecture3

-- Assignment 1 (2 hours)
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

contradiction :: Form -> Bool
contradiction = not . satisfiable

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

testTautology :: Form -> Bool
testTautology f = tautology (Dsj [f, Neg f]) && not (tautology (Cnj [f, Neg f]))

testContradiction :: Form -> Bool
testContradiction f = contradiction (Cnj [f, Neg f]) && not (contradiction (Dsj [f, Neg f]))

testEntails :: Form -> Property
testEntails f = not (contradiction f) ==> entails f (Dsj [f, Neg f]) && not (entails f (Cnj [f, Neg f]))

testEquiv :: Form -> Bool
testEquiv f = equiv f f && not (equiv f (Neg f))

{-
  We created tests using the formula generator. We created tautologies/contradictions
  for these generated forms by doing Dsj [f, Neg f] or Cnj [f, Neg f] to test tautology
  and contradiction. We tested entails by asserting `entails f True && not entails f False`.
  We tested equiv by asserting `equiv f f && not equiv f !f`. We noticed that this test for
  entails didn't work with contradictions, so we added not contracdiction as a precondition.
-}

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

-- Assignment 3 (2 hours)
v2f :: Valuation -> Form
v2f vs = Cnj (map (\ v -> if snd v then Prop (fst v) else Neg (Prop (fst v))) vs)

cnf :: Form -> Form
cnf f = Dsj [v2f v | v <- allVals f, evl v f]

{-
  We created a v2f function that changes a valuation to a formula asserting this
  valuation, eg: [(1, True), (2, False)] -> Cnj [Prop 1, Neg (Prop 2)].
  Then we created the cnf function that converts a formula to CNF. It constructs
  a truth table by evaluating every valuation for a formula, and using v2f for
  all valuations that evaluate to true. The different outputs of v2f are then
  combined into a single disjunction.
-}

-- Assignment 4 (No extra time spent - already completed while doing assignment 1-3)

-- Generator
createCnj :: Form -> Form -> Form
createCnj first second = Cnj [first, second]

createDsj :: Form -> Form -> Form
createDsj first second = Dsj [first, second]

formGen :: Int -> Gen Form
formGen n
  | n <= 0 = fmap Prop (choose (1, 5))
  | n > 10 = formGen 10
  | otherwise = oneof [
      fmap Neg (formGen (n - 1)),
      liftM2 createCnj (formGen (n - 1)) (formGen (n - 2)),
      liftM2 createDsj (formGen (n - 1)) (formGen (n - 2)),
      liftM2 Impl (formGen (n - 1)) (formGen (n - 2)),
      liftM2 Equiv (formGen (n - 1)) (formGen (n - 2))]

instance Arbitrary Form where
  arbitrary = sized formGen

{-
  We created a generator that generates formulas by recursively adding new formulas
  to the formula. It starts with a random n up to 10 and picks a random form type.
  The parameters of these form constructors are generated with the same generator,
  using (n - 1) or (n - 2) as the parameter to the generator. When n reaches 0 or lower,
  it inserts an atom and terminates the recursion.
-}

-- Tests/Properties

testCnf :: Form -> Bool
testCnf form = equiv form cnfForm && isCnf cnfForm where cnfForm = cnf form

isNnf :: Form -> Bool
isNnf (Prop x) = True
isNnf (Neg (Prop x)) = True
isNnf (Neg x) = False
isNnf (Dsj fs) = all isNnf fs
isNnf (Cnj fs) = all isNnf fs
isNnf (Impl x y) = isNnf x && isNnf y
isNnf (Equiv x y) = isNnf x && isNnf y

isArrowFree :: Form -> Bool
isArrowFree (Prop x) = True
isArrowFree (Neg x) = isArrowFree x
isArrowFree (Dsj fs) = all isArrowFree fs
isArrowFree (Cnj fs) = all isArrowFree fs
isArrowFree (Impl x y) = False
isArrowFree (Equiv x y) = False

hasNoDsj :: Form -> Bool
hasNoDsj (Prop x) = True
hasNoDsj (Neg x) = hasNoDsj x
hasNoDsj (Dsj fs) = False
hasNoDsj (Cnj fs) = all hasNoDsj fs
hasNoDsj (Impl x y) = hasNoDsj x && hasNoDsj y
hasNoDsj (Equiv x y) = hasNoDsj x && hasNoDsj y

hasNoCnjOfDsj :: Form -> Bool
hasNoCnjOfDsj (Prop x) = True
hasNoCnjOfDsj (Neg x) = hasNoCnjOfDsj x
hasNoCnjOfDsj (Dsj fs) = all hasNoCnjOfDsj fs
hasNoCnjOfDsj (Cnj fs) = all hasNoDsj fs
hasNoCnjOfDsj (Impl x y) = hasNoCnjOfDsj x && hasNoCnjOfDsj y
hasNoCnjOfDsj (Equiv x y) = hasNoCnjOfDsj x && hasNoCnjOfDsj y

isCnf :: Form -> Bool
isCnf f = isArrowFree f && isNnf f && hasNoCnjOfDsj f

{-
  We created isNnf, isArrowfree and hasNoCnjOfDsj to test that formulas
  are in NNF, are arrow free, and contain no conjunctions of disjunctions.
  We combined these properties into isCnf, which tests whether these three
  properties are true.
-}
