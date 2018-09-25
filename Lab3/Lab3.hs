
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
{-
  Make all conjunctions and disjunctions binary by applying the association
  rules. That is, Cnj [p, q, r] <=> Cnj [Cnj [p,q] r]
-}
associate :: Form -> Form
associate (Cnj (x:y:z:xs)) = Cnj [x, associate (Cnj (y:z:xs))]
associate (Cnj (x:y:xs)) = Cnj [associate x, associate y]
associate (Dsj (x:y:z:xs)) = Dsj [x, associate (Dsj (y:z:xs))]
associate (Dsj (x:y:xs)) = Dsj [associate x, associate y]
associate x = x

{-
  Remove redudent Cnj and Dsj. Example: Dsj [p, Dsj (q, r)] becomes Dsj [p, q, r]
  Preconditions: Arrowfree and Negated normal form
-}
flatten :: Form -> Form
flatten (Prop x) = Prop x
flatten (Neg (Prop x)) = Neg (Prop x)
flatten (Cnj x) = Cnj $ concatMap liftInnerCnj x
flatten (Dsj x) = Dsj $ concatMap liftInnerDsj x

liftInnerCnj :: Form -> [Form]
liftInnerCnj (Cnj x) = concatMap liftInnerCnj x
liftInnerCnj (Dsj x) = [flatten (Dsj (concatMap liftInnerDsj x))]
liftInnerCnj x = [x]

liftInnerDsj :: Form -> [Form]
liftInnerDsj (Dsj x) = concatMap liftInnerDsj x
liftInnerDsj (Cnj x) = [Cnj (concatMap liftInnerCnj x)]
liftInnerDsj x = [x]

{-
  This function will apply logic rules in order to reach a CNF. It will work
  towards an outer conjunction and then with the innerRules function to an
  inner disjunction of literals.

  It uses the following rules:
    - (p ^ q) v (r ^ s) = (p v r) ^ (p v s) ^ (q v r) ^ (q v s)
    - p v (q ^ r) = (p v q) ^ (p v r)
-}
logicRules :: Form -> Form
logicRules (Prop x) = Prop x
logicRules (Neg (Prop x)) = Neg (Prop x)
logicRules (Dsj [Cnj [x1, x2], Cnj [y1, y2]]) = logicRules $ Cnj
  [Dsj [x1, y1], Dsj [x1, y2], Dsj [x2, y1], Dsj [x2, y2] ]
logicRules (Dsj [x, Cnj [y, z]]) =  logicRules (Cnj [
  Dsj [x, y],
  Dsj [x, z]])
logicRules (Dsj [Cnj y, x]) = logicRules $ Dsj [x, Cnj y] -- Flip to match rule
logicRules (Dsj x) = associate $ Dsj (map logicRules x)

-- When we have reached an outer conjunction, make inner normal form
logicRules (Cnj x) = associate $ Cnj (map innerRules x)

{-
  This function creates the inner part of the CNF. It lifts all conjunctions
  such that by means of flatten they will be outer conjunctions in the end.
-}
innerRules :: Form -> Form
innerRules (Prop x) = Prop x
innerRules (Neg (Prop x))  = Neg (Prop x)
innerRules (Dsj [x, Cnj [y, z]]) = Cnj
  [Dsj [innerRules x, innerRules y], Dsj [innerRules x, innerRules z]]
innerRules (Dsj [Cnj x, y]) = innerRules $ Dsj [y, Cnj x]       -- Flip to match rule
innerRules (Dsj xs) = Dsj (map innerRules xs)
innerRules (Cnj x) = Cnj (map innerRules x)

{-
  Pre-parse a formula with the give arrowfree and nnf functions for the
  cnf algorithm
-}
pre :: Form -> Form
pre = associate . nnf . arrowfree

{-
  Turns the formula into CNF by applying logic rules until the formula is in
  CNF.
-}
cnf :: Form -> Form
cnf = flatten . until (isCnf . flatten) logicRules . pre

-- Assignment 4 (No extra time spent - already completed while doing assignment 1-3)

-- Generator
createCnj :: Form -> Form -> Form
createCnj first second = Cnj [first, second]

createDsj :: Form -> Form -> Form
createDsj first second = Dsj [first, second]

formGen :: Int -> Gen Form
formGen n
  | n <= 0 = fmap Prop (choose (1,3))
  | n > 5 = formGen 5
  | otherwise = oneof [
      fmap Neg (formGen (n - 1)),                         -- Negation
      liftM2 createDsj (formGen (n-1) ) (formGen (n-2)),   -- Disjunctive
      liftM2 createCnj (formGen (n-1) ) (formGen (n-2)),   -- Conjunctive
      liftM2 Impl (formGen (n - 1)) (formGen (n - 2)),    -- Implication
      liftM2 Equiv (formGen (n - 1)) (formGen (n - 2))]   -- Equivalence

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


{-
  Whether a formula is either a property or a negated property
-}
isLiteral :: Form -> Bool
isLiteral (Prop _) = True
isLiteral (Neg (Prop _)) = True
isLiteral _ = False

{-
  Tests if the disjunction only contains litterals
-}
isDsjOfLiterals :: Form -> Bool
isDsjOfLiterals (Dsj x) = all isLiteral x
isDsjOfLiterals x = isLiteral x

{-
  True if the formula is in cnf
-}
isCnf :: Form -> Bool
isCnf (Dsj x) = isDsjOfLiterals (Dsj x)
isCnf (Cnj x) = all isDsjOfLiterals x
isCnf x = isLiteral x

prop_Temp_Cnf :: Form -> Bool
prop_Temp_Cnf = isCnf . flatten . logicRules . logicRules . logicRules
  . logicRules . logicRules . logicRules . pre

prop_Flatten :: Form -> Bool
prop_Flatten f = associate f `equiv` f

prop_Pre :: Form -> Bool
prop_Pre f = pre f `equiv` f

prop_Association :: Form -> Bool
prop_Association f = (flatten . pre) f `equiv` f

{-
  Calculate the length of a formula
-}
formLength :: Form -> Int
formLength (Prop _) = 1
formLength (Neg x) = 1 + formLength x
formLength (Dsj x) = 1 + sum (map formLength x)
formLength (Cnj x) = 1 + sum (map formLength x)
formLength (Impl x y) = 1 + formLength x + formLength y
formLength (Equiv x y) = 1 + formLength x + formLength y

{-
  Tests whether the formula's that are transformed into cnf are logically
  equivalent to the original formula. Input is restriced to formula's of size
  200 to be able to run the test in reasonable time.
-}
prop_Cnf_Equiv :: Form -> Property
prop_Cnf_Equiv f = (formLength . pre) f < 200 ==> cnf f `equiv` f

prop_Cnf_Correct :: Form -> Property
prop_Cnf_Correct f =  (formLength . pre) f < 200 ==> isCnf $ cnf f

-- Bonus assignment 5: 30 minutes
type Clause  = [Int]
type Clauses = [Clause]

formToClause :: Form -> Clause
formToClause (Prop x) = [x]
formToClause (Neg (Prop x)) = [-x]
formToClause (Dsj x) = concatMap formToClause x

{-
  Change a form into clauses.

  Precondition: the form is in CNF
-}
formToClauses :: Form -> Clauses
formToClauses (Prop x) = [[x]]
formToClauses (Neg (Prop x)) = [[-x]]
formToClauses (Cnj x) = map formToClause x
formToClauses (Dsj x) = map formToClause x

clauseToForm :: Clause -> Form
clauseToForm x = Dsj $ map (\x -> if x < 0 then Neg (Prop (-x)) else Prop x) x

clausesToForm :: Clauses -> Form
clausesToForm x = Cnj $ map clauseToForm x

prop_Clauses_reverse_equiv :: Form -> Bool
prop_Clauses_reverse_equiv f = clausesToForm (formToClauses (cnf f)) `equiv` f

{-
  TEST Report

  The property for testing whether applying the formToClauses function combined
  with the clausesToForm function will result in the same form, passes all tests
  in quickcheck.
-}
