module Lab3 where

import Data.List
import Data.Char
import Test.QuickCheck
import Control.Monad
import Lecture3

-- Assignment 1 --
contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = tautology $ Impl f g

equiv :: Form -> Form -> Bool
equiv f g = tautology $ Equiv f g

conjList :: Form -> Form -> Form
conjList f1 f2 = Cnj [f1, f2]

disjList :: Form -> Form -> Form
disjList f1 f2 = Dsj [f1, f2]

formGen :: Int -> Gen Form
formGen n
  | n <= 0 = fmap Prop (choose (1,3))
  | n > 5 = formGen 5
  | otherwise = oneof [
      fmap Neg (formGen (n - 1)),                         -- Negation
      liftM2 disjList (formGen (n-1) ) (formGen (n-2)),   -- Disjunctive
      liftM2 conjList (formGen (n-1) ) (formGen (n-2)),   -- Conjunctive
      liftM2 Impl (formGen (n - 1)) (formGen (n - 2)),    -- Implication
      liftM2 Equiv (formGen (n - 1)) (formGen (n - 2))]   -- Equivalence

instance Arbitrary Form where
  arbitrary = sized formGen

prop_Satisfiable :: Form -> Property
prop_Satisfiable a = satisfiable a ==> not  (contradiction a)

prop_tautology :: Form -> Bool
prop_tautology a = tautology (Dsj [a, Neg a])

prop_tautology2 a = tautology a ==> a `equiv` Dsj [p, Neg p]

none :: (a->Bool) -> [a] -> Bool
none f = not . any f

prop_Contradiction :: Form -> Property
prop_Contradiction f = contradiction f ==> none (`evl` f) (allVals f)
prop_Contradiction2 f = contradiction (Cnj [f, Neg f])

-- Assignment 2 --
prop_Parse :: Form -> Bool
prop_Parse f = show (head $ parse (show f)) == show f

-- Assignment 3: 20 hours --

{-
  Make all conjunctions and disjunctions binary by applying the association
  rules. That is, Cnj [p, q, r] <=> Cnj [Cnj [p,q] r]
-}
flatten :: Form -> Form
flatten (Cnj (x:y:z:xs)) = Cnj [x, flatten (Cnj (y:z:xs))]
flatten (Cnj (x:y:xs)) = Cnj [flatten x, flatten y]
flatten (Dsj (x:y:z:xs)) = Dsj [x, flatten (Dsj (y:z:xs))]
flatten (Dsj (x:y:xs)) = Dsj [flatten x, flatten y]
flatten x = x

{-
  Remove redudent Cnj and Dsj. Example: Dsj [p, Dsj (q, r)] becomes Dsj [p, q, r]
  Preconditions: Arrowfree and Negated normal form
-}
association :: Form -> Form
association (Prop x) = Prop x
association (Neg (Prop x)) = Neg (Prop x)
association (Cnj x) = Cnj $ concatMap liftInnerCnj x
association (Dsj x) = Dsj $ concatMap liftInnerDsj x

liftInnerCnj :: Form -> [Form]
liftInnerCnj (Cnj x) = concatMap liftInnerCnj x
liftInnerCnj (Dsj x) = [association (Dsj (concatMap liftInnerDsj x))]
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
logicRules (Dsj x) = flatten $ Dsj (map logicRules x)

-- When we have reached an outer conjunction, make inner normal form
logicRules (Cnj x) = flatten $ Cnj (map innerRules x)

{-
  This function creates the inner part of the CNF. It lifts all conjunctions
  such that by means of association they will be outer conjunctions in the end.
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

{-
  Pre-parse a formula with the give arrowfree and nnf functions for the
  cnf algorithm
-}
pre :: Form -> Form
pre = flatten . nnf . arrowfree

{-
  Helper function to pre-parse a String
-}
preParse :: String -> Form
preParse = pre . head . parse

{-
  Turns the formula into CNF by applying logic rules until the formula is in
  CNF.
-}
cnf :: Form -> Form
cnf = association . until (isCnf . association) logicRules . pre


-- Assignment 4 --
{-
  The formula generator is already defined for assignment one. Below are
  the properties that can be used for testing the CNF algorithm
-}

prop_Temp_Cnf :: Form -> Bool
prop_Temp_Cnf = isCnf . association . logicRules . logicRules . logicRules
  . logicRules . logicRules . logicRules . pre

prop_Flatten :: Form -> Bool
prop_Flatten f = flatten f `equiv` f

prop_Pre :: Form -> Bool
prop_Pre f = pre f `equiv` f

prop_Association :: Form -> Bool
prop_Association f = (association . pre) f `equiv` f

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

{-
  TEST Report

  Both of the properties on the CNF test succeed, which means that the algorithm
  is working for formulas to turn them into CNF, while still being logically
  equivalent to the original formula

-}

-- Assignment 5 --
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

prop_Clauses_reverse_equiv :: Form -> Property
prop_Clauses_reverse_equiv f =
  (formLength . pre) f < 200 ==>
  clausesToForm (formToClauses (cnf f)) `equiv` f

{-
  TEST Report

  The property for testing whether applying the formToClauses function combined
  with the clausesToForm function will result in the same form, passes all tests
  in quickcheck.
-}
