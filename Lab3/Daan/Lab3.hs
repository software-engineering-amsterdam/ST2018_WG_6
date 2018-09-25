module Lab3 where

import Data.List
import Data.Char
import Control.Monad
import System.Random
import Test.QuickCheck
import Lecture3

----------- Assignment 1 (2h30) ---------------
{-
    Contradiction
    A formula is a contradiction when all valuations evaluate to false.
    Therefore, a contradiction is the opposite of a satisfiable function, thus a
    contradiction is the negation of the satisfiable function.
-}
contradiction :: Form -> Bool
contradiction f = (not . satisfiable) f

{-
    Tautology
    A formula is a tautology when all valuations evaluate to true. Therefore
    if we alter the satisfiable function by changing any -> all, we check if
    all valuations evaluate to true.
-}
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

{-
    Entailment
    A formula p is entailed by a formula q when all valuations of p that evaluate
    to true also evaluate to true in formula q.
    In order to indicate whether a formula is entailed by the other a couple steps
    are undertaken. First the all valuations of p are filtered by whether they evaluate
    to true. Then, this set of valuations is then matched against all valuations of q.
    For each valuation of p that check true it is checked whether it is a subset of
    any valuation of q. If this is the case for all valuations of p we can speak
    of logical entailment.

    After finishing this code it was found out that logical entailment can also
    be found by checking whether formula p implies formula q.

-}
entails :: Form -> Form -> Bool
entails p c = all (\v -> evl v c) (entailValuations p c)

-- Chains the two filter functions below
entailValuations :: Form -> Form -> [Valuation]
entailValuations p c = foldr entailFilterConcl (allVals c) (filterTruthTable True p (allVals p))

-- Filters the truth table of valuations by a specified value (True/False).
filterTruthTable :: Bool -> Form -> [Valuation] -> [Valuation]
filterTruthTable _ _ [] = []
filterTruthTable b f (v:vs)
    | (evl v f) == b = (v:filterTruthTable b f vs)
    | otherwise = filterTruthTable b f vs

-- Retrieves all the valuations of the conclusion that match those of Valuations1
entailFilterConcl :: Valuation -> [Valuation] -> [Valuation]
entailFilterConcl _ [] = []
entailFilterConcl sub (x:xs)
    | elem sub (subsequences x) = (x: (entailFilterConcl sub xs))
    | otherwise = entailFilterConcl sub xs

{-
    Equivalence
    The equivalence of two formula is indicated by whether formula 1 implies formula 2
    in conjuction with formula 2 implying formula 1. This conjunction should be a tautology,
    thus each valuation of this formula should check true.
-}
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Cnj [Impl f1 f2, Impl f2 f1])

------------ Assignment 2 - (4h) ---------------
-- Generate the conjunction p1 and p2
genCnj :: Form -> Form -> Form
genCnj p1 p2 = Cnj [p1, p2]

-- Generate the disjunction p1 or p2
genDsj :: Form -> Form -> Form
genDsj p1 p2 = Dsj [p1, p2]

{-
    Formula Generator
    The function below is used to generate a random formula with a maximum depth
    of 12 and a maximum of 4 properties. Each recursion the generator creates either
    a property, negation, conjunction, disjunction, imply or equivalence form type.
    If the maximum depth has been reached the generated unit is always an atom/Prop.
-}
genForm :: Int -> Gen Form
genForm n
    | n == 0 = fmap Prop (choose(1, 5))
    | n > 12 = genForm 12
    | n > 0 = oneof [
        fmap Prop (choose(1, 5)),
        fmap Neg (genForm(n-1)),
        liftM2 genCnj (genForm (n-1)) (genForm (n-1)),
        liftM2 genDsj (genForm (n-1)) (genForm (n-1)),
        liftM2 Impl (genForm (n-1)) (genForm (n-1)),
        liftM2 Equiv (genForm (n-1)) (genForm (n-1))
    ]

instance Arbitrary Form where
    arbitrary = sized genForm

{-
    Parser Test
    The following test is used to check the validity of the parser. If the parser
    is properly functioning its output should be the same formula as is put into it
    as a string. Therefore the following predicate should check true. The quickcheck
    test below uses the Form Generator to test various formulas against the parser test.
-}
doTestParser :: Form -> Bool
doTestParser f = (show . head. parse. show) f == show f

testParser = quickCheckResult(doTestParser)

-- Assignment 3 (1h)

{-
    Valuation to Disjunction
    The function below is used to transform a valuation into a disjunction for the
    CNF transformer. For each valuation, thus a list of (Name, Bool) a property is
    created and added to a list for the disjunction. The values are inverted, thus if
    the bool of the valuation equals true the resulting property is a negation of the
    property. Likewise if a valuation value equals false the un-negated property is
    added.
    
-}
val2Dsj :: Valuation -> [Form]
val2Dsj [] = []
val2Dsj ((i, b):vs)
    | b = ( (Neg (Prop i)) : val2Dsj vs)
    | otherwise = ( Prop i : val2Dsj vs)

-- The valFalse function returns a list of valuations which evaluate to false.
valFalse :: Form -> [Valuation]
valFalse f = filterTruthTable False f (allVals f)

{-
    Form to Conjunctive Normal Form
    This function can be used to transformate formulas into conjunctive normal
    form. This is done using the "Truth Table Method". This mean that the truth
    table of the formula is evualated and the CNF formula is created from this.
    Each property of a valuation that returns false is negated and combined in a
    disjunction. The resulting formula is then a conjunction of all disjunctions of
    inverted valuations.
-}
form2CNF :: Form -> Form
form2CNF f = Cnj (map (Dsj . val2Dsj) (valFalse f))

-- Assignment 4 (30min)
{-
    The form2CNF function can easily be checked with the generator built for
    assignment 2. If the result of form2CNF is equivalent to the original function
    we can state that it works succesful.
-}

{-
    The result from the form2CNF will be checked against three properties,
    which are defined below. The properties are:
    1) all formulas in CNF must be arrowless
    2) all formulas in CNF must be in NNF
    3) all formulas in CNF must follow the grammar, as specified below
    4) all formulas are equivalent to their CNF counterparts
-}

-- 1) The formula is arrowless
isArrowFree :: Form -> Bool
isArrowFree f = (show . arrowfree) f == show f

-- 2) The formula is in NNF
isNNF :: Form -> Bool
isNNF f
    | isArrowFree f = (show . nnf) f == show f
    | otherwise = False

{-
    3) The formula follows the following grammar:
    Literal L ::= Prop | Neg Prop
    Clause  D ::= L | Dsj[L, D]
    Formula C ::= D | Cnj[D, C]
-}

-- A literal is a property or a negation of a property
isLiteral :: Form -> Bool
isLiteral (Prop x) = True
isLiteral (Neg (Prop x)) = True
isLiteral _ = False

-- A clause is a literal or a disjunction of literals
isClause :: Form -> Bool
isClause (Dsj x) = all isLiteral x
isClause f
    | isLiteral f = True
    | otherwise = False

-- The formula is a clause or a conjunction of clauses
isCNFForm :: Form -> Bool
isCNFForm (Cnj x) = all isClause x
isCNFForm f
    | isClause f = True
    | otherwise = False

-- 4) A formula is equivalent to the CNF-form of the formula
isCNFEqual :: Form -> Bool
isCNFEqual f = equiv f (form2CNF f)

{-
    The following test checks the four properties defined above. The result
    is that all tests pass. Therefore we can conclude that according to the
    properties as defined the form2CNF function succesfully translates a
    formula to CNF.

    *Lab3> testCNFProp
    +++ OK, passed 100 tests.
    Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-}
testCNFProp = quickCheckResult(\f -> 
    (isArrowFree . form2CNF) f && 
    (isNNF . form2CNF) f && 
    (isCNFForm . form2CNF) f &&
    (isCNFEqual f))