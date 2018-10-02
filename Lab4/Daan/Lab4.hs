module Lab4 where

import Data.List
import Data.Function
import Control.Monad.Fix
import System.Random
import Test.QuickCheck
import SetOrd

{-
    Assignment 1

    Exercise 4.8)
    I understand that "elem 1 1" will not run. Although why would
    the error be "[a] is not an instance of class "Num""?

    It shouldn't be an instance of class num, because it should be
    a list of instances of class num, right?

    1h
-}

{-
    Assignment 2 (1h30)
-}

-- Set generator using QuickCheck
genSet :: (Arbitrary a, Ord a) => Int -> Gen (Set a)
genSet n = fmap list2set arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = sized genSet

-- Set generator from scratch
genSet' :: Int -> IO (Set Int)
genSet' n = fmap list2set (genRandomIntList n)

-- Generate a random integer between -n and n
genRandomInt :: Int -> IO Int
genRandomInt n = getStdRandom (randomR (-n, n))

-- Generate a list with random integers
genRandomIntList :: Int -> IO [Int]
genRandomIntList 0 = return []
genRandomIntList n = do
    x <- genRandomInt 20
    xs <- genRandomIntList (n-1)
    return (x:xs)

{-
    Assignment 3 (1h)
-}
-- Compute the intersect of 2 sets using the list intersect function
setIntersect :: (Eq a, Ord a) => Set a -> Set a -> Set a
setIntersect (Set a) (Set b) = list2set (intersect a b)

-- Test the intersect implementation
testIntersect :: (Eq a, Ord a) => Set a -> Set a -> Bool
testIntersect a b = testIntersectCheck (setIntersect a b) a b

-- Check whether all elements from the intersection are both in A and B
testIntersectCheck :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
testIntersectCheck (Set i) a b = all (\x -> inSet x a && inSet x b) i

-- Compute the union of 2 sets using the list union function
setUnion :: (Eq a, Ord a) => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = list2set (union a b)

-- Test the union implementation
testUnion :: (Eq a, Ord a) => Set a -> Set a -> Bool
testUnion a b = testUnionCheck (setUnion a b) a b

-- Check whether all element from the union are either in A or B
testUnionCheck :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
testUnionCheck (Set i) a b = all (\x -> inSet x a || inSet x b) i

-- Compute the difference of two sets
setDifference :: (Eq a, Ord a) => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set (a \\ b)

-- Test the difference implementation
testDifference :: (Eq a, Ord a) => Set a -> Set a -> Bool
testDifference a b = testDifferenceCheck (setDifference a b) a b

-- Check whether all elements are taken from A and not from B
testDifferenceCheck :: (Eq a, Ord a) => Set a -> Set a -> Set a -> Bool
testDifferenceCheck (Set i) a b = all (\x -> inSet x a && not(inSet x b)) i

-- Test function without quickcheck
noQCTest :: Int -> Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
noQCTest ct mt siz test =   if ct == mt then print (show ct ++ " tests passed")
                            else do
                                s1 <- genSet' siz
                                s2 <- genSet' siz
                                if test s1 s2 then do
                                    print("pass on: " ++ show(s1) ++ "  " ++ show(s2))
                                    noQCTest (ct+1) mt siz test
                                else error("failed test on: " ++ show(s1) ++ "  " ++ show(s2))

{-
    All the test functions listed above can be check using this test function.
    In order to use it, the following paramters must be provided:
    ct: Current count (should be 0)
    mt: Max count (max amount of tests)
    siz: Size of the test set
    test: One of the three tests mentioned before

    When running one of the tests 5 time we get the following output:
    *Lab4> noQCTest 0 5 5 testIntersect
    "pass on: { -13,-5,-2,13,18}  { -19,-16,-10,-4,6}"
    "pass on: { -15,-12,4,13,16}  { -17,-15,-13,-7,2}"
    "pass on: { -17,-14,-9,-3,6}  { -17,-14,-10,5}"
    "pass on: { -3,-2,-1,16}  { -14,-4,3,15,17}"
    "pass on: { -19,-8,-7,-5,-4}  { -19,-8,0,9}"
    "5 tests passed"
-}

{-
    Assignment 4
    Read chapter five of the Haskell Road
-}

type Rel a = [(a, a)]

-- Operator which returns transitions
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

{-
    Assignment 5 (10min)
    Symmetric closure
-}

-- Computes the symmetric closure by adding the inverse of the relation
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((a, b):rs) = sortBy sortRel (nub ((a, b):(b, a):(symClos(rs))))

-- Sorting function for sortBy with relations
sortRel (a1, a2) (b1, b2)
    | a1 < b1 = LT
    | a1 > b1 = GT
    | a1 == b1 = compare a2 b2

{-
    Assignment 6 (1h30)
    Transitive closure
-}

-- Composition of two relations

-- Computes the transitive closure using the formula R+ = U (R1, R2, .. Ri) where
    -- R(i+1) = R o R(i)
trClos :: Ord a => Rel a -> Rel a
trClos r = sortBy (on compare fst) (trClosHelper r r) where
    trClosHelper = fix(\f br cr -> if cr == union cr (rNext br cr) then cr else f br (rNext br cr)) where
        rNext br cr = union (br @@ cr) br

{-
    Assignment 7
    Properties for Symmetric and Transitive
-}
-- Check whether R is a subset of it's transitive closure
prop_trSubset :: Ord a => Rel a -> Bool
prop_trSubset r = all (\x -> elem x (trClos r)) r

-- Check whether the transitive closure is transitive
prop_trTrans :: Ord a => Rel a -> Bool
prop_trTrans [] = True
prop_trTrans r = and [ trans pair r | pair <- r] where
    trans (x, y) rl = and [elem (x, v) rl | (u, v) <- rl, u == y]

-- Check whether the transitive closure of r is valid
checkTrClos :: Ord a => Rel a -> Bool
checkTrClos r = (prop_trTrans . trClos) r && prop_trSubset r

trClosTest = quickCheckResult(checkTrClos :: (Rel Int -> Bool))

{-
    The test created above checks whether the transitive closure matches
    the listed properties, which are:
    - R should be a subset of R+
    - R+ should be transitive

    Actually a third property should be checked to fully validate whether
    the implementation is correct, which is that R+ is the smallest transitive
    closure. However I coulnd't manage to find out how to check that succesfully.

    Runing the test above yields the following result:
    *Lab4> trClosTest
    +++ OK, passed 100 tests.
    Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}

    During the test it starts to slow down occasionally, which can be explained
    by the fact that it has to loop over the lists multiple times which can be
    time consuming when dealing with big lists.
-}

-- Check whether the symmetric closure is equal to the union of R and R inversed
prop_symRevUnion :: Ord a => Rel a -> Bool
prop_symRevUnion r =  symClos r == symRevUnion r

symRevUnion :: Ord a => Rel a -> Rel a
symRevUnion r = sortBy sortRel (nub (union r (map (\(x, y) -> (y, x)) r)))

symClosTest = quickCheckResult(prop_symRevUnion :: (Rel Int -> Bool))

{-
    The test above checks whether the symmetric closure contains the property:
    - The symmetric closure of R is the union of R and it reverse

    Testing this is not really effective as this same property is used to create
    the closure in the first place.

    When running the test the result is as follows:
    *Lab4> symClosTest
    +++ OK, passed 100 tests.
    Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-}

{-
    Assignment 8
    The symmetric closure of the transitive closure is equivalent to the 
    transitive closure of the symmetric closure. If the symmetric closure is
    done last it simply adds the inverse of the symmetric closure to the
    relation. If the transitive closure is done first it will perform this
    closure over the inverse of the relation as well, which yields the same
    result. It can be proven using the following function.
-}

testSymTrans :: Ord a => Rel a  -> Bool
testSymTrans r = (symClos . trClos) r == (trClos . symClos) r