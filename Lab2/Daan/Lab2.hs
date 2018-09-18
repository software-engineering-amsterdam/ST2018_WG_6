
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- even :: Int -> Bool
-- even x = mod x 2 == 0

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

------------- Assignment 1 (2h30) -----------

-- Performs 100 tests to see if the distribution checks out
quartTest :: IO ()
quartTest = do
    res <- mapM (quartCheck) [1..100]
    let succes = length (filter (==True) res) in
        putStrLn((show succes) ++ " of 100 tests passed")

-- Creates a list of 10k random floats and runs the check
quartCheck :: a -> IO Bool
quartCheck _ = quartDivide (probs 10000) >>= quartCheckValid

-- Checks whether the list is properly random distributed
quartCheckValid :: [Int] -> IO Bool
quartCheckValid [] = do return(True)
quartCheckValid (x:xs) =
    if (abs (x - 2500) > 125) then return(False)
    else do
        ret <- quartCheckValid xs
        return (ret)

-- Counts the amount of numbers that fit the criteria
quartCount :: (Float -> Bool) -> IO [Float] -> IO Int
quartCount cond flist = length . filter cond <$> flist

-- Creates a list of the amount of numbers per quart
quartDivide :: IO [Float] -> IO [Int]
quartDivide flist = do
    q1 <- quartCount (\x -> x <= 0.25) flist
    q2 <- quartCount (\x -> x > 0.25 && x <= 0.5) flist
    q3 <- quartCount (\x -> x > 0.5 && x <= 0.75) flist
    q4 <- quartCount (\x -> x > 0.75) flist
    return (q1:q2:q3:q4:[])

{-
    Test Results:
    Generally the tests run almost perfect. Sometimes one or
    two cases fail but it seems that the function is overall
    properly random. Examples of test results:
    
    *Lab2> quartTest 
    100 of 100 tests passed
    *Lab2> quartTest 
    100 of 100 tests passed
    *Lab2> quartTest 
    98 of 100 tests passed
    *Lab2> quartTest 
    99 of 100 tests passed

-}

--------------- Assignment 2 (30min) --------------

{-
    Function which tests whether three side do not form an triangle
    If the sides are smaller than one, or the sum of two sides is
    smaller than the length of the third it is not a triangle,
    thus this function succeeds.
-}
isNotTriangle :: Integer -> Integer -> Integer -> Bool
isNotTriangle a b c
    | a < 1 || b < 1 || c < 1 ||
      a + b < c || a + c < b || c + b < a = True
    | otherwise = False

{-
    Function which determines whether three sides form an equilateral
    triangle. It succceeds if all sides are of equal length.
-}
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c
    | a == b && b == c = True
    | otherwise = False

{-
    Function which determines whether a triangle is rectangular.
    A triangle is rectangular if it follows pythagoras' algorithm.
-}
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c
    | a^2 + b^2 == c^2 = True
    | a^2 + c^2 == b^2 = True
    | b^2 + c^2 == a^2 = True
    | otherwise = False

{-
    Function which determines whether a triangle is Isosceles.
    This succeeds if two sides are of equal length and it is
    not an equilateral triangle.
-}
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c
    | a == b && not (isEquilateral a  b c) = True
    | b == c && not (isEquilateral a b c) = True
    | a == c && not (isEquilateral a b c) = True
    | otherwise = False

-- Function which returns the shape of a triangle
triangle :: [Integer] -> Shape
triangle (a:b:c:[])
    | isNotTriangle a b c = NoTriangle
    | isIsosceles a b c = Isosceles
    | isEquilateral a b c = Equilateral
    | isRectangular a b c = Rectangular
    | otherwise = Other

{-
    Examples of triangles which can be tested.
    These are either self-thought or plucked from internet.
-}
testNoTriangles = [[1, 2, 4], [1, 1, 10], [2, 4, 7]]
testIsosceles = [[2, 2, 3], [5, 5, 8], [3, 4, 4]]
testEquilateral = [[3, 3, 3], [6, 6, 6], [1, 1, 1]]
testRectangular = [[3, 4, 5], [5, 12, 13], [6, 8, 10]]

testTriangles :: IO ()
testTriangles = do
    putStrLn("No Triangles: " ++ show(testNoTriangles)++ " -> " ++ show(map triangle testNoTriangles))
    putStrLn("Isosceles : " ++ show(testIsosceles)++ " -> " ++ show(map triangle testIsosceles))
    putStrLn("Equilateral: " ++ show(testEquilateral)++ " -> " ++ show(map triangle testEquilateral))
    putStrLn("Rectangular: " ++ show(testRectangular)++ " -> " ++ show(map triangle testRectangular))

{-
    The function above runs the tests that determine whether the
    implementation is correct. The first list is designed to
    break the triangle equality, thus they should yield NoTriangle.
    The second list contains sides which follow triangle equality and
    have two equal sides. Thus all these should yield Isosceles.
    The third list has three equal sides, which should be very straightforward.
    The final list contains examples of rectangular triangles plucked from
    internet.

    Dynamically creating these triangles wouldn't be very effective
    since they would be created using the same rules as they are checked.
    Therefore some examples are created by heart or from internet. The result
    of the tests is as follows:
    
    *Lab2> testTriangles 
    No Triangles: [[1,2,4],[1,1,10],[2,4,7]] -> [NoTriangle,NoTriangle,NoTriangle]
    Isosceles : [[2,2,3],[5,5,8],[3,4,4]] -> [Isosceles,Isosceles,Isosceles]
    Equilateral: [[3,3,3],[6,6,6],[1,1,1]] -> [Equilateral,Equilateral,Equilateral]
    Rectangular: [[3,4,5],[5,12,13],[6,8,10]] -> [Rectangular,Rectangular,Rectangular]

-}

---------------- Assignment 3 (3h) ---------------
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

p0, p1, p2, p3 :: Integral a => (a -> Bool)
p0 = even
p1 = (\ x -> even x && x > 3)
p2 = (\ x -> even x || x > 3)
p3 = (\ x -> (even x && x > 3) || even x)

props :: Integral a => [a -> Bool]
props = [p0, p1, p2, p3]

propOrd :: Integral a => (a -> Bool) -> (a -> Bool) -> Ordering
propOrd p q
    | not (stronger [-10..10] p q) = LT
    | (stronger [-10..10] p q) && (stronger [-10..10] q p) = EQ
    | otherwise = GT

testStronger :: Integral a => [a -> Bool]
testStronger = sortBy propOrd props

------------------ Assignment 4 (5h) ------------------
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y = length x == length y && isPermutationElement x y

isPermutationElement :: Eq a => [a] -> [a] -> Bool
isPermutationElement [] y = True
isPermutationElement (x:xs) y = length (filter (==x) y) == 1 && isPermutationElement xs y

-- Pre-conditions:
-- Property 1) There are no duplicates in the list

-- Post-conditions:

-- Property 2) If p is a permutation of q then q is a permutation of p
property_assoc :: Eq a => [a] -> [a] -> Bool
property_assoc p q = isPermutation p q && isPermutation q p

-- Property 3) If p is a permutation of q then all permutations of p are
-- also a permutation of q.
property_allp :: Eq a => [a] -> [a] -> Bool
property_allp p q = all (isPermutation q) (permutations p)

-- Property 4) If p is a permutation of q then the permutations of p
-- also contains q
property_cont :: Eq a => [a] -> [a] -> Bool
property_cont p q = isPermutation p q && elem q (permutations p)

{-
    By assuming the list cannot contain any duplicates we are actually
    setting a pre-condition, making the original pre-condition stronger.

    This is currently as far as I got. It took quite some time to come
    up with some properties. I will look into quickCheck more in order
    to be able to build the quickCheck functions necessary for this.  
-}

------------------ Assignment 5 (1h30) ---------------
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = isPermutationElement x y && isDerangementElement x y

isDerangementElement :: Eq a => [a] -> [a] -> Bool
isDerangementElement [] [] = True
isDerangementElement _ [] = False
isDerangementElement [] _ = False
isDerangementElement (x:xs) (y:ys) = (x /= y) && (isDerangementElement xs ys)

deran :: (Enum a, Num a, Eq a) => a -> [[a]]
deran n = filter (\x -> isDerangement [0..(n-1)] x) (permutations [0..(n-1)])

-- Precondition:
-- Property 1) All elements in x are also present in y
prop_containsAllElements :: Eq a => [a] -> [a] -> Bool
prop_containsAllElements [] _ = True
prop_containsAllElements (x:xs) y = elem x y && prop_containsAllElements xs y

-- Property 2) Both lists are of equal length
prop_equalLength :: Eq a => [a] -> [a] -> Bool
prop_equalLength x y = length x == length y

-- Postcondition:
-- Property 3) The derangement of x -> y is equal to the derangement y -> x
prop_associative :: Eq a => [a] -> [a] -> Bool
prop_associative x y = isDerangement x y == isDerangement y x

{-
    This is also where this adventure ends. I think it is still very difficult
    to define properties for functions as these. The function is a checker so
    writing a checker to check the checker seems pretty counterintuitive. If the
    checker of the checker finds something the checker doesnt this should simply
    be added to the checker. Therefore this seems pretty strange...
-}

-------------------- Assignment 6 (2h) ---------------
{-
    SPECIFICATIONS:
    * ROT13 is an involution, thus: x == ROT13(ROT13(x))
    * All characters are flipped, which means they are 13 characters apart.
      Thus: forall c in x:
        ord c == ord(ROT13(c))+-13
    * The string length should stay the same, thus: len(x) == len(ROT13(x))
-}

inRange :: Int -> Int -> Int -> Bool
inRange min max x = x >= min && x <= max

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (c:s)
    | inRange 97 109 (ord c) || inRange 65 77 (ord c) = (chr (ord(c)+13):rot13 s)
    | inRange 110 122 (ord c) || inRange 78 90 (ord c) = (chr (ord(c)-13):rot13 s)
    | otherwise = []

-- Test whether the involution property is true
-- Input original String and Transformed String
prop_r13_inv :: [Char] -> [Char] -> Bool
prop_r13_inv s ts = s == rot13(ts)

-- Test whether the characters from both strings are 13 distanced
-- Input original String and Transformed String
prop_r13_dis :: [Char] -> [Char] -> Bool
prop_r13_dis [] [] = True
prop_r13_dis _ [] = False
prop_r13_dis [] _ = False
prop_r13_dis (c:s) (tc:ts) = abs (ord(c) - ord(tc)) == 13 && prop_r13_dis s ts

-- Test whether the string length is the same, not necessary with prop_r13_dis
prop_r13_len :: [Char] -> [Char] -> Bool
prop_r13_len s ts = length s == length ts

-- Combine the three properties above
prop_comb :: [Char] -> [Char] -> Bool
prop_comb s ts = prop_r13_inv s ts && prop_r13_dis s ts && prop_r13_len s ts

-- QuickCheck function to test the ROT13 implementation
testROT13 = quickCheckResult(\x -> (all isAsciiLower x) || (all isAsciiUpper x) --> prop_comb x (rot13 x))

--------------- Assignment 7 (1h45) -------------------

-- Check if the length of the iban not too large
ibanLength :: String -> Bool
ibanLength s = length s <= 34 && length s > 4

-- Check whether the country code are uppercase ASCII characters
ibanCC :: String -> Bool
ibanCC (cc1:cc2:_) = isAsciiUpper cc1 && isAsciiUpper cc2
ibanCC _ = False

-- Check whether the check digits are actually digits
ibanCD :: String -> Bool
ibanCD (_:_:cd1:cd2:_) = isDigit cd1 && isDigit cd2
ibanCD _ = False

-- Check whether the complete structure of the iban is valid
ibanStruct :: String -> Bool
ibanStruct s = ibanLength s && ibanCC s && ibanCD s

-- Move the first 4 characters to the end
ibanMove :: String -> String
ibanMove (c1:c2:c3:c4:cs) = cs ++ (c1:c2:c3:c4:[])

-- Convert all capital ASCII characters to numbers
ibanConvert :: String -> String
ibanConvert [] = []
ibanConvert (c:s)
    | isAsciiUpper c = show (ord(c)-55) ++ ibanConvert s
    | otherwise = (c:ibanConvert s)

-- Copmute the mod 97 operation on the String as a number
ibanCompute :: String -> Integer
ibanCompute s = mod (read s::Integer) 97

-- Combine the three operations above to check for a valid iban
iban :: String -> Bool
iban s = (ibanCompute . ibanConvert . ibanMove) s == 1

-- Testdata with three valid iban strings and three invalid strings
ibanTestData = [("DE89370400440532013000", True), ("DE89370433440532013011", False),
                ("AL35202111090000000001234567", True), ("THISISNOTANIBAN", False),
                ("AZ96AZEJ00000000001234567890", True), ("BE71096123456799", False)]

-- Test which tests the strings above
ibanTest :: [(String,Bool)] -> IO ()
ibanTest [] = return()
ibanTest ((i,v):xs)
    | iban i == v = putStrLn("Iban: " ++ show(i) ++ " succesfully checked " ++ show(v)) >> (ibanTest xs)
    | otherwise = putStrLn("(!!)Iban: " ++ show(i) ++ " failed the test") >> (ibanTest xs)