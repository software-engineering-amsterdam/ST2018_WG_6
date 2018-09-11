module Lab1 where
import Data.List
import Test.QuickCheck    

------------ Helper code ----------------
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

------------- Assignment 1 (40min) --------------
-- The sum of all squares
sumsq :: Integer -> Integer
sumsq 1 = 1
sumsq n = n^2 + sumsq (n-1)

-- The formula as specified in the workshop
formsq :: Integer -> Integer
formsq n = quot (n*(n+1)*((2*n)+1)) 6

-- QuickCheck test to compare the formulas above
test1 = quickCheckResult(\a -> a > 0 --> sumsq a == formsq a)

-- The sum of all n^3
sump3 :: Integer -> Integer
sump3 1 = 1
sump3 n = n^3 + sump3 (n-1)

-- The formula as specified in the workshop
formp3 :: Integer -> Integer
formp3 n = (quot (n*(n+1)) 2)^2

-- QuickCheck test to compare the formulas above
test2 = quickCheckResult(\a -> a > 0 --> sump3 a == formp3 a)

--------------- Assignment 2 (15min) --------------
powlist :: Integer -> [[Integer]]
powlist a = subsequences [1..a]

test3 = quickCheckResult(\a -> ((a > 0) && (a <= 25)) --> (length (powlist a)) == (2^a))
{-
  The property is hard to test because quickcheck feeds numbers
  to the formula which are too large. This causes the check to slow
  down or crash. The solution is to limit the amount of numbers, but
  this reduces the value of the result.

  Assuming that subsequences returns a list which is equal to the powerlist
  this implementation is checking the mathematical fact.
-}

---------------- Assignment 3 (10min) ---------------
perlist :: Int -> [[Int]]
perlist n = permutations [1..n]

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * (factorial (n-1))

test4 = quickCheckResult(\a -> ((a > 0) && (a <=10)) --> (length (perlist a)) == (factorial a))
{-
  This property is hard to test in the same way that the previous property
  is hard to test. Again, assuming that the implementation of permutations
  is correct, this test is in fact testing the mathematical fact.
-}

---------------- Assignment 4 (15min) ----------------
-- Take the first 10000 primes which can be reversed
primerev :: [Integer]
primerev = take 10000 (filter prev primes)

-- returns whether the reversal of a prime is also a prime
prev :: Integer -> Bool
prev p = prime (reversal p)

{-
  This function could be tested by comparing the output from
  this function to a list of reversal primes which is proven
  to be correct.
-}

---------------- Assignment 5 (30min) ---------------
-- Find the first prime which is the combination of 101 consecutive primes
primecons :: Integer
primecons = primesum 0

-- Take 101 consecutive primes starting at index n
plist :: Int -> [Integer]
plist n = take 101 (drop n primes)

-- Find the list that matches the criteria and return the sum
primesum :: Int -> Integer
primesum n
  | prime p = p
  | otherwise = primesum (n+1)
  where p = sum (plist n)

{-
  Tjarco has a well formulated answer to this question
-}

---------------- Assignment 6 (5min) ---------------
-- Returns a counterexample to the provided conjecture
primeref :: Integer
primeref = primemul 1

-- Finds the first counterexample
primemul :: Int -> Integer
primemul n
  | not (prime p) = p
  | otherwise = primemul (n+1)
  where p = (product (take n primes)) + 1

----------------- Assignment 7 (2h) ---------------
-- Returns whether a number suffices for Luhns algorithm
luhn :: Integer -> Bool
luhn num = let numlist = reverse (integerToDigits num) in
  (mod (sum (luhn_double numlist)) 10) == 0

-- Double all seconds digits
luhn_double :: [Integer] -> [Integer]
luhn_double [] = []
luhn_double [x] = [x]
luhn_double (x1:x2:xs)
  | x2 > 4    = (x1:(x2*2)-9:luhn_double xs)
  | otherwise = (x1:(x2*2):luhn_double xs)

-- Listify integer numbers, found on stackoverflow
integerToDigits :: Integer -> [Integer]
integerToDigits n
  | n < 1 = []
  | otherwise = integerToDigits (div n 10) ++ [mod n 10]

-- Test whether a number fits an American Express card
isAmericanExpress :: Integer -> Bool
isAmericanExpress num = let numlist = integerToDigits num in
  ((take 2 numlist == [3, 4]) || (take 2 numlist == [3, 7])) &&
  (length numlist == 15) &&
  (luhn num)

-- Test whether a number fits a Visa card
isVisa :: Integer -> Bool
isVisa num = let numlist = integerToDigits num in
  (take 1 numlist == [4]) &&
  (length numlist == 16) &&
  (luhn num)

-- Test whether a number fits a Mastercard card
isMaster :: Integer -> Bool
isMaster num = let numlist = integerToDigits num in
  (length numlist == 16) &&
  ((inRange (div num 100000000000000) 51 55) || (inRange (div num 1000000000000) 2221 2720)) &&
  (luhn num)

-- Check whether a number is in the given range
inRange :: Integer -> Integer -> Integer -> Bool
inRange x min max = (x >= min) && (x <= max)

--------------  Assignment 8 (2h) ---------------
data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Encodes whether a boy accuses another boy
accuses :: Boy -> Boy -> Bool
accuses Matthew b
  | b == Carl = False
  | b == Matthew = False
  | otherwise = True

accuses Peter b
  | b == Matthew = True
  | b == Jack = True
  | otherwise = False

accuses Jack b = not (accuses Matthew b) && not (accuses Peter b)
accuses Arnold b = accuses Matthew b /= accuses Peter b
accuses Carl b = not (accuses Arnold b)

-- Returns a list of accusers per boy
accusers :: Boy -> [Boy]
accusers b = filter (\x -> accuses x b) boys

guilty, honest :: [Boy]
guilty = filter (\b -> (length (accusers b)) >= 3) boys
honest = accusers (head guilty)