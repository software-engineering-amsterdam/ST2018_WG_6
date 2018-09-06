module Lab1 where
import Data.List
import Data.Char
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial n = if n < 2 then 1 else n * factorial (n-1)

-- Assignment 1 (15 minutes)
sumSquares, sumSquares2 :: Int -> Int
sumSquares n = sum (map (^2) [1..n])
sumSquares2 n = quot (n * (n + 1) * (2 * n + 1)) 6
test11 = quickCheckResult (\ n -> n > 0 --> sumSquares n == sumSquares2 n)

sumThirdPower, sumThirdPower2 :: Int -> Int
sumThirdPower n = sum (map (^3) [1..n])
sumThirdPower2 n = quot (n * (n + 1)) 2 ^ 2
test12 = quickCheckResult (\ n -> n > 0 --> sumThirdPower n == sumThirdPower2 n)

-- Assignment 2 (30 minutes)
powerSetCardinality :: Int -> Int
powerSetCardinality n = length (subsequences [1..n])
test2 = quickCheckResult (\ n -> n > 0 && n < 25 --> powerSetCardinality n == 2 ^ n)

-- Questions:
-- Is this property hard to test? If you find that it is, can you given a reason why?
--   Yes, because higher n's take a lot more time to compute. We decided to only test
--   n's under 25 to keep test times manageable.

-- Give your thoughts on the following issue: when you perform the test for exercise 4,
-- what are you testing actually? Are you checking a mathematical fact? Or are you testing
-- whether subsequences satisfies a part of its specification? Or are you testing something
-- else still?
--   When we test this, we are mainly testing a mathematical fact. Since the subsequences
--   function is part of Haskell, we can assume this has been properly tested and functions as expected.
--   Because of this I would definitely say that we are not testing the subsequences function.
--   However, we are also testing our implementation, and testing that we are correctly using these
--   functions.

-- Assignment 3 (50 minutes)
permutationCount :: Int -> Int
permutationCount n = length (perms [1..n])
test3 = quickCheckResult (\ n -> n > 0 && n < 10 --> permutationCount n == factorial n)

-- Questions:
-- Is this property hard to test? If you find that it is, can you given a reason why?
--   Yes, because higher n's take a lot more time to compute. We decided to only test
--   n's under 10 to keep test times manageable.

-- Again, give your thoughts on the following issue: when you perform the test for
-- exercise 5, what are you testing actually? Are you checking a mathematical fact?
-- Or are you testing whether perms satisfies a part of its specification? Or are you
-- testing something else still?
--   In this test, we are testing quite a few different things. Because we implemented
--   the perms function ourselves, we are partially testing that it behaves as expected.
--   At the same time we are testing that the formula we found is correct, and we are testing
--   the implementation of the functions. Because we are testing these things simultaneously,
--   it is difficult to understand the meaning of a test failure. Therefore it would
--   probably be better to test the specifications of the implemented functions
--   separately from the found formula and its implementation.

-- Assignment 4 (30 minutes)
reversalPrimes :: [Integer]
reversalPrimes = filter (prime . reversal) primes

-- Questions:
-- How would you test this function, by the way?
--   It could be tested by reversing all primes in the list and verifying that
--   these reversals are also primes, but this would be done with the same functions
--   that also make up the original function (prime, reversal), so unless the test
--   function would use a different method of verifying this property, it would not
--   really be testing anything. We could store the (pre-known) list of reversal
--   primes and check that this list is the same as the output of the implemented
--   function, but this also seems a bit cumbersome.

-- Assignment 5 (15 minutes)
primeSum :: Int -> Int -> Integer
primeSum start size = sum (take size (drop start primes))

primeSums101 :: [Integer]
primeSums101 = map (`primeSum` 101) [0..]

primeSums101ThatArePrime :: [Integer]
primeSums101ThatArePrime = filter prime primeSums101

smallestPrimeSum101ThatIsPrime = head primeSums101ThatArePrime

-- Questions:
-- Do you have to test that your answer is correct? How could this be checked?
--   Tjarco has a good answer to this question.

-- Assignment 6 (30 minutes)
primeProducts :: [[Integer]]
primeProducts = map (`take` primes) [1..]

primeProductsThatAreNotPrime :: [[Integer]]
primeProductsThatAreNotPrime = filter (\p -> not (prime (product p + 1))) primeProducts

-- Assignment 7 (1.5 hours)
integerToDigits :: Integer -> [Int]
integerToDigits = map digitToInt . show

digitsToInteger :: [Int] -> Integer
digitsToInteger = read . concatMap show

sumDigits :: Int -> Int
sumDigits = sum . integerToDigits . toInteger

doubleAndSum :: Int -> Int
doubleAndSum = sumDigits . (* 2)

digitsToSumDigits :: [Int] -> [Int]
digitsToSumDigits [] = []
digitsToSumDigits (first:second:rest) = first : doubleAndSum second : digitsToSumDigits rest
digitsToSumDigits (first:rest) = first : digitsToSumDigits rest

getSumDigits :: Integer -> [Int]
getSumDigits = reverse . digitsToSumDigits . reverse . integerToDigits

computeCheckSum :: [Int] -> Int
computeCheckSum = (`mod` 10) . (* 9) . sum . init

luhn :: Integer -> Bool
luhn n = computeCheckSum sumDigits == last sumDigits
  where sumDigits = getSumDigits n

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = length digits == 15 && firstTwoNums `elem` [34, 37] && luhn n where
  digits = integerToDigits n
  firstTwoNums = digitsToInteger (take 2 digits)

isMaster :: Integer -> Bool
isMaster n = length digits == 16 && (firstTwoNums `elem` [51..55] || firstFourNums `elem` [2221..2720]) && luhn n where
  digits = integerToDigits n
  firstTwoNums = digitsToInteger (take 2 digits)
  firstFourNums = digitsToInteger (take 4 digits)

isVisa :: Integer -> Bool
isVisa n = length digits `elem` [13, 16..19] && head digits == 4 && luhn n where
  digits = integerToDigits n

-- TODO: Write tests

-- Assignment 8 (1.25 hours)
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew Matthew = False
accuses Matthew Carl = False
accuses Matthew other = True

accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter other = False

accuses Jack other = not (accuses Matthew other) && not (accuses Peter other)
accuses Arnold other = accuses Matthew other /= accuses Peter other
accuses Carl other = not (accuses Arnold other)

accusers :: Boy -> [Boy]
accusers boy = filter (`accuses` boy) boys

guilty, honest :: [Boy]
guilty = filter (\ b -> length (accusers b) >= 3) boys
honest = head (filter (\ a -> length a >= 3) (map accusers boys))
