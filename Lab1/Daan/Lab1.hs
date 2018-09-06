module Lab1 where
import Data.List
import Test.QuickCheck    

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

-- Haskell formulation of exercise 2 -- 30min
-- The sum of all squares
form2a :: Integer -> Integer
form2a 1 = 1
form2a n = n^2 + form2a (n-1)

-- The formula as specified in the workshop
form2b :: Integer -> Integer
form2b n = quot (n*(n+1)*((2*n)+1)) 6

-- QuickCheck test to compare the formulas above
test1 = quickCheckResult(\a -> a > 0 --> form2a a == form2b a)

-- Haskell formulation of exercise 3 -- 10min
-- The sum of all n^3
form3a :: Integer -> Integer
form3a 1 = 1
form3a n = n^3 + form3a (n-1)

-- The formula as specified in the workshop
form3b :: Integer -> Integer
form3b n = (quot (n*(n+1)) 2)^2

-- QuickCheck test to compare the formulas above
test2 = quickCheckResult(\a -> a > 0 --> form3a a == form3b a)

-- Haskell formulation of exercise 4 -- 15min
test3 = quickCheckResult(\a -> ((a > 0) && (a <= 25)) --> (length (powlist a)) == (2^a))

powlist :: Integer -> [[Integer]]
powlist a = subsequences [1..a]

-- Haskell formulation of exercise 5 -- 10min
test4 = quickCheckResult(\a -> ((a > 0) && (a <=10)) --> (length (perlist a)) == (factorial a))

perlist :: Int -> [[Int]]
perlist n = permutations [1..n]

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * (factorial (n-1))

-- Prime and its reversal 15min
primerev :: [Integer]
primerev = take 10000 (filter prev primes)

prev :: Integer -> Bool
prev p = prime (reversal p)

-- Numer of consecutive primes 30min
primecons :: Integer
primecons = primesum 0

plist :: Int -> [Integer]
plist n = take 101 (drop n primes)

primesum :: Int -> Integer
primesum n = let p = sum (plist n) in
            if (prime p) then p else primesum (n+1)

-- Refute a conjecture 5min
primeref :: Integer
primeref = primemul 1

primemul :: Int -> Integer
primemul n = let p = (product (take n primes)) + 1 in
            if (not (prime p)) then p else primemul (n+1)

-- Implement and test Luhn algorithm 2 hours
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

isAmericanExpress :: Integer -> Bool
isAmericanExpress num = let numlist = integerToDigits num in
  ((take 2 numlist == [3, 4]) || (take 2 numlist == [3, 7])) &&
  (length numlist == 15) &&
  (luhn num)

isVisa :: Integer -> Bool
isVisa num = let numlist = integerToDigits num in
  (take 1 numlist == [4]) &&
  (length numlist == 16) &&
  (luhn num)

isMaster :: Integer -> Bool
isMaster num = let numlist = integerToDigits num in
  (length numlist == 16) &&
  ((inRange (div num 100000000000000) 51 55) || (inRange (div num 1000000000000) 2221 2720)) &&
  (luhn num)

inRange :: Integer -> Integer -> Integer -> Bool
inRange x min max = (x >= min) && (x <= max)

-- Crime Scene Investigation S:21:05
data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Encodes whether a boy accuses another boy
accuses :: Boy -> Boy -> Bool
accuses Matthew b
  | b == Carl = False
  | b == Matthew = False
  | otherwise _
accuses Peter b
  | b == Matthew = True
  | b == Jack = True
  | otherwise = False