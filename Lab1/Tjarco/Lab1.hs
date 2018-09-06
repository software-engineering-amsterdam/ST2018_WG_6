
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

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Assignment 1: 20 minutes --
sumSquares :: Integer -> Integer
sumSquares n = sum (map (^2) [1 .. n])

sumSquares2 :: Integer -> Integer
sumSquares2 n = quot (n*(n+1)*(2*n+1)) 6

-- Test question 2 from the workshop --
sumSquaresTest = quickCheckResult(\n -> n > 0 --> sumSquares n == sumSquares2 n)

sumCubes :: Integer -> Integer
sumCubes n = sum (map (^3) [1 .. n])

sumCubes2 :: Integer -> Integer
sumCubes2 n = quot (n*(n+1)) 2 ^ 2

-- Test question 3 from the workshop
sumCubesTest = quickCheckResult(\n -> n > 0 --> sumCubes n == sumCubes2 n)

-- Assignment 2: 10 minutes --
setLength :: Int -> Int
setLength n = length [0 .. n]

powerSetLength :: Int -> Int
powerSetLength n = length (subsequences [0..n])

{-
  Unfortunatly, QuickCheck will feed integers to powerSetLenght function that
  are too large and will make the subsequences function hang. Limiting the value
  of n to a maximum avoids this issue
-}
powerSetLengthTest = quickCheckResult(\n -> n >= 0 && n < 25 -->
                                      powerSetLength n == 2 ^ setLength n)

-- Assignment 3: 10 minutes --
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

permutationsLength :: Int -> Int
permutationsLength n = length (permutations [0..n-1])

{-
  Same as with the previous assignment, QuickCheck will feed integers that are
  too large to complete the test in reasonable time, therefore the input is
  restricted to < 10
-}
permutationsLengthTest = quickCheckResult(\n -> n >= 0 && n < 10-->
                                          permutationsLength n == factorial n)

-- Assignment 4: 15 minutes --
reverseIsPrime :: Integer -> Bool
reverseIsPrime n = prime (reversal n)

reversablePrimes :: [Integer]
reversablePrimes = [n | n <- primes, reverseIsPrime n]

findFirst10000ReversablePrimes :: [Integer]
findFirst10000ReversablePrimes = take 10000 reversablePrimes

-- Asignment 5: 45 minutes --
sumIsPrime :: [Integer] -> Bool
sumIsPrime = prime . sum

findPrimeSum :: Int -> [Integer] -> [Integer]
findPrimeSum n (x:xs)
  | sumIsPrime (take n (x:xs)) = take n (x:xs)
  | sumIsPrime (take n xs) = take n xs
  | otherwise = findPrimeSum n xs

findPrimeSum101 :: [Integer]
findPrimeSum101 = findPrimeSum 101 primes

-- Assignment 6: 40 minutes --
productIsNoPrime :: [Integer] -> Bool
productIsNoPrime x = not (prime (product x + 1))

findNotProductPrimes :: [Int] -> [Integer] -> [[Integer]]
findNotProductPrimes (n:ns) x
  | productIsNoPrime (take n x) = take n x : findNotProductPrimes ns x
  | otherwise = findNotProductPrimes ns x

findSmallestNotProductPrime :: [Integer]
findSmallestNotProductPrime = concat (take 1 (findNotProductPrimes [1..] primes))

-- Assignment 7: 90 minutes --
luhn :: Integer -> Bool
luhn x =  mod (sum (calculateLuhnNumber (digits x))) 10 == 0

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:xs)
  | y > 4 =  x : (y*2)-9 : doubleEveryOther xs
  | otherwise = x : y*2 : doubleEveryOther xs
doubleEveryOther (x:xs) = x : doubleEveryOther xs

digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

calculateLuhnNumber :: [Integer] -> [Integer]
calculateLuhnNumber x = reverse (doubleEveryOther (reverse x))

checkFirstDigits:: [Integer] -> [Integer] -> Bool
checkFirstDigits s l = take (length s) l == s

checkAllFirstDigits:: [[Integer]] -> [Integer] -> Bool
checkAllFirstDigits [] l = False
checkAllFirstDigits (x:xs) l = checkFirstDigits x l || checkAllFirstDigits xs l

inRange:: Integer -> Integer -> Integer -> Bool
inRange x s e = x >= s && x<= e

isAmericanExpress, isMaster, isVisa:: Integer -> Bool

isAmericanExpress x
  | length (digits x) /= 15 = False
  | not (checkAllFirstDigits [[3,7], [3,4]] (digits x)) = False
  | otherwise = luhn x

isMaster x
  | length (digits x) /= 16 = False
  | not (checkAllFirstDigits [[5,1],[5,2], [5,3], [5,4], [5,5]] (digits x))
    && not (inRange (div x 1000000000000) 2221 2720) = False
  | otherwise = luhn x

isVisa x
  | length (digits x) /= 13
    && length (digits x) /= 16
    && length (digits x) /= 19 = False
  | not (checkFirstDigits [4] (digits x)) = False
  | otherwise = luhn x

-- Assignment 8: 120 minutes --
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
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
guilty = [boy | boy <- boys, length (accusers boy) >= 3]
honest = accusers (head guilty)
