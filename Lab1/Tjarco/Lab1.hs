
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

factorial :: Int -> Int
factorial 0  = 1
factorial n = n * factorial (n-1)

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
powerSetLength :: Int -> Int
powerSetLength n = length (subsequences [1..n])

{-
  Unfortunatly, QuickCheck will feed integers to powerSetLenght function that
  are too large and will make the subsequences function hang. Limiting the value
  of n to a maximum avoids this issue. Therfore, the property is indeed hard to
  test

  We are checking a mathematical fact, namely if |A| = n, then |P(A)| = 2^n
-}
powerSetLengthTest = quickCheckResult(\n -> n >= 0 && n < 25 -->
                                      powerSetLength n == 2 ^ n)

-- Assignment 3: 10 minutes --
permutationsLength :: Int -> Int
permutationsLength n = length (permutations [0..n-1])

{-
  Same as with the previous assignment, QuickCheck will feed integers that are
  too large to complete the test in reasonable time, therefore the input is
  restricted to < 10. The property is indeed hard to test

  We are testing a mathematical fact, namely that the length of a permutation of
  a list A is n!, where n = |A|
-}
permutationsLengthTest = quickCheckResult(\n -> n >= 0 && n < 9-->
                                          permutationsLength n == factorial n)

-- Assignment 4: 15 minutes --
reverseIsPrime :: Integer -> Bool
reverseIsPrime n = prime (reversal n)

reversablePrimes :: [Integer]
reversablePrimes = [n | n <- primes, reverseIsPrime n]

findReversablePrimesBelow1000 :: [Integer]
findReversablePrimesBelow1000 = takeWhile (<1000) reversablePrimes

{-
  One could test this function by finding a list of reversable primes
  and hold it against a list generated by this function.
-}

firstFiveReversablePrimes = [2,3,5,7,11]

reversablePrimesTest :: Bool
reversablePrimesTest = firstFiveReversablePrimes == take 5 reversablePrimes

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

{-
  One can test the correctness of the function by taking the sum of the
  result and check if it is a prime. Proving that it is indeed the smallest
  prime with this property, is trivial.

  Assume that the first range of 101 elements in the list of ordered primes
  which sum  is a prime is [2 .. n1 .. n100 .. ]. Hence x = Sum n1 .. n100 is
  a prime

  now assume that x is not the smallest prime number with this property. This
  means that there is a range [2 .. m1 .. m100 ..] such that m1 > n1 where
  Sum m1 .. m100 is smaller than x. However, since m1 is larger than n1, the sum
  of consecutive primes can never be smaller, hence a contradiction, which proves
  that x is the smallest prime
-}

-- Assignment 6: 40 minutes --
productIsNoPrime :: [Integer] -> Bool
productIsNoPrime x = not (prime (product x + 1))

findNotProductPrimes :: [Int] -> [Integer] -> [[Integer]]
findNotProductPrimes (n:ns) x
  | productIsNoPrime (take n x) = take n x : findNotProductPrimes ns x
  | otherwise = findNotProductPrimes ns x

findSmallestNotProductPrime :: [Integer]
findSmallestNotProductPrime = concat (take 1 (findNotProductPrimes [1..] primes))
{-
  Hence, the smallest counter example is [2,3,5,7,11,13]
-}

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

-- Assignment 7 Tests: 15 minutes --

{-
  The test is implemented by getting valid generated numbers for the different
  cards, allongside with some altered numbers such that they become invalid.

  The test functions check wheter the return values from the card validators
  match the expected result from the valid and invalid numbers in the values
  lists
-}
americanExpressValues = [378282246310005, 371449635398431,
  378734493671000, 378734493671001, 340000000000009, 300000000000009]
americanExpressValuesCheck = [True, True, True, False, True, False]

americanExpressTest, masterTest, visaTest :: [Integer] -> [Bool] -> Bool
americanExpressTest [] [] = True
americanExpressTest (v:vs) (c:cs) = isAmericanExpress v == c
  && americanExpressTest vs cs

masterValues = [5431111399308413, 5031111399308413, 5476749615967326,
  5197346823291066, 51973468232910664, 5595351405983310]
masterValuesCheck = [True, False, True, True, False, True]

masterTest [] [] = True
masterTest (v:vs) (c:cs) = isMaster v == c && masterTest vs cs

visaValues = [4109729743927200, 4399407860631330, 44699401260631330,
  4671369802283134, 471369802283134, 4022598666185698]
visaValuesCheck = [True, True, False, True, False, True]

visaTest [] [] = True
visaTest (v:vs) (c:cs) = isVisa v == c && visaTest vs cs

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

{-
  Hence, the guilty one is Jack and the honest ones are Matthew, Peter and Carl
-}

-- Bonus: Special Pythagorean triplet: 150 minutes --
isN :: Float -> Bool
isN n = floor n == ceiling n

c :: Float -> Float -> Float
c a b = sqrt (a**2 + b**2)
cIsN a b = isN (c a b)

findCandidateList [] = []
findCandidateList (x:xs) = (x : filter (cIsN x) xs) : findCandidateList xs

filteredList n = filter ((>1) . length) (findCandidateList n)

pythagoreanFilter n [] = []
pythagoreanFilter n (a:b:xs)
  | a + b + c a b == n = [a, b, c a b]
  |otherwise = pythagoreanFilter n (a:xs)
pythagoreanFilter n (a:b) = []

pythagorean n = head (filter (not . null) (map (pythagoreanFilter n) (filteredList [1..n])))

isPythagoreanTriangle [] = False
isPythagoreanTriangle (a:b:c) = a**2 + b**2 == head c ** 2

pythagoreanTest = sum (pythagorean 1000) == 1000 && isPythagoreanTriangle (pythagorean 1000)

{-
  The actual sollution to the assignment, since the assignment states that the
  result should be the product of the triplet abc.

  The answer is: a*b*c =  200 * 375 * 425 = 31875000
-}
pythagoreanProduct = product . pythagorean

-- Bonus: Sum of primes below 2 milion: 10 minutes --

primesBelow n  = takeWhile (< n) primes
sumOfPrimes = sum . primesBelow
sumOfPrimbesBelow2mil = sumOfPrimes 2000000
{-
  The answer is 142913828922
-}

main = do
  putStrLn "-- Lab 1 Team 6 --"
  putStrLn "\nAssignment 1"
  putStr "Sum squares test: "
  sumSquaresTest
  putStr "Sum cubes test: "
  sumCubesTest
  putStrLn "\nAssignment 2"
  putStr "Power set cardinality test: "
  powerSetLengthTest
  putStrLn "\nAssignment 3"
  putStr "Permutations length test"
  permutationsLengthTest
  putStrLn "\nAssignment 4"
  putStrLn "Reversable primes < 1000 :"
  print findReversablePrimesBelow1000
  putStrLn "\nAssignment 5"
  putStrLn "Smallest prime which is the sum of 101 primes: "
  print (sum findPrimeSum101)
  putStrLn "\nAssignment 6"
  putStrLn "Smallest counterexample: "
  print findSmallestNotProductPrime
  putStrLn "\nAssignment 7"
  putStr "Mastercard  test: "
  print (masterTest masterValues masterValuesCheck)
  putStr "American Express  test: "
  print (americanExpressTest americanExpressValues americanExpressValuesCheck)
  putStr "Visa test: "
  print (visaTest visaValues visaValuesCheck)
  putStrLn "\nAssignment 8"
  putStr "Guilty boy: "
  print guilty
  putStr "Honest boys: "
  print honest
