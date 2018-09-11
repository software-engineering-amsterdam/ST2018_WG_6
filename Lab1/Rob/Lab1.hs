
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

  -- Assignment 1 part 1
  sumSquares, sumSquares' :: Int -> Int
  sumSquares x = (x*(x+1)*(2*x+1)) `div` 6
  sumSquares' x = sum [y*y | y <- [1..x]]

  test1 = quickCheckResult (\n -> n > 0 --> sumSquares n == sumSquares' n)
  
  -- Assignment 1 part 2
  sumCubes, sumCubes' :: Int -> Int
  sumCubes x = (x*(x+1) `div` 2) ^ 2
  sumCubes' x = sum [y^3 | y <- [1..x]]

  test2 = quickCheckResult (\n -> n > 0 --> sumCubes n == sumCubes' n)


  -- Assignment 2
  f3 :: Int -> Int
  f3 x = length (subsequences [1..x])
   
  test3 = quickCheckResult (\n -> n > 0 && n < 20 --> f3 n == 2^n)

  perms :: [a] ->[[a]]
  perms [] = [[]]
  perms (x:xs) = concat (map (insrt x) (perms xs)) where
  
  insrt x [] = [[x]]

  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


  -- Assignment 3
  factorial :: Int -> Int
  factorial x = product [1..x]

  test4 = quickCheckResult(\n -> n > 0 && n < 5 --> length (permutations [1..n]) == factorial n)

  -- Assignment 4
  reversalPrime :: Integer -> Bool
  reversalPrime x = prime (reversal x)

  reversalPrimes = [ x | x <- primes, reversalPrime x]
  -- We need an actual list to test this.


  -- Assignment 5
  primeSum n = sum (take n primes)