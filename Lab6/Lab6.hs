module Lab6 where

import Data.List
import System.Random
import Lecture6 hiding (exM)
import Control.Monad

-- Timing computation
import Text.Printf
import Control.Exception
import System.CPUTime

{-
    Assignment 1 (30min)
-}
exM :: Integer -> Integer -> Integer -> Integer
exM x p n
  | n == 1 = 0
  | p == 0 && n /= 0 = 1
  | even p = exM x (p `quot` 2) n ^ 2 `mod` n
  | otherwise = exM x ((p-1) `quot` 2) n ^ 2 * (x `mod` n)

{-
    Assignment 2 (30min)

    The new modular exponential function is more efficient as proved with
    this test. When running it with arbitrary (large) values the execution
    time of exM is always shorter than that of expM. The following tests
    verify this:

    *Lab6> exMTest 1000 16 52 10000
    expM) Computation time: 0.01187 sec
    exM) Computation time: 0.00923 sec <- faster

    *Lab6> exMTest 1000 16 52 10000
    expM) Computation time: 0.01049 sec
    exM) Computation time: 0.00821 sec <- faster

    *Lab6> exMTest 1000 16 52 10000
    expM) Computation time: 0.01055 sec
    exM) Computation time: 0.00823 sec <- faster
-}

time :: Integer -> Integer -> Integer -> Integer -> IO (Double, Double)
time 0 _ _ _ = do return(0, 0)
time n bmax emax mmax = do
    let b = getStdRandom (randomR (0,bmax))
    let e = getStdRandom (randomR (0,emax))
    let m = getStdRandom (randomR (0,mmax))

    start <- getCPUTime
    v <- liftM3 expM b e m
    end   <- getCPUTime
    let diff1 = (fromIntegral (end - start)) / (10^12)
    start <- getCPUTime
    v <- liftM3 exM b e m
    end   <- getCPUTime
    let diff2 = (fromIntegral (end - start)) / (10^12)
    
    (ndiff1, ndiff2) <- time (n-1) bmax emax mmax
    let ret1 = diff1 + ndiff1
    let ret2 = diff2 + ndiff2
    return (ret1, ret2)

exMTest :: Integer -> Integer -> Integer -> Integer -> IO ()
exMTest n bmax emax mmax = do
    (expTime, exTime) <- time n bmax emax mmax
    printf "expM) Computation time: %0.5f sec\n" (expTime :: Double)
    printf "exM) Computation time: %0.5f sec\n" (exTime :: Double)
    return ()

{-
    Assignment 3 (15m)

    The list of composite numbers is equal to the list of not prime
    numbers. Therefore the list can be created as follows:

    composites :: [Integer]
    composites = [x | x <- [4..], (not . prime) x]
-}

{-
    Assignment 4 (30min)

    The least number that fools the Fermats test greatly depends on
    the amount of tests run (k). When using k=1 and k=2 the number 9
    frequently fools the test. However, when using k=3 the least number
    found to fool the test is 185, although it could be lower when running
    the test multiple times.
    
    Unfortunately when increasing k to 3 the test becomes incredibly slow, 
    therefore this is done 3 times in a row at most. This is also the reason
    why it is not advisable to run the test for k=4, as this will take way 
    too long to execute. However we can expect that for k increasing the number
    that fools the Fermat's test will increase as well.

    -- Fermat test for k=1
    *Lab6> absurdFermatLeast 100 1
    9
    *Lab6> absurdFermatLeast 100 1
    9
    *Lab6> absurdFermatLeast 100 1
    9

    -- Fermat test for k=2
    *Lab6> absurdFermatLeast 100 2
    9
    *Lab6> absurdFermatLeast 100 2
    9
    *Lab6> absurdFermatLeast 100 2
    9

    -- Fermat test for k=3
    *Lab6> absurdFermatLeast 3 3
    703
    *Lab6> absurdFermatLeast 3 3
    561
    *Lab6> absurdFermatLeast 3 3
    185
-}

-- Returns the first composite value that fools the Fermat test
absurdFermat :: Int -> Int -> [Integer] -> IO Integer
absurdFermat k i list = do
    b <- primeTestsF k n
    if b then
        return n
    else
        do absurdFermat k (i+1) list
    where n = list !! i

-- Returns the least composite value that fools the Fermat test after n times
absurdFermatLeast :: Int -> Int -> IO Integer
absurdFermatLeast n k = do
    first <- absurdFermat k 0 composites
    doAbsurdFermatLeast (n-1) k first composites

doAbsurdFermatLeast :: Int -> Int -> Integer -> [Integer] -> IO Integer
doAbsurdFermatLeast 0 k prev list = do
    new <- absurdFermat k 0 list
    return (min prev new)

doAbsurdFermatLeast n k prev list = do
    new <- absurdFermat k 0 list
    doAbsurdFermatLeast (n-1) k (min new prev) list

{-
    Assignment 5

    All Carmichaels numbers will pass the test, as can be experienced when executing
    the testFermantLiarCarm function. As an example when executing the function the
    result is as follows:

    *Lab6> testFermantLiarCarm 5 1
    [294409,56052361,118901521,172947529,216821881]

    *Lab6> take 5 carmichael 
    [294409,56052361,118901521,172947529,216821881]

    Carmichael numbers are numbers which fulfill the core formula from Fermats
    little theorem, although they are composite. Therefore all his numbers will
    pass the Fermats test.

-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1)]

testFermantLiarCarm :: Int -> Int -> IO [Integer]
testFermantLiarCarm n k = filterM (primeTestsF k) (take n carmichael)

{-
    Assignment 6

    It is currently not possible to test the carmichael numbers
    against the Miller-Rabin test, because it is too computationally
    expensive. Where as the Fermat's could be done on some carmichael
    numbers this is too difficult to do for the Miller-Rabin test.
-}

testMRCarm :: Int -> Int -> IO [Integer]
testMRCarm n k = filterM (primeMR k) (take n carmichael)

-- Assignment 7
{-
    Finds the first 9 mersene prime numbers.

    This last function is also quite intensive computational-wise.
    Therefore only the first 9 Mersenne primes are returned.

    The following numbers are returned as Mersenne primes
    *Lab6> mersennePrimes 
    [3,7,31,127,8191,131071,524287]

    These numbers are checked against the internet and check out to
    be actual Mersenne primes.
-}
mersennePrimes :: IO [Integer]
mersennePrimes = do
    bases <- filterM (\p -> primeMR 1 (2^p - 1)) (take 9 primes)
    return (map (\p -> 2^p-1) bases)

main :: IO ()
main = do
    putStrLn "Lab 6 -- Team 6"
    putStrLn "\nAssignment 2"
    putStrLn "Comparing expM and exM:"
    exMTest 1000 16 52 10000
    putStrLn "\nAssignment 4"
    putStrLn "Testing Fermat absurdity"
    putStrLn "Least after 100 times for k=1:"
    e <- absurdFermatLeast 100 1
    print e
    putStrLn "Least after 100 times for k=2:"
    d <- absurdFermatLeast 100 2
    print d
    putStrLn "Least after 100 times for k=3:"
    c <- absurdFermatLeast 10 3
    print c
    putStrLn "\nAssignment 5"
    putStrLn "Testing Carmichael numbers against Fermat"
    putStrLn "\nCarmichael numbers which pass the test:"
    b <- testFermantLiarCarm 5 1
    print b
    putStrLn "First 5 Carmichael numbers:"
    print (take 5 carmichael)
    putStrLn "\nAssignment 7"
    putStrLn "First 9 Mersenne numbers:"
    a <- mersennePrimes
    print a