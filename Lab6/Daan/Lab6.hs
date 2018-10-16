module Lab6 where

import Data.List
import System.Random
import Lecture6
import Control.Monad

-- Timing computation
import Text.Printf
import Control.Exception
import System.CPUTime

{-
    Assignment 1 (30min)

    exM :: Integer -> Integer -> Integer -> Integer
    exM x ex n = doexM x ex n 1

    doexM :: Integer -> Integer -> Integer -> Integer -> Integer
    doexM _ 0 _ c = c
    doexM b e m c = doexM b (e-1) m cn
        where cn = mod (b*c) m
-}

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
    composites = [x | x <- [1..], x > 1, (not . prime) x]
-}

{-
    Assignment 4 (30min)

    The least number that fools the Fermats test greatly depends on
    the amount of tests run (k). When using k=1 and k=2 the number 9
    frequently fools the test. However, when using k=3 the least number
    found to fool the test is 185, although it could be lower when running
    the test multiple times. Unfortunately when increasing k to 3 the test
    becomes incredibly slow, therefore this is done 3 times in a row at most.
    This is also the reason why it is not advisable to run the test for k=4, 
    as this will take way too long to execute.

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
    Assignment 4

    Doing the fermat absurdity test with carmichael numbers is quite
    boring as the first carmichael number always fools the test. 

    When testing the first N carmichael numbers against the fermat test
    the program always crashes at the second Carmichael number, probably
    because it is too big to test. But according to the definition of the
    carmichael numbers they will always pass the Fermat's test.

    Carmichael numbers are numbers which fulfill the core formula from Fermats
    little theorem, although they are composite. Therefore all his numbers will
    pass the Fermats test.

-}

absurdFermatLeastCM :: Int -> Int -> IO Integer
absurdFermatLeastCM n k = do
    first <- absurdFermat k 0 carmichael
    doAbsurdFermatLeast (n-1) k first carmichael

absurdFermatCM :: Int -> Int -> IO ()
absurdFermatCM n k = doAbsurdFermatCM n 0 k

doAbsurdFermatCM :: Int -> Int -> Int -> IO ()
doAbsurdFermatCM n i k
    | n == i = return ()
    | otherwise = do
        putStrLn ("Carmichael num: " ++ show(numCM))
        b <- primeTestsF k numCM
        putStrLn ("Fermat says prime: " ++ show(b))
        putStrLn ("")
        doAbsurdFermatCM n (i+1) k
        where numCM = carmichael !! i

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1)]

{-
    Assignment 6
-}
absurdMRCM :: Int -> Int -> IO ()
absurdMRCM n k = doAbsurdMRCM n 0 k

doAbsurdMRCM :: Int -> Int -> Int -> IO ()
doAbsurdMRCM n i k
    | n == i = return ()
    | otherwise = do
        putStrLn ("Carmichael num: " ++ show(numCM))
        b <- primeMR k numCM
        putStrLn ("MR says prime: " ++ show(b))
        putStrLn ("")
        doAbsurdMRCM n (i+1) k
        where numCM = carmichael !! i