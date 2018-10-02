module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Assignment 2 --
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

randomFlip :: Int -> IO Int
randomFlip x = do
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

genRandomSet :: IO (Set Int)
genRandomSet = do
  l <- genIntList
  return (list2set l)

{-
    A quickcheck implementation for generating arbitrary Sets.
-}
setGen :: (Arbitrary a, Ord a) => Int -> Gen (Set a)
setGen n = fmap list2set arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized setGen

genRandomSet' :: IO (Set Int)
genRandomSet' = generate arbitrary :: IO (Set Int)

-- Assignment 3 --
intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set (xs `intersect` ys)

union' :: Ord a => Set a -> Set a -> Set a
union' (Set xs) (Set ys) = list2set (xs `union` ys)

difference :: Ord a => Set a -> Set a -> Set a
difference (Set set1) set2 = Set [x | x <- set1, not (x `inSet` set2)]

testSetR :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
testSetR c n f = if c == n then print (show n ++ " test have passed")
  else do
    set1 <- genRandomSet
    set2 <- genRandomSet
    if f set1 set2 then
      testSetR (c+1) n f
    else
      error ("Failed on: " ++ show set1 ++ " and " ++ show set2)

testSet :: (Set Int -> Set Int -> Bool) -> IO ()
testSet = testSetR 0 100

prop_Union :: Set Int -> Set Int -> Bool
prop_Union s1@(Set xs) s2@(Set ys) =
  all (`inSet` un) xs && all (`inSet` un) ys
  where un = s1 `union'` s2

set2list :: Set a -> [a]
set2list (Set xs) = xs

prop_Intersection :: Set Int -> Set Int -> Bool
prop_Intersection s1 s2 =
  all (\x -> x `inSet` s1 && x `inSet` s2) (set2list (s1 `intersection` s2))

prop_Difference :: Set Int -> Set Int -> Bool
prop_Difference s1 s2 =
  all (\x -> not (x `inSet` dif)) (set2list s2)
  where dif = s1 `difference` s2

-- Execute the tests with both my own implementation and quickcheck
test_union = do
  putStrLn "Testing with testSet: "
  testSet prop_Union
  putStrLn "\nTesting with quickcheck: "
  quickCheck prop_Union

test_intersection = do
  putStrLn "Testing with testSet: "
  testSet prop_Intersection
  putStrLn "\nTesting with quickcheck: "
  quickCheck prop_Intersection

test_difference = do
  putStrLn "Testing with testSet: "
  testSet prop_Difference
  putStrLn "\nTesting with quickcheck: "
  quickCheck prop_Difference

-- Assignment 5
type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = xs `union` [(y, x) | (x, y) <- xs]

-- Assignment 6 --
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClosList :: Ord a => Rel a -> Rel a -> Rel a
trClosList r s
  | sort s == sort (s `union` (s @@ r)) = s
  | otherwise = trClosList r (s `union` (s @@ r))

trClos :: Ord a => Rel a -> Rel a
trClos r = trClosList r r

-- Assignment 7
{-
  For every R+ of R, it should hold dat applying the trClos function again
  should result in the same R+. That is trClos (R+) == R+
-}
prop_trClosBalanced :: Ord a => Rel a -> Bool
prop_trClosBalanced r = trClos r == trClos (trClos r)

{-
  For every R+ of R, it should hold that R is a subset of R+
-}
prop_trClosSublist :: Ord a => Rel a -> Bool
prop_trClosSublist r = all (`elem` clos) r where clos = trClos r

{-
  Addaption from the implementation in Haskell Road to work with our Rel type
  which isn't a set. Used to test the transitive closure function
-}
isTransClos :: Ord a => Rel a -> Bool
isTransClos [] = True
isTransClos s = and [ trans pair s | pair <- s ] where
  trans (x,y) r =
    and [(x,v) `elem` r | (u,v) <- r, u == y ]

prop_TransClos :: Rel Int -> Bool
prop_TransClos = isTransClos . trClos

{-
  For every symetric closure S on R, it should hold that for every pair (x,y) in
  S, S should also contain (y,x)
-}
prop_symClos :: Ord a => Rel a -> Bool
prop_symClos r = all (\(x,y) -> (y,x) `elem` sc) sc
  where sc = symClos r

test_symAndTransClosures :: IO ()
test_symAndTransClosures = do
  putStrLn "Testing symmectric closure function"
  quickCheck (prop_symClos :: (Rel Int -> Bool))
  putStrLn "Testing transitive closure function on repeated application"
  quickCheck (prop_trClosBalanced :: (Rel Int -> Bool))
  putStrLn "Testing transitive closure function on containing R"
  quickCheck (prop_trClosSublist :: (Rel Int -> Bool))
  putStrLn "Testing transitive closure function"
  quickCheck (prop_TransClos :: (Rel Int -> Bool))

-- Assignment 8
{-
  Test whether transitive closure of the symmetric closure is equal to the
  symmetric closure of the transitive closure
-}
prop_symClosTrClos :: Ord a => Rel a -> Bool
prop_symClosTrClos r = symClos (trClos r) == trClos (symClos r)

test_symClosTrClosEquality :: IO ()
test_symClosTrClosEquality = do
  putStrLn ("Testing whether transitive closure of symmetric closure is equal to"
        ++ " the symmetric closure of the transitive closure")
  quickCheck (prop_symClos :: (Rel Int -> Bool))

{-
  Test report: they are equal for all test cases. However, we can also show that
  they would be equal:

  Both functions for transitive closure and symmetric closure only use the union
  operator as a final set operation. This means that applying the function on a
  set will never remove elements from that set. Therefore, the operations are
  associative. This means that the order of calling this functions does not matter,
  the result will be the same.
-}

main :: IO ()
main = do
  putStrLn "Lab 4 -- Team 6"
  putStrLn "\nAssignment 3"
  putStrLn "\nTesting Union"
  test_union
  putStrLn "\nTesting Intersection"
  test_intersection
  putStrLn "\nTesting Difference"
  test_difference
  putStrLn "\nAssignment 7"
  putStrLn "Testing the functions for Symmetric and Transitive closures"
  test_symAndTransClosures
  putStrLn "\nAssignment 8"
  test_symClosTrClosEquality
