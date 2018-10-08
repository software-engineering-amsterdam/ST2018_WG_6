module Lab5 where

import Data.List
import Lecture5
import Lecture5Refactor (solveAndShow, Position)
import System.CPUTime
import Text.Printf
import Control.Applicative

-- Assignment 1 --
{-
  The acctual addaption to the NRC sudoko problem is done in the Lecture5.hs
  file. Below is a test whether the addaption is correct
-}

testNrc :: Bool
testNrc = (nodeListToGrid . initNode) exampleNRCSolved ==
  (nodeListToGrid . solveNs . initNode) exampleNrc
  where
    nodeListToGrid = sud2grid . fst . head

-- Assignment 2 --
{-
  The actual refactor is done in the Lecture5.hs file

  After the refactor, it was easier to extend for the NRC problem. There was just
  another Constrnt needed, for the NRC blocks.

  For efficiency, there is not that much of a difference. Below code times
  the execution of both the original and the refactored version. If we run them
  100000 times, the results are as follows:

  Computation time old: 10.12346 sec
  Computation time refactor: 10.09564 sec

  Computation time old: 10.33268 sec
  Computation time refactor: 10.40448 sec

  Computation time old: 10.12653 sec
  Computation time refactor: 10.13401 sec
-}

time :: Int -> IO ()
time n = do
    start <- getCPUTime
    runSolver (Lecture5.solveAndShow exampleNrc) n
    end   <- getCPUTime
    startr <- getCPUTime
    runSolver (Lecture5Refactor.solveAndShow exampleNrc) n
    endr <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    let diffr = fromIntegral (endr - startr) / (10^12)
    printf "Computation time old: %0.5f sec\n" (diff :: Double)
    printf "Computation time refactor: %0.5f sec\n" (diffr :: Double)

runSolver ::IO t -> Int -> IO ()
runSolver _ 0 = return ()
runSolver s n = do
  a <- s
  runSolver s (n-1)
  return ()

-- Assignment 3
isMinimal :: Node -> Bool
isMinimal s = uniqueSol s && all (not . reducedUnique) pos
  where
    pos = filledPositions (fst s)
    reducedUnique (r,c) =  uniqueSol (eraseN s (r,c))

testMinimal :: IO ()
testMinimal = do
  node <- genRandomSudoku
  problem <- genProblem node
  print (isMinimal problem)

{-
  Running the test several times gives true every time, meaning that the
  genProblem function correcly generates minimal problems
-}

-- Assignment 4

{-
  Returns all positions belonging to the block of the argument position
-}
posForBlock :: Position -> [Position]
posForBlock (r,c) = liftA2 (,) (bl r) (bl c)

{-
  Erase all the positions given from the Node. Used to remove blocks.
-}
eraseAtPositions :: Node -> [Position] -> Node
eraseAtPositions = foldl eraseN

{-
  Return all the blocks, given by all posibile positions from the Rows and
  Colums
-}
allBlocks :: [Row] -> [Column] -> IO [[Position]]
allBlocks r c = randomize (map posForBlock (liftA2 (,) r c))

{-
  First, get n random blocks. Then erase those n random blocks from the Node
-}
eraseRandomBlocks :: Int -> Node -> IO Node
eraseRandomBlocks n node = do
    randomBlocks <- allBlocks [1,4,7] [1,4,7]
    return (foldl eraseAtPositions node (take n randomBlocks))

{-
  Generates a problem with a unique solution where n blocks are removed.
-}
genEmptyBlocksProblem :: Int -> IO Node
genEmptyBlocksProblem n = do
  s <- genRandomSudoku
  problem <- eraseRandomBlocks n s
  if uniqueSol problem then return problem
  else genEmptyBlocksProblem n

gen3EmptyBlocksProblem, gen4EmptyBlocksProblem ,gen5EmptyBlocksProblem :: IO Node
gen3EmptyBlocksProblem = genEmptyBlocksProblem 3
gen4EmptyBlocksProblem = genEmptyBlocksProblem 4
gen5EmptyBlocksProblem = genEmptyBlocksProblem 5

{-
  In conclusion, it is possible to generate this kind of problems where 3, 4 or 5
  blocks are removed. For more than 6, the function does not finish. Therefore
  we conclude that it is not possible to generate empty blocks problems with more
  than 5 empty blocks.
-}

-- Assignment 5:  45 minutes
freeAtPosNrc :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNrc s (r,c) = freeAtPos s (r,c) `intersect` freeInSubgridNrc s (r,c)

constraintsNrc :: Sudoku -> [Constraint]
constraintsNrc s = sortBy length3rd
    [(r,c, freeAtPosNrc s (r,c)) |
                       (r,c) <- openPositions s ]

emptyNodeNrc :: Node
emptyNodeNrc = (const 0,constraintsNrc (const 0))

genRandomNRCSudoku :: IO Node
genRandomNRCSudoku = do
  [r] <- rsolveNs [emptyNodeNrc]
  return r

randomNrcS :: IO ()
randomNrcS = genRandomNRCSudoku >>= showNode

genRandomNrcProblem :: IO Node
genRandomNrcProblem = do
  s <- genRandomNRCSudoku
  genProblem s

randomNrcProblem =  genRandomNrcProblem >>= showNode

main :: IO ()
main = genEmptyBlocksProblem 6 >>= showNode
