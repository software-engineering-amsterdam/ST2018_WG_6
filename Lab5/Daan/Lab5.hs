module Lab5 where

import Data.List
import System.Random
import Control.Monad
import Lecture5
import Lecture5Refactored (solveAndShow)

-- Timing computation
import Text.Printf
import Control.Exception
import System.CPUTime

{-
    Assignment 1
    
    I added the following functions to the Lecture file:

    -------------------------------------------------------

    freeInSubgrid' :: Sudoku -> (Row,Column) -> [Value]
    freeInSubgrid' s (r,c) = freeInSeq (subGrid' s (r,c))

    nrcBlocks :: [[Int]]
    nrcBlocks = [[2..4], [6..8]]

    bl' :: Int -> [Int]
    bl' x = concat $ filter (elem x) nrcBlocks

    subGrid' :: Sudoku -> (Row,Column) -> [Value]
    subGrid' s (r,c) =
        [s (r',c')| r' <- bl' r, c' <- bl' c]

    subgridInjective' :: Sudoku -> (Row, Column) -> Bool
    subgridInjective' s (r,c) = injective vs where
        vs = filter (/=0) (subGrid' s (r,c))

    sameblock' :: (Row,Column) -> (Row,Column) -> Bool
    sameblock' (r,c) (x,y) = bl' r == bl' x && bl' c == bl' y 

    ---------------------------------------------------------

    And I added the following lines to existing functions:

    ---------------------------------------------------------

    freeAtPos :: Sudoku -> (Row,Column) -> [Value]
        `intersect` (freeInSubgrid' s (r,c))

    consistent :: Sudoku -> Bool
     ++
        [ subgridInjective' s (r,c) |
            r <- [2,6], c <- [2,6]]
    
    prune :: (Row,Column,Value) -> [Contraint] -> [Constraint]
        | sameblock' (r,c) (x,y) = 
            (x,y,zs\\[v]) : prune (r,c,v) rest
    
    After adding this code we can solve the example NRC sudoku:

    *Lab5> solveAndShow exampleAssignment
    +-------+-------+-------+
    | 4 7 8 | 3 9 2 | 6 1 5 |
    | 6 1 9 | 7 5 8 | 3 2 4 |
    | 2 3 5 | 4 1 6 | 9 7 8 |
    +-------+-------+-------+
    | 7 2 6 | 8 3 5 | 1 4 9 |
    | 8 9 1 | 6 2 4 | 7 5 3 |
    | 3 5 4 | 9 7 1 | 2 8 6 |
    +-------+-------+-------+
    | 5 6 7 | 2 8 9 | 4 3 1 |
    | 9 8 3 | 1 4 7 | 5 6 2 |
    | 1 4 2 | 5 6 3 | 8 9 7 |
    +-------+-------+-------+
    [()]
-}

{-
    Assignment 2 (1h30)

    The refactord code is easier to extend on than the original code, although
    it differs not much. The result is that all the freeIn* functions can be removed
    and replaced with list definitions, including one for the NRC blocks.

    The refactored version is also slightly more efficient, as proved with the
    following function. It measures the execution time of the function, which is often
    but not always faster as the original code. The following test is used to check the
    execution times for solving the exampleNRC sudoku.

    Lab5.main
    [List of sudoku solutions]
    Normal) Computation time: 0.14399 sec
    Refactored) Computation time: 0.11675 sec
-}

time :: IO t -> IO t -> IO ()
time a b = do
    start <- getCPUTime
    v <- solveNtimes 1000 a
    end   <- getCPUTime
    let diff1 = (fromIntegral (end - start)) / (10^12)
    start <- getCPUTime
    v <- solveNtimes 1000 b
    end   <- getCPUTime
    let diff2 = (fromIntegral (end - start)) / (10^12)
    printf "Normal) Computation time: %0.5f sec\n" (diff1 :: Double)
    printf "Refactored) Computation time: %0.5f sec\n" (diff2 :: Double)
    return ()

solveNtimes :: Int -> IO t -> IO ()
solveNtimes 0 _ = return ()
solveNtimes n a = do
    v <- a
    solveNtimes (n-1) a
    return ()

ass2 = do
    time (Lecture5.solveAndShow exampleNRC) (Lecture5Refactored.solveAndShow exampleNRC)

{-
    Assignment 3 (2h)
    
    For this assignment a function has been created that
    checks whether a problem is a minimal problem, thus
    having just one solution, and multiples when removing
    one number from the grid.

    In order to check this two tests are performed. First it
    is tested whether the sudoku has just one solution. Then
    all filled squares are deleted in turn and it is determined
    whether this new problem also has just 1 solution. If this is
    the case the first sudoku is apparently not the minimal problem.

-}

testMinimalGrid :: Grid -> Bool
testMinimalGrid g = isMinimal ((initNode g) !! 0 )

testMinimalGen :: IO Bool
testMinimalGen = fmap (isMinimal) (genProblem =<< genRandomSudoku)

isMinimal :: Node -> Bool
isMinimal (s,con) = all (\(r,c) -> not (uniqueSol (eraseN (s,con) (r,c)))) (filledPositions s) && uniqueSol (s,con)

{-
    When testing this with the NRC example:

    *Lecture5> testMinimalGrid exampleNRC
    False

    Which checks out as there is a more minimal version of this problem

    -----------------------------------------------------------------

    When testing this with the generator:

    *Lecture5> testMinimalGen
    True

    Which also checks out as the problem builder only generates problems
    which are minimal.
-}

{-
    Assignment 4
    The following code is used to create sudoku problems where a specified
    amount of blocks is removed. This is done by randomly selecting three
    blocks and remove these from a generated sudoku. Then the remaining filled
    positions are randomized and minimalized.
-}

-- Return a list of lists with the positions of all blocks
allBlocks = [[(r,c) | r <- b1, c <- b2] | b1 <- blocks, b2 <- blocks]

-- Delete a predetermined amount of blocks from the list of blocks
deleteBlocks :: [[(Row,Column)]] -> Int -> IO [(Row,Column)]
deleteBlocks blcks bmin
  | length blcks > bmin = do
    remBlock <- fmap head (getRandomItem blcks)
    deleteBlocks (delete remBlock blcks) bmin
  | otherwise = do
    return (intercalate [] blcks)

-- Delete the selected blocks from the sudoku
deleteBlockFromSudoku :: Node -> [(Row,Column)] -> IO Node
deleteBlockFromSudoku n dblocks = do
    return (foldl eraseN n dblocks)

-- Generate a sudoku problem with empty blocks, usually done with 3 (hence the name)
gen3BProblem :: Int -> IO Node
gen3BProblem bmin = do 
  sudokuNode <- genRandomSudoku
  delBlocks <- deleteBlocks allBlocks bmin
  sudokuRes <- deleteBlockFromSudoku sudokuNode delBlocks
  ys <- (randomize . filledPositions) (fst sudokuRes)
  return (minimalize sudokuRes ys)

ass4 :: Int -> IO ()
ass4 x = (gen3BProblem x) >>= showNode

{-
    When using this generator the resulting sudoku problems are not always
    as sophisticated. In some cases the remaining blocks will be completely
    filled while in other cases the remaining blocks hold fewer values.

    *Lab5> ass4 3
    +-------+-------+-------+
    | 4 2 6 | 3 1 8 | 9 5 7 |
    | 3 9 8 | 7 6 5 | 4 1 2 |
    | 7 1 5 | 2 4 9 | 6 3 8 |
    +-------+-------+-------+
    | 1 6 3 | 4 5 2 | 8 7 9 |
    | 5 4 7 | 8 9 3 | 1 2 6 |
    | 2 8 9 | 1 7 6 | 3 4 5 |
    +-------+-------+-------+
    |       |       |       |
    |       |       |       |
    |       |       |       |
    +-------+-------+-------+
    *Lab5> ass4 3
    +-------+-------+-------+
    |   9   | 1     |   5   |
    |       |     2 |       |
    | 7     |   5   | 8     |
    +-------+-------+-------+
    |   3   | 7   5 |       |
    |       |   9   |       |
    | 4   6 |       |       |
    +-------+-------+-------+
    |       |       |       |
    |       | 2 1   |       |
    |       |   8 6 |       |
    +-------+-------+-------+

    It is also possible to generate sudoku with 4 and 5 empty blocks
    *Lab5> ass4 4
    +-------+-------+-------+
    |       |     4 |       |
    |       |       |       |
    |       | 3   2 |       |
    +-------+-------+-------+
    |   9 8 |       | 4     |
    | 5     |       |   8   |
    |       |       | 7     |
    +-------+-------+-------+
    | 8   4 |       |   1 2 |
    |   7   |       |       |
    |   1 2 |       |       |
    +-------+-------+-------+

    *Lab5> ass4 5
    +-------+-------+-------+
    |       |       |     4 |
    |       |       | 7 6 2 |
    |       |       |   9   |
    +-------+-------+-------+
    |     2 |       |       |
    | 3 5   |       |       |
    |   1   |       |       |
    +-------+-------+-------+
    | 1 9   |       | 8 7   |
    |     8 |       | 9   5 |
    |   3 6 |       |       |
    +-------+-------+-------+
-}

{-
    Assignment 5

    With the current implementation of the NRC problem in Lecture 5
    it already generates NRC problems currently. Therefore simply
    running the following statement will generate a NRC problem:

    genRandomSudoku >>= genProblem >>= showNode
-}