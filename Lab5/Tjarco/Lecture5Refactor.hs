
module Lecture5Refactor

where

import Data.List
import System.Random

type Row    = Int
type Column = Int
type Value  = Int
type Grid   = [[Value]]

-- Assignment 2 refactor
type Position = (Row,Column)
type Constrnt = [[Position]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+-------+-------+-------+")
    showRow as; showRow bs; showRow cs
    putStrLn ("+-------+-------+-------+")
    showRow ds; showRow es; showRow fs
    putStrLn ("+-------+-------+-------+")
    showRow gs; showRow hs; showRow is
    putStrLn ("+-------+-------+-------+")

type Sudoku = Position -> Value

sud2grid :: Sudoku -> Grid
sud2grid s =
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ]

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c)
  where
  pos :: [[a]] -> Position -> a
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks

subGrid :: Sudoku -> Position -> [Value]
subGrid s (r,c) =
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r =
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c =
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

-- Assignment 2 refactor
rowConstrnt, columnConstrnt, blockConstrnt :: Constrnt
rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks]
blockConstrntNrc = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocksNrc, b2 <- blocksNrc]

freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r, c) xs = let ys = filter (elem (r,c)) xs in
  foldl1 intersect (map ((values \\) . map s) ys)

freeAtPos :: Sudoku -> Position -> [Value]
freeAtPos s p =
  freeAtPos' s p rowConstrnt
  `intersect` freeAtPos' s p columnConstrnt
  `intersect` freeAtPos' s p blockConstrnt
  `intersect` freeAtPos' s p rowConstrnt

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> Position -> Bool
subgridInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subGrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjectiveNrc s (r,c) |
                    r <- [2,6], c <- [2,6]]

extend :: Sudoku -> (Position,Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (y,z) x = if x == y then z else f x

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         prune (r,c,v) constraints) | v <- vs ]

prune :: (Row,Column,Value)
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblockNrc (r,c) (x,y)=
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in
              if (not . consistent) s then []
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,
                            c <- positions,
                            s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraints :: Sudoku -> [Constraint]
constraints s = sortBy length3rd
    [(r,c, freeAtPos s (r,c)) |
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node])
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs)
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

---- NRC variation ----
blocksNrc :: [[Int]]
blocksNrc = [[2..4], [6..8]]

belongsNrc :: Int -> [Int]
belongsNrc x = concat $ filter (elem x) blocksNrc

subGridNrc :: Sudoku -> (Row, Column) -> [Value]
subGridNrc s (r, c) = [s (r', c') | r' <- belongsNrc r, c' <- belongsNrc c]

freeInSubgridNrc :: Sudoku -> (Row, Column) -> [Value]
freeInSubgridNrc s (r, c)= freeInSeq (subGridNrc s (r, c))

subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where
   vs = filter (/= 0) (subGridNrc s (r,c))

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = belongsNrc r == belongsNrc x &&
  belongsNrc c == belongsNrc y
---- End NRC Variation ----

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs
                  if null y
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs)
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs
                        then return []
                        else return
                          (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node])
            -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes =
  do xs <- ionodes
     if null xs
       then return []
       else
         if goal (head xs)
           then return [head xs]
           else do ys <- rsearch succ goal (succ (head xs))
                   if (not . null) ys
                      then return [head ys]
                      else if null (tail xs) then return []
                           else
                             rsearch
                               succ goal (return $ tail xs)
