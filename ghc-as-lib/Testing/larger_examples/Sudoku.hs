module Sudoku where

--import Test.QuickCheck

import Data.Maybe(isJust,isNothing,fromJust,listToMaybe,catMaybes)
import Data.Char(digitToInt)
import Data.List(nub,transpose)
------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (rep (rep Nothing))
   where rep = replicate 9

absu :: Sudoku
absu = Sudoku [[Nothing | _ <- [1..9], _ <- [1..9]]]

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sud) = isGoodShape sud && isGoodContents sud
  where isGoodShape s    = goodLength s && all goodLength s

        isGoodContents s = all goodCell (concat s)

        goodCell (Just d) = d > 0 && d < 10
        goodCell Nothing  = True

goodLength :: Foldable t => t a -> Bool
goodLength s      = length s == 9

isSud :: Sudoku -> Bool
isSud (Sudoku []) = False
isSud (Sudoku s) = length s == 9 && and [length r == 9 && all (\x -> x >= Just 1 && x <= Just 9 || x == Nothing) r | r <- s]


prop_is_sud s = isSud s && isSudoku s

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
--isFilled  = all (/= Nothing) . concat . rows
isFilled (Sudoku s) = all (fun) (concat s)
  where fun = (g Nothing)
        g   = (/=)

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen

printSudoku :: Sudoku -> IO()
printSudoku sud
    | (isOkay sud && isSudoku sud) =printAllRows (rows sud)
    | otherwise = putStrLn "Not valid sudoku"

printAllRows :: [Row]-> IO ()
printAllRows [] = return ()
printAllRows (row:rows) = do
 printRow row
 printAllRows rows

printRow :: Row -> IO ()
printRow [] =do
  putStrLn ['\n']
  return ()
printRow (cell:cells) = do 
  printCell cell
  printRow cells

printCell :: Cell -> IO ()
printCell Nothing = putStr " . "
printCell (Just x)=do
  putStr " "
  putStr (show x)
  putStr " "

{-
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . showSudoku


showSudoku :: Sudoku -> String
showSudoku = unlines . map showRow . rows
  where showRow = concatMap showCell
        showCell :: Cell -> String
        showCell Nothing  = "."
        showCell (Just d) =  show d
-}

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
{-
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
           s <- readFile file -- error if file does not exist
           return (readSud s)

readSud :: String -> Sudoku
readSud s = if isSudoku  try then try
    else error ("Not a valid sudoku:\n"
                 ++  showSudoku try )
       where try  = Sudoku $ map (map readCell) (lines s)-}

readCell :: Char -> Cell
readCell '.' = Nothing
readCell d   = Just $ digitToInt d

  -- Just $ digitToInt d
  -- error if not a digit; bad digits caught by isSudoku

------------------------------------------------------------------------------

-- * C1
{- 
-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [ (1, elements allCells)
                , (9, return Nothing)
                ]

allCells :: [Cell]
allCells = [Just d | d <- [1..9]]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
 -}
------------------------------------------------------------------------------

type Block = [Maybe Int]


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock b = let digits = catMaybes b in nub digits == digits


-- * D2

-- conceptually easiest solution (?): get all squares by their coordinates

blocks :: Sudoku -> [Block]
blocks (Sudoku rs) = rows' ++ columns ++ squares
     where rows'   = rs
           columns = transpose rows'
           squares = [getSquare coords rs | coords <- blockCoordinates]

           getSquare (x,y) rs =  firstSquare $ map (drop x) (drop y rs)
           firstSquare rs =  concatMap (take blockSize) $ take blockSize rs

blockSize = 3  -- does not need to be this general

blockCoordinates :: [Pos]
blockCoordinates = [(x,y) | x <- blockPositions,
                            y <- blockPositions ]

blockPositions = take blockSize [0, blockSize..] -- [0,3,6]

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud  = length bs == 3 * 9
                        && all goodLength bs
   where bs = blocks sud


-- * D3

isOkay :: Sudoku -> Bool
isOkay = all (noDups . filter isJust) . blocks
  where  noDups []           = True     -- noDups bs = nub bs == bs
         noDups (c:cs)       = c `notElem` cs && noDups cs


isOk :: Sudoku -> Bool
isOk s = all isOkayBlock (blocks s)

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks sud =
  [ pos | pos <- allCoords, isNothing (sud `atPosition` pos)]

allCoords = [ (y,x) | x <- [0..8], y <- [0..8] ]

sud `atPosition` (y,x) = (rows sud !!y )!!x


prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = blanks allBlankSudoku == allCoords

-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = front ++ y:back
  where (front, _:back) = splitAt i xs


{- prop_bangBangEquals_correct :: Int -> Char -> String -> Property
prop_bangBangEquals_correct i c s = i < length s && i >= 0  ==>
                                   (s !!= (i,c)) !! i == c -}

-- * E3

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rs) (y,x) cell =
   Sudoku $ rs !!= (y,newRow)
    where newRow = (rs !! y) !!= (x,cell)

prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated sud pos cell =
  cell == update sud pos' cell `atPosition` pos'
 where
   pos' = inRange pos


inRange (y,x) = (abs (x `mod` 9) ,abs (y `mod` 9))

{-

-- * E4

candidates :: Sudoku -> Pos -> [Int]
candidates s pos = [ n | n <- [1..9], isOkay $ update s pos (Just n) ]

prop_candidates_correct :: Sudoku -> Pos -> Property

 -}
-- update a blank in an isOkay sudoku
-- with a good candidate gives an isOkay sudoku
{-
 prop_candidates_correct sud pos =
   isOkay sud && isNothing (sud `atPosition` pos') ==>
--   collect (length . filter isJust . concat $ rows sud) $
   all isOkay [update sud pos' (Just n) | n <- candidates sud pos']
    where pos' = inRange pos
 -}
------------------------------------------------------------------------------

-- * F1

solve :: Sudoku -> Maybe Sudoku
solve sud
  | isSudoku sud = listToMaybe (solve' sud (blanks sud))
  | otherwise  = Nothing

solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' sud holes
  | not (isOkay sud) = []
solve' sud []          = [sud]
solve' sud (h:hs)      =
    concat [solve' (update sud h (Just n)) hs | n <- [1..9]]

-- small optimisation made: isOkay only needs
-- to be checked once


-- * F2
{-
readAndSolve :: FilePath -> IO ()
readAndSolve file = do
   sud <- readSudoku file
   case solve sud of
      Just sol -> printSudoku sol
      Nothing  -> putStrLn "No solution"
-}


-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool

s1 `isSolutionOf` s2 = ok s1 && ok s2 && s1 `matches` s2
  where ok s =  isSudoku s && isOkay s
        s1 `matches` s2 = and $ zipWith match (allcells s1) (allcells s2)
        allcells = concat . rows
        match (Just _)  Nothing  = True
        match (Just n) (Just m)  = n == m
        match _        _         = False

-- * F4
{- prop_SolveSound :: Sudoku -> Property
prop_SolveSound  sud = isSudoku sud ==>
         case solve sud of
            Nothing  -> True
            Just sol -> sol `isSolutionOf` sud -}
