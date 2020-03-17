import Data.List
import Data.Ord
import GHC.Generics

------------------------------------------------------------------------------
-- Variables/Constants
------------------------------------------------------------------------------

-- Coordinates are 0 - 8 since in 9x9 grid our coordinate range is
-- (0, 0) - (8, 8)
coordinateValues = [0..8]

board:: Puzzle
board =
    [ [0, 0, 0, 0, 6, 2, 3, 0, 0]
    , [3, 4, 9, 0, 1, 0, 7, 0, 0]
    , [0, 5, 0, 4, 3, 0, 0, 0, 1]
    , [0, 0, 2, 6, 5, 0, 0, 0, 9]
    , [0, 0, 8, 0, 4, 0, 1, 6, 2]
    , [0, 6, 4, 2, 9, 1, 0, 3, 8]
    , [0, 0, 0, 0, 0, 6, 0, 0, 0]
    , [0, 8, 0, 0, 7, 0, 0, 5, 4]
    , [9, 0, 0, 3, 2, 0, 6, 0, 7]
    ]

------------------------------------------------------------------------------
-- Types and Data Types
------------------------------------------------------------------------------

-- Cell represents a slot in the puzzle.
-- If the cell == 0, the cell is considered to be empty
-- If the cell contains a non-zero value, it was either
-- set initially to a value,
-- or has been filled by the solver 
type Cell = Int

-- A row is a list of integers that can contain numbers between 0 - 9
-- Once solved, a row must have atleast one of each values 1 - 9
type Row = [Cell]

-- A column is a list of integers that can contain numbers between 0 - 9
-- Once solved, a column must have atleast one of each values 1 - 9
type Column = [Cell]

-- A box is a 3x3 set of cells that can contains numbers between 0 - 9
-- Once solved, a block must have atleast one of each values 1 - 9 
type Block = [Cell]

-- The Sudoku puzzle
-- A list of list of ints that can contain values 0 - 9
-- 0 represents a blank cell
type Puzzle = [Row]

-- A location on the puzzle
-- Range from (0, 0) - (8, 8)
type Coordinate = (Int, Int)

-- A list of possible values for each blank cell
type Possibilities = [(Coordinate, [Int])]

------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------

------------------------------------
-- Helper Functions
------------------------------------

takeThree :: [int] -> [[int]]
takeThree [] = []
takeThree (a:b:c:ds) = [a,b,c] : takeThree ds

------------------------------------
-- Altering the board
------------------------------------

concatBoard :: Puzzle -> [Cell]
concatBoard p = concat p

combine :: [a] -> [a] -> [[a]]
combine x y = [x,y]

------------------------------------
-- Getting parts of the board
------------------------------------

-- List of list of every row in the puzzle
-- Redundant since it is the same format as the puzzle,
-- but put in for clarities sake
rows :: Puzzle -> [Row]
rows p = p 

-- List of lists of all columns in a puzzle
columns :: Puzzle -> [Column]
columns p = transpose p 

blocks :: Puzzle -> [Block]
blocks =  map concat . takeThree . concat . transpose . map takeThree 

-- A list of all blank (initiallized to zero) cells in a puzzle
blankCoordinates :: Puzzle -> [Coordinate]
blankCoordinates  p = [(x, y) | x <- coordinateValues, y <- coordinateValues
                                                     , p !! x !! y == 0]

-- Gets row r from puzzle p
-- r = row to get 
getRow :: Int -> Puzzle -> Row
getRow r p = (rows p) !! r

-- Gets column c from puzzle p
-- c = column to get 
getColumn :: Int -> Puzzle -> Column
getColumn c p = (columns p) !! c

-- Gets block b from puzzle p
-- b = block to get
getBlock :: Coordinate -> Puzzle -> Block
getBlock (row,col) p | (row < 3 && col < 3) = (blocks p) !! 0
                     | (row < 6 && col < 3) = (blocks p) !! 1 
                     | (row < 9 && col < 3) = (blocks p) !! 2
                     | (row < 3 && col < 6) = (blocks p) !! 3
                     | (row < 6 && col < 6) = (blocks p) !! 4
                     | (row < 9 && col < 6) = (blocks p) !! 5
                     | (row < 3 && col < 9) = (blocks p) !! 6
                     | (row < 6 && col < 9) = (blocks p) !! 7
                     | (row < 9 && col < 9) = (blocks p) !! 8

-- Takes a list of blankCoordinates and outputs a list of list of all possbile values
-- for each blank coordinate
possibleCellValues :: [Coordinate] -> Puzzle -> [[Int]]
possibleCellValues [] p = []
possibleCellValues (x:xs) p = [i | i <- [1..9], checkAll i x p] : possibleCellValues xs p

------------------------------------
-- Checking for valid digits 
------------------------------------

-- Checks to see if an Int in some cell for some row is possible
checkRow :: Int -> Row -> Bool
checkRow c r = notElem c r 

-- Checks to see if an Int in some cell for some column is possible
checkColumn :: Int -> Column -> Bool
checkColumn cell column = notElem cell column

-- Checks to see if an Int in some cell for some block is possible
checkBlock :: Int -> Block -> Bool
checkBlock c b = notElem c b

-- Returns true if integer i isn't in the row, column, or block of the coordinate
checkAll :: Int -> Coordinate -> Puzzle -> Bool
checkAll i (row,col) p = checkRow i (getRow row p)
                      && checkColumn i (getColumn col p)
                      && checkBlock i (getBlock (row,col) p)




















--main = do
-- putStr . unlines $ map show easyBoard
