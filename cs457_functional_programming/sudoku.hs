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

------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------

------------------------------------
-- Altering the board
------------------------------------

concatBoard :: Puzzle -> [Cell]
concatBoard p = concat p

------------------------------------
-- Getting parts of the board
------------------------------------

-- Redundant, but put in for clarities sake
rows :: Puzzle -> [Row]
rows p = p 

-- List of lists of all columns in a puzzle
columns :: Puzzle -> [Column]
columns p = transpose p 

--blocks :: Puzzle -> [Block]
--blocks p = (concatBoard p)

-- A list of all blank (initiallized to zero) cells in a puzzle
blankCoordinates :: Puzzle -> [Coordinate]
blankCoordinates  p = [(x, y) | x <- coordinateValues, y <- coordinateValues, p !! x !! y == 0]






--main = do
-- putStr . unlines $ map show easyBoard
