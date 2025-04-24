module Checkers where
import Data.Maybe
import Data.Ord

data Move   = Move Piece Position deriving (Show, Eq, Ord)
data Team   =  Black | White deriving (Show, Eq, Ord)
data Winner = Victor Team | Stalemate deriving (Show, Eq)

type Position = (Int, Int)
type Game     = (Team,[Piece],Int)
type Piece    = (Position, (Bool, Team))--Bool is king status
--ask Fogarty about test framework usability, highlighting, error squiggles
showPiece :: (Bool,Team) -> String
showPiece (_,Black) = "○"
showPiece (_,White) = "●"

-- takes a position and checks whether there is a piece there, if yes then returns Just Piece, if no then returns Nothing
getPiece :: Game -> Position -> Maybe (Bool, Team)
getPiece (_,pieces,_) pos = lookup pos pieces

toString :: Game -> String
toString game = unlines $ boardRows ++ [footer]
    where rowString y game = show y ++ " " ++ unwords [cellString (x, y) game | x <- [1..8]]
          cellString pos game = maybe "." showPiece (getPiece game pos)
          footer = "  a b c d e f g h" 
          boardRows = [rowString y game | y <- [8,7..1]] 
