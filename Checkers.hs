module Checkers where
import Data.Maybe

data Piece = Black | White deriving Show

showPiece :: Piece -> String
showPiece Black = "○"
showPiece White = "●"
