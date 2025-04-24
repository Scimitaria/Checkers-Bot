module Checkers where
import Data.Maybe
import Data.Ord
import Debug.Trace
--ask Fogarty about test framework usability, highlighting, error squiggles
data Move   = Move Piece Position     deriving (Show, Eq, Ord)
data Team   = Black | White           deriving (Show, Eq, Ord)
data Winner = Victor Team | Stalemate deriving (Show, Eq)

type King     = Bool
type Position = (Int,Int)
type Game     = (Team,[Piece],Int)
type Piece    = (Position, (King,Team))

getPiece :: Game -> Position -> Maybe (King,Team)
getPiece (_,pieces,_) pos = lookup pos pieces
getTeamPieces :: Game -> Team -> [Piece]
getTeamPieces (_,pieces,_) White = [piece | piece <- pieces, getPieceTeam piece == White]
getTeamPieces (_,pieces,_) Black = [piece | piece <- pieces, getPieceTeam piece == Black]

getPieceTeam :: Piece -> Team
getPieceTeam (_,(_,c)) = c
getPosition :: Piece -> Position
getPosition ((x,y), (_,_)) = (x,y)
isKing :: Piece -> King
isKing (_,(king,_)) = king

opposite :: Game -> Piece -> [Piece]
opposite game (_, (_, White)) = getTeamPieces game Black
opposite game (_, (_, Black)) = getTeamPieces game White
oppositeTeam :: Team -> Team
oppositeTeam White = Black
oppositeTeam Black = White

canJump :: Game -> Piece -> Position -> Bool
canJump game ((x,y),(king,team)) new =
    if (new `elem` [(x+1,y+1),(x-1,y+1)])&&((team==White)||king)||
       (new `elem` [(x-1,y-1),(x+1,y-1)])&&((team==Black)||king) then
       let op = oppositeTeam team
       in case getPiece game new of
           Just (_,op) -> True
           _ -> False
    else False
canMake :: Game -> Piece -> Position -> Bool
canMake game@(turn,pieces,_) piece@(pos@(x1,y1),(king,team)) newPos =
    if (not $ inBounds newPos)||(not isEmpty) then False else
    case (turn,team) of
      (White,White) -> let k = king&&((newPos==(x1-1,y1-1))||(newPos==(x1+1,y1-1)))        --backwards move
                       in  k||((newPos `elem` [(x1+1,y1+1),(x1-1,y1+1)])||                  --regular move
                           (newPos==(x1+2,y1+2))&&(canJump game piece (x1+1,y1+1))||        --jump
                           (newPos==(x1-2,y1+2))&&(canJump game piece (x1-1,y1+1)))||       --jump
                           king&&((newPos==(x1-2,y1-2))&&(canJump game piece (x1-1,y1-1))|| --backwards jump
                           (newPos==(x1+2,y1-2))&&(canJump game piece (x1+1,y1-1)))         --backwards jump
      (Black,Black) -> let k = king&&((newPos==(x1+1,y1+1))||(newPos==(x1-1,y1+1)))         --backwards move
                       in  k||(newPos `elem` [(x1-1,y1-1),(x1+1,y1-1)])||                   --regular move
                           (newPos==(x1-2,y1-2))&&(canJump game piece (x1-1,y1-1))||        --jump
                           (newPos==(x1+2,y1-2))&&(canJump game piece (x1+1,y1-1))||        --jump
                           king&&((newPos==(x1+2,y1+2))&&(canJump game piece (x1+1,y1+1))|| --backwards jump
                           (newPos==(x1-2,y1+2))&&(canJump game piece (x1-1,y1+1)))         --backwards jump
      (_,_) -> False
    where isEmpty = isNothing $ getPiece game newPos
          inBounds :: Position -> Bool
          inBounds (x,y) = x>0 && x<9 && y>0 && y<9

move :: Game -> Move -> Maybe Game
move game@(turn,pieces,count)  mv@(Move old newPos)
  | not $ canMake game old newPos = Nothing --Can't make move
  | getPieceTeam old /= turn      = Nothing --Attempted move is by the wrong team
  | otherwise = Just $ cMove game mv
cMove :: Game -> Move -> Game
cMove game@(turn,pieces,count) (Move old@((x,y),(_,team)) newPos) =
    case (turn,team) of  --jump
      (White,White) -> if      (newPos==(x+2,y+2)) then (newTeam,whites++[p | p@(k,t) <- pieces,p /=((x+1,y+1),(fromJust $ getPiece game (x+1,y+1)))],newCount)
                       else if (newPos==(x-2,y+2)) then (newTeam,whites++[p | p@(k,t) <- pieces,p /=((x-1,y+1),(fromJust $ getPiece game (x-1,y+1)))],newCount)
                       else newGame
      (Black,Black) -> if      (newPos==(x-2,y-2)) then (newTeam,[p | p@(k,t) <- pieces, p/=((x-1,y-1),(fromJust $ getPiece game (x-1,y-1)))]++blacks,newCount)
                       else if (newPos==(x+2,y-2)) then (newTeam,[p | p@(k,t) <- pieces, p/=((x+1,y-1),(fromJust $ getPiece game (x+1,y-1)))]++blacks,newCount)
                       else newGame
      (_,_) -> newGame
    where whites = getTeamPieces game White
          blacks = getTeamPieces game Black
          newPiece = promote (newPos,(isKing old,getPieceTeam old))
          newGame@(newTeam,pieces,newCount) = 
              if turn == White
                then (Black,replacePiece whites old newPiece++blacks,count-1) 
                else (White,whites++replacePiece blacks old newPiece,count-1)
          replacePiece :: [Piece] -> Piece -> Piece -> [Piece]
          replacePiece pieces old new = new:[piece | piece <- pieces, piece /= old]
          promote :: Piece -> Piece
          promote ((x,8),(False,White)) = ((x,8),(True,White))
          promote ((x,1),(False,Black)) = ((x,1),(True,Black))
          promote piece = piece

printGame :: Game -> IO()
printGame game = putStrLn $ toString game
toString :: Game -> String
toString game = unlines $ boardRows ++ [footer]
    where rowString y game = show y ++ " " ++ unwords [cellString (x, y) game | x <- [1..8]]
          cellString pos game = maybe "." showPiece (getPiece game pos)
          footer = "  a b c d e f g h" 
          boardRows = [rowString y game | y <- [8,7..1]] 
          showPiece :: (King,Team) -> String
          showPiece (True,Black) = "⦾"
          showPiece (True,White) = "⦿"
          showPiece (_,Black)    = "○"
          showPiece (_,White)    = "●"
