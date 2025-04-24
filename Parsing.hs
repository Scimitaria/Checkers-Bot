module Parsing where
import Checkers
import Text.Read
import Data.List
import Data.List.Split


letterToNum :: Char -> Maybe Int
letterToNum letter = 
    case letter of
      'a' -> Just 1
      'b' -> Just 2
      'c' -> Just 3
      'd' -> Just 4
      'e' -> Just 5
      'f' -> Just 6
      'g' -> Just 7
      'h' -> Just 8
      _ -> Nothing
parsePosition :: String -> Maybe Position
parsePosition str = do
    (xStr, yStr) <- case str of
        xStr:yStr:[] -> Just (xStr,[yStr])
        _ -> Nothing
    x <- letterToNum xStr
    y <- readMaybe yStr
    return (x,y)
readMove :: Game -> String -> Maybe Move
readMove game str = 
    do 
      let 
      (oldStr, newStr) <- case (splitOn "," str) of
          (oldStr:newStr:_) -> Just (oldStr, newStr)
          _ -> Nothing
      oldPos <- parsePosition oldStr
      newPos <- parsePosition newStr
      oldPieceParts <- getPiece game oldPos
      return (Move (oldPos,oldPieceParts) newPos)

parseKing :: String -> Maybe King
parseKing str = do
    king <- if str == "t" then Just True else if str == "f" then Just False else Nothing
    return king
parseTeam :: String -> Maybe Team
parseTeam str = do
    team <- if str == "w" then Just White else if str == "b" then Just Black else Nothing
    return team
parsePiece :: String -> Maybe Piece
parsePiece str = do
    (teamStr,kingStr,xStr,yStr) <- case str of
        (teamStr:kingStr:xStr:yStr:_) -> Just ([teamStr],[kingStr],xStr,[yStr])
        _ -> Nothing
    team <- parseTeam teamStr
    king <- parseKing kingStr
    x <- letterToNum xStr
    y <- readMaybe yStr
    return ((x,y),(king,team))
readGame :: String -> Maybe Game
readGame str = do
    (teamStr, counterStr, piecesStr) <- case lines str of 
        (teamStr:counterStr:piecesStr:_) -> Just (teamStr,counterStr,piecesStr)
        _ -> Nothing
    team <- parseTeam teamStr
    pieces <- sequence [parsePiece pieceStr | pieceStr <- words piecesStr]
    counter <- readMaybe counterStr
    return (team, pieces, counter)

showPos :: Position -> String
showPos pos = case pos of
    (1,y) -> "a" ++ show y
    (2,y) -> "b" ++ show y
    (3,y) -> "c" ++ show y
    (4,y) -> "d" ++ show y
    (5,y) -> "e" ++ show y
    (6,y) -> "f" ++ show y
    (7,y) -> "g" ++ show y
    (8,y) -> "h" ++ show y
    _ -> error "error parsing position"
showPiece :: (King,Team) -> String
showPiece piece = case piece of
    (True ,White) -> "wt"
    (False,White) -> "wf"
    (True ,Black) -> "bt"
    (False,Black) -> "bf"
showGame :: Game -> String
showGame game@(turn, pieces, turns) = 
    intercalate "\n" [showTurn turn, show turns, unwords piecesStrs]
  where showTurn :: Team -> String
        showTurn White = "w"
        showTurn Black = "b"
        piecesStrs = [showPiece (pType, pTeam) ++ showPos(x,y) | piece@((x,y), (pType, pTeam)) <- pieces]

writeGame :: Game -> FilePath -> IO ()
writeGame game path = do
    writeFile path (showGame game)
    return ()
loadGame :: FilePath -> IO (Maybe Game)
loadGame path = do
  str <- readFile path
  return $ readGame str
