module CheckersBot where
import Checkers
import Data.Maybe
import Data.List
import Data.Ord
--just use the chess algorithm
type Rating = Int

whoWillWin :: Game -> Winner
whoWillWin game@(team, pieces, count) = 
    case winner game of
        Just w -> w
        Nothing ->  if any (== (Victor team)) outcomes then (Victor team)
                    else if any (== Stalemate) outcomes then Stalemate
                    else (Victor (oppositeTeam team))
            where outcomes = [whoWillWin (cMove game mv) | mv <- possibleGameMoves game]
whoMightWin :: Game -> Int -> Rating
whoMightWin game 0 = rateGame game 
whoMightWin game@(team, _, _) depth 
    | victor == Just (Victor White) = 1000 
    | victor == Just (Victor Black) = -1000 
    | team == White = maximum scores 
    | otherwise = minimum scores
    where scores = [whoMightWin newGame (depth-1) | newMove <- possibleGameMoves game, let Just newGame = move game newMove]
          victor = winner game
checkForWinner :: Game -> (Game -> IO ()) -> IO ()
checkForWinner game cont = 
  case winner game of
    Nothing -> cont game
    Just Stalemate -> do printGame game
                         putStrLn "It's a draw!"
    Just (Victor w) -> do printGame game
                          putStrLn (show w ++ " wins!") 
winner :: Game -> Maybe Winner
winner game@(turn,pieces,count) 
    | count < 1 = Just Stalemate
    | null [w | w@(_, (_, White)) <- pieces] = Just $ Victor Black
    | null [b | b@(_, (_, Black)) <- pieces] = Just $ Victor White
    | null validMoves = Just $ Victor $ oppositeTeam turn
    | otherwise = Nothing
    where validMoves = possibleGameMoves game
winEval :: Winner -> String
winEval w = case w of
  Stalemate -> "a stalemate"
  (Victor team) -> "a victory for " ++ show team

rateGame :: Game -> Rating
rateGame game@(_,pieces,_) = 
    case winner game of
      Just Stalemate -> 0
      Just (Victor Black) -> -1000
      Just (Victor White) -> 1000
      Nothing -> wMaterial - bMaterial
    where wMaterial = sum [if king then 2 else 1 | (_, (king, team)) <- pieces, team == White]
          bMaterial = sum [if king then 2 else 1 | (_, (king, team)) <- pieces, team == Black]
rateEval :: Int -> String
rateEval i
 | i==0       ="a stalemate"
 | i==(-1000) ="a victory for black"
 | i==1000    ="a victory for white"
 | i>10       ="white is winning overwhelmingly"
 | i<(-10)    ="black is winning overwhelmingly"
 | i>10       ="white is winning"
 | i<(-0)     ="black is winning"
 | i>0        ="white is winning by a bit"
 | i<0        ="black is winning by a bit"

bestMove :: Game -> Move
bestMove game@(team, pieces, count) = if not $ null winnings then w else if not $ null ties then t else a
    where outputs = [(whoWillWin (cMove game mv), mv) | mv <- possibleGameMoves game]       
          winnings@(w:_) = [theMove | (winner, theMove) <- outputs, winner == (Victor team)]
          ties@(t:_) = [theMove | (winner, theMove) <- outputs, winner == Stalemate]
          (a:_) = [theMove | (_, theMove) <- outputs]
putBestMove :: Game -> IO ()
putBestMove game = putStrLn $ show $ bestMove game
goodMove :: Game -> Int -> Move
goodMove game@(team, pieces, count) depth = if team == Black then mins else maxs
    where outputs = [(whoMightWin newGame depth, newMove) | newMove <- possibleGameMoves game, let Just newGame = move game newMove]
          (_,mins) = (minimum outputs)
          (_,maxs) = (maximum outputs)
