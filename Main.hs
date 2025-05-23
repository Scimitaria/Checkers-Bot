module Main where
import Checkers
import CheckersBot
import Parsing
import Testing
import TestInputs
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Maybe
import Text.Read

data Flag = Winner | Depth String | MoveFl String | Verbose | Interactive 
          | Interactive2p | Hard | Cheats | Help | Test  deriving (Show, Eq)

options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"]        (NoArg Winner)           "Print best move and exit."
          , Option ['d'] ["depth"]         (ReqArg Depth "<num>")   "Specifies a cutoff depth."
          , Option ['m'] ["move"]          (ReqArg MoveFl "<move>") "Print the result of the move and exit."
          , Option ['v'] ["verbose"]       (NoArg Verbose)          "Pretty-print the result of the move and exit."
          , Option ['i'] ["interactive"]   (NoArg Interactive)      "Play a game against the computer."
          , Option ['2'] ["interactive2p"] (NoArg Interactive2p)    "Play a two-player game of checkers."
          , Option ['c'] ["cheats"]        (NoArg Cheats)           "enable illegal moves."
          , Option ['h'] ["help"]          (NoArg Help)             "Print usage information and exit."
          , Option ['t'] ["test"]          (NoArg Test)             "Run tests and exit."
          ]

main :: IO()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    if (Help `elem` flags) || (not $ null errors)
    then putStrLn $ usageInfo "Checkers [options] [filename] checkers game." options
    else if Test `elem` flags then runTests 1 True
    else do let fName = if (null args)||(null inputs) then "initial.txt" else head inputs
            let getDepth :: [Flag] -> Maybe Int
                getDepth [] = Just 3
                getDepth (Depth d:_) = readMaybe d
                getDepth (_:fs) = getDepth fs
            contents <- readFile fName
            case (readGame contents, getDepth flags) of
              (Nothing, _) -> putStrLn "Error 404: Game not found"
              (game, Nothing) -> putStrLn "Error 404: Depth not found"
              (Just game,Just depth) -> dispatch flags game depth

dispatch :: [Flag] -> Game -> Int -> IO()
dispatch fs game depth 
  | any isMove fs = moveIO fs game
  | Winner `elem` fs = 
      if Verbose `elem` fs 
      then putStrLn ("The best move is: " ++ show wM ++ "; the outcome will be: " ++ (winEval (whoWillWin game))) 
      else putStrLn ("The best move is: " ++ show wM)
  | Interactive `elem` fs = do
    t <- teamSelect
    interactive (Cheats `elem` fs) t depth game
  | Interactive2p `elem` fs = interactive2p (Cheats `elem` fs) depth game
  | otherwise = if Verbose `elem` fs then putStrLn ("Try: " ++ show dM ++ "; its rating is " ++ dR)
  else putStrLn ("Try: " ++ show dM)
    where wM = bestMove game
          dM = goodMove game depth
          dR = case move game dM of
                Just g -> rateEval $ rateGame game
                Nothing -> "error: invalid move"

interactive :: Bool -> Team -> Int -> Game -> IO()
interactive isCheat team depth game@(turn,pieces,turns) = do
  printGame game
  putStrLn (show turn ++ "'s turn; " ++ show turns ++ " turns remaining")
  putStrLn ("moves: " ++ (show $ possibleGameMoves game))
  if turn /= team
  then case move game (goodMove game depth) of
         Just g -> checkForWinner g (interactive isCheat team depth)
         Nothing -> putStrLn "guys I think the AI is borked"
  else do
    m0 <- prompt "enter move"
    case m0 of
      "forfeit" -> putStrLn (show (oppositeTeam team) ++ " wins") 
      "save" -> do filename <- prompt "enter desired file name"
                   writeGame game (filename++".txt")
                   interactive isCheat team depth game
      "predict" -> putStrLn (rateEval $ whoMightWin game depth)
      ('a':'d':'d':x) -> 
        case parsePiece x of
          Just p -> interactive isCheat team depth (turn,(p:pieces),turns)
          Nothing -> do putStrLn "invalid piece"
      ('r':'e':'m':x) -> 
        case parsePiece x of
          Just p -> interactive isCheat team depth (turn,[piece | piece <- pieces, piece/=p],turns)
          Nothing -> do putStrLn "invalid piece"
                        interactive isCheat team depth game
      _ ->
        let m = readMove game m0
            nG = (if isCheat then Just . (cMove game) else move game) =<< m
        in case (m,nG) of
            (Just movefl,Just g) -> checkForWinner g (interactive isCheat team depth)
            (Just movefl,Nothing) -> do putStrLn "Error: illegal move"
                                        interactive isCheat team depth game
            (Nothing,_) -> do putStrLn "Error: invalid move"
                              interactive isCheat team depth game

interactive2p :: Bool -> Int -> Game -> IO()
interactive2p isCheat depth game@(turn,pieces,turns) = do
    printGame game
    putStrLn (show turn ++ "'s turn; " ++ show turns ++ " turns remaining")
    m0 <- prompt "enter move"
    case m0 of
      "forfeit" -> putStrLn (show (oppositeTeam turn) ++ " wins")
      "save" -> do filename <- prompt "enter desired file name"
                   writeGame game (filename++".txt")
                   interactive2p isCheat depth game
      ('a':'d':'d':x) -> 
        case parsePiece x of
          Just p -> interactive2p isCheat depth (turn,(p:pieces),turns)
          Nothing -> do putStrLn "invalid piece"
                        interactive2p isCheat depth game
      "predict" -> putStrLn (rateEval $ whoMightWin game depth)
      ('r':'e':'m':x) -> 
        case parsePiece x of
          Just p -> interactive2p isCheat depth (turn,[piece | piece <- pieces, piece/=p],turns)
          Nothing -> do putStrLn "invalid piece"
                        interactive2p isCheat depth game
      _ ->
        let m = readMove game m0
            nG = (if isCheat then Just . (cMove game) else move game) =<< m
        in case (m,nG) of
            (Just movefl,Just g) -> checkForWinner g (interactive2p isCheat depth)            
            (Just movefl,Nothing) -> do putStrLn "Error: illegal move"
                                        interactive2p isCheat depth game
            (Nothing,_) -> do putStrLn "Error: invalid move"
                              interactive2p isCheat depth game

isMove :: Flag -> Bool
isMove (MoveFl _) = True
isMove _ = False
getMoveFl :: [Flag] -> Game -> Maybe Move
getMoveFl [] _ = Nothing
getMoveFl (MoveFl m:_) game = readMove game m
getMoveFl (_:fs) game = getMoveFl fs game
moveIO :: [Flag] -> Game -> IO()
moveIO flags game = 
  case getMoveFl flags game of
    Just movefl -> 
        case move game movefl of
          Just g -> if Verbose `elem` flags then printGame g else putStrLn $ showGame g
          Nothing -> putStrLn "Error: illegal move"
    Nothing -> putStrLn "Error: invalid move"

prompt :: String -> IO String
prompt str = 
  do putStr $ str ++ ": "
     hFlush stdout
     answer <- getLine
     return answer
teamSelect :: IO Team
teamSelect = do
  team <- prompt "Select your team"
  if team `elem` ["White","white","W","w"] then return White else return Black
