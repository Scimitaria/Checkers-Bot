module Main where
import Checkers
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
              (Just game,Just depth) -> printGame game--dispatch flags game depth
