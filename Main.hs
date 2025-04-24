module Main where
import Checkers
import Testing
import TestInputs
import System.IO
import System.Environment
import System.Console.GetOpt

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
    else printGame jumpGame
