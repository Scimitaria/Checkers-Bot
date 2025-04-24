module Testing where
import Checkers
import TestInputs
import Data.List
import Data.Semigroup
import Test.Grader.Tests
import Test.Grader.Core
import Test.Grader.Eval
import Test.Grader.Rubric
import Control.Monad.Extra
import Control.Monad.Trans.RWS
import System.IO.Unsafe
import Data.Maybe

testGetPiece :: Grader String
testGetPiece = assess "getPiece" 0 $ do--The number denotes the point value, which isn't particularly meaningful for our purposes
        check "that getPiece returns a piece where one exists" $ getPiece initialGame (1,1) `shouldBe` Just (False,White)
        check "that getPiece does not return a piece where one does not exist" $ getPiece initialGame (1,2) `shouldBe` Nothing

tree :: Grader String
tree = describe "Checkers-Bot" $ 
    do describe "Checkers" $ do
        testGetPiece

runTests :: Int -> Bool -> IO ()--how do test io
runTests verb force = do
        let a = runGrader tree
        format <- makeFormat verb force "projectDesc.yaml"
        runRWST a () format
        return ()
