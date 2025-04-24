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

testCanJump :: Grader String
testCanJump = assess "canJump" 0 $ do
    check "if you can jump"             $ canJump jumpGame    ((7,5),(False,Black)) (6,4) `shouldBe` True
    check "if you can't jump"           $ canJump jumpGame    ((2,6),(False,Black)) (1,5) `shouldBe` False
    check "if you can jump backwards"   $ canJump kingedGame1 ((3,3),(True,Black))  (4,4) `shouldBe` True
    check "if you can't jump backwards" $ canJump kingedGame1 ((3,3),(True,Black))  (2,4) `shouldBe` False

testCanMake :: Grader String
testCanMake = assess "canMake" 0 $ do
    check "if a white piece can move"           $ canMake initialGame ((1,3),(False,White)) (2,4) `shouldBe` True
    check "if a black piece can move"           $ canMake game1       ((2,6),(False,Black)) (1,5) `shouldBe` True
    check "if you can jump"                     $ canMake jumpGame    ((7,5),(False,Black)) (5,3) `shouldBe` True
    check "that you can't move straight"        $ canMake initialGame ((1,3),(False,White)) (1,4) `shouldBe` False
    check "that you can't move out of bounds"   $ canMake game1       ((8,6),(False,Black)) (9,5) `shouldBe` False
    check "that you can't move to a full space" $ canMake jumpGame    ((7,5),(False,Black)) (6,4) `shouldBe` False

testMove :: Grader String
testMove = assess "move" 0 $ do
    check "that move can move a white piece"    $ getPiece (fromJust $ move initialGame (Move ((1,3),(False,White)) (2,4))) (2,4) `shouldBe` Just (False,White)
    check "that move can move a black piece"    $ getPiece (fromJust $ move game1       (Move ((2,6),(False,Black)) (1,5))) (1,5) `shouldBe` Just (False,Black)
    check "that move can jump"                  $ getPiece (fromJust $ move jumpGame    (Move ((7,5),(False,Black)) (5,3))) (5,3) `shouldBe` Just (False,Black)
    check "that you can't move straight"        $ move initialGame (Move ((1,3),(False,Black)) (1,4)) `shouldBe` Nothing
    check "that you can't move out of bounds"   $ move game1       (Move ((8,6),(False,Black)) (9,5)) `shouldBe` Nothing
    check "that you can't move to a full space" $ move jumpGame    (Move ((7,5),(False,Black)) (6,4)) `shouldBe` Nothing
    check "that you can't move backwards"       $ move kingGame    (Move ((7,7),(False,White)) (6,6)) `shouldBe` Nothing
    check "that you can't jump backwards"       $ move kingedGame1 (Move ((3,3),(True,Black))  (1,5)) `shouldBe` Nothing
    check "that you can king a white piece"     $ getPiece (fromJust $ move kingGame    (Move ((7,7),(False,White)) (8,8))) (8,8) `shouldBe` Just (True,White)
    check "that you can king a black piece"     $ getPiece (fromJust $ move kingGameB   (Move ((2,2),(False,Black)) (1,1))) (1,1) `shouldBe` Just (True,Black)
    check "that you can take a white piece"     $ whitePieces `shouldNotContain` ((7,5),(False,Black))  
    check "that you can take a black piece"     $ blackPieces `shouldNotContain` ((6,4),(False,White))
    check "that a king can move backwards"      $ getPiece (fromJust $ move kingedGame  (Move ((3,3),(True,White))  (2,2))) (2,2) `shouldBe` Just (True,White)
    check "that a king can jump backwards"      $ getPiece (fromJust $ move kingedGame1 (Move ((3,3),(True,Black))  (5,5))) (5,5) `shouldBe` Just (True,Black)
    where (_,whitePieces,_) = fromJust $ move jumpGameW (Move ((6,4),(False,White)) (8,6))
          (_,blackPieces,_) = fromJust $ move jumpGame  (Move ((7,5),(False,Black)) (5,3))

tree :: Grader String
tree = describe "Checkers-Bot" $
    do describe "Checkers" $ do
        testGetPiece
        testCanJump
        testCanMake
        testMove

runTests :: Int -> Bool -> IO ()--how do test io
runTests verb force = do
        let a = runGrader tree
        format <- makeFormat verb force "projectDesc.yaml"
        runRWST a () format
        return ()
