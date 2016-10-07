module HillClimbingSolverGame where

import GameMechanics
import Data.Char (toUpper)
import qualified CodeBuilder
import System.IO
import System.Random
import Control.Applicative

readScore :: IO (Int, Int)
readScore = readLn

data CFG = CFG Guess AnswerResult


-- heuristic gives the distance to the goal by penalizing
-- the pegs with the right colours but in wrong position.
computeScore :: AnswerResult -> Int
computeScore (AnswerResult black white) = 
  case (black, white) of
    (0, 0) -> 0
    (0, 1) -> 1
    (1, 0) -> 2
    (0, 2) -> 3
    (1, 1) -> 4
    (2, 0) -> 5
    (0, 3) -> 6
    (1, 2) -> 7
    (2, 1) -> 8
    (3, 0) -> 9
    (0, 4) -> 10
    (1, 3) -> 11
    (2, 2) -> 12
    (4, 0) -> 13


-- TODO: I HAVE TO REMOVE THE ORPHANED INSTANCE HERE!!!!
instance Ord AnswerResult where
  compare result result' = compare (computeScore result) (computeScore result')


randomPegsToKeep :: [Int] -> Int -> IO [Int]
randomPegsToKeep positionsKept 0 = return positionsKept
randomPegsToKeep positionsKept pegsLeft = do
  randomNumber <- randomRIO (0, 3)
  case randomNumber `elem` positionsKept of
    True -> randomPegsToKeep positionsKept pegsLeft
    False -> randomPegsToKeep (randomNumber : positionsKept) (pegsLeft - 1)

randomPegsToShift = undefined


genCode :: String -> CFG -> Guess
genCode pegs (CFG guess (AnswerResult blackPegs whitePegs)) = 
  let 
    pegsToKeep = randomPegsToKeep [] blackPegs
    pegsToShift = randomPegsToShift pegsToKeep whitePegs
  in
    "ABCD"

genInitialCFG :: IO CFG
genInitialCFG = do
  code <- CodeBuilder.makeCode 
  return $ CFG code (AnswerResult 0 0)


checkForGameOver :: (Int, Int) -> Int -> IO ()
checkForGameOver (4, 0) _ = endGame "I am the MACHINE!"
checkForGameOver _ roundNum | roundNum > numRounds = endGame "You lose"
                            | otherwise = return ()


playRound :: CFG -> String -> Int -> IO ()
playRound cfg@(CFG cfGuess cfResult) pegs roundNum =
  let 
    chooseCFG score = case (uncurry AnswerResult score) > cfResult of
                          True -> CFG guess (uncurry AnswerResult score)
                          False -> cfg
    guess = genCode pegs cfg
  in do

    putStrLn $ "My guess is: " ++ guess
    putStrLn "How did I do?"
    score <- readScore
    checkForGameOver score roundNum
    playRound (chooseCFG score) pegs (roundNum + 1)
  
startGame :: IO ()
startGame = do
  cfg <- genInitialCFG
  playRound cfg CodeBuilder.pegs 1
  return ()