module HillClimbingSolverGame where

import GameMechanics
import Data.Char (toUpper)
import CodeBuilder
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


instance Ord AnswerResult where
  compare result result' = compare (computeScore result) (computeScore result')


randomPegsTokeep = undefined
randomPegsToShift = undefined


genCode :: [String] -> CFG -> Guess
genCode possibilities (CFG guess (AnswerResult blackPegs whitePegs)) = 
  let 
    pegsToKeep = randomPegsTokeep "ABCDEF" blackPegs
    pegsToShift = randomPegsToShift pegsToKeep whitePegs
  in
    "ABCD"

genInitialCFG :: IO CFG
genInitialCFG = do
  code <- makeCode 
  return $ CFG code (AnswerResult 0 0)


checkForGameOver :: (Int, Int) -> Int -> IO ()
checkForGameOver (4, 0) _ = endGame "I am the MACHINE!"
checkForGameOver _ roundNum | roundNum > numRounds = endGame "You lose"
                            | otherwise = return ()


playRound :: CFG -> [String] -> Int -> IO ()
playRound cfg@(CFG cfGuess cfResult) possibilities roundNum =
  let 
    chooseCFG score = case (uncurry AnswerResult score) > cfResult of
                          True -> CFG guess (uncurry AnswerResult score)
                          False -> cfg
    guess = genCode possibilities cfg
  in do

    putStrLn $ "My guess is: " ++ guess
    putStrLn "How did I do?"
    score <- readScore
    checkForGameOver score roundNum
    playRound (chooseCFG score) possibilities (roundNum + 1)
  
startGame :: IO ()
startGame = do
  cfg <- genInitialCFG
  playRound cfg possibilities 1
  return ()