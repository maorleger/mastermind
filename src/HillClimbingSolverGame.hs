module HillClimbingSolverGame where

import GameMechanics
import Data.Char (toUpper)
import CodeBuilder
import System.IO
import System.Random

readScore :: IO (Int, Int)
readScore = readLn

-- heuristic gives the distance to the goal by penalizing
-- the pegs with the right colours but in wrong position.
computeScore :: AnswerResult -> Integer
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


randomPegsTokeep = undefined
randomPegsToShift = undefined


genCode :: [String] -> Guess -> AnswerResult -> Guess
genCode possibilities prevGuess (AnswerResult blackPegs whitePegs) = 
  let 
    pegsToKeep = randomPegsTokeep "ABCDEF" blackPegs
    pegsToShift = randomPegsToShift pegsToKeep whitePegs
  in
    "ABCD"



-- TODO: 
-- 1. Be able to read [1,2] and translate it to an AnswerResult
playRound :: Guess -> AnswerResult -> Int -> IO ()
playRound guess prevResult roundNum = do
  putStrLn $ "My guess is: " ++ guess
  putStrLn $ "How did I do?"
  score <- readScore
  print $ computeScore (AnswerResult (fst score) (snd score))
  return ()



startGame :: IO ()
startGame = do
  playRound (genCode possibilities "" (AnswerResult 0 0)) (AnswerResult 0 0) 1
  return ()