module HumanSolverGame (startGame) where

import CodeBuilder (makeCode)
import GameMechanics
import Data.Char (toUpper)
import System.IO


playRound :: Answer -> Int -> IO ()
playRound answer roundNum =
  do
    putStr "Please enter a guess $ "
    hFlush stdout
    guess <- (fmap . fmap) toUpper getLine
    case checkGuess answer <$> validateGuess answer guess of
      Left msg ->
        putStrLn msg >>
        playRound answer roundNum
      Right (AnswerResult 4 0) -> endGame "You win!"
      Right result -> putStrLn ("Not quite... " ++ show result) >>
        if roundNum >= numRounds
          then endGame ("Sorry, the correct code was: " ++ answer)
          else playRound answer (roundNum + 1)
      

startGame :: IO ()
startGame = do
  code <- makeCode
  playRound code 1
