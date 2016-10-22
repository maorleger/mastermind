module HumanSolverGame (startGame) where

import CodeBuilder
import GameMechanics
import Data.Char (toUpper)
import System.IO
import Data.Monoid ((<>))


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
      Right result -> putStrLn ("Not quite... " <> show result) >>
        if roundNum >= numRounds
          then endGame ("Sorry, the correct code was: " <> show answer)
          else playRound answer (roundNum + 1)


startGame :: IO ()
startGame = do
  putStrLn $ "Hi there! I made up a code that consists of the following pegs: " <> show pegs
  putStrLn "You can guess the code by using the first letter of each color to represent a peg"
  putStrLn "For example, to guess [Blue, Orange, Pink, Yellow] you can input BOPY"
  code <- makeCode
  playRound code 1
