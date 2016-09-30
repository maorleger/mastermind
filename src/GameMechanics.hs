module GameMechanics where

import Data.List (intersect)
import System.IO
import System.Exit (exitSuccess)

numRounds :: Int
numRounds = 7

data AnswerResult = AnswerResult Int Int deriving (Eq)

instance Show AnswerResult where
  show (AnswerResult x y) = "[" ++ show x ++ "," ++ show y ++ "]"

instance Monoid AnswerResult where
  mempty = AnswerResult 0 0
  mappend (AnswerResult x y) (AnswerResult x' y') = AnswerResult (x + x') (y + y')

getIncorrectPositions :: Eq a => [(a,a)] -> Int
getIncorrectPositions a = 
  let 
    answer = map fst a
    guess = map snd a
  in length $ intersect answer guess

checkGuess :: Eq a => [a] -> [a] -> AnswerResult
checkGuess answer guess = 
  let 
    zippedGuesses = zip answer guess
    correctPositions = foldr (\(x, y) acc -> if x == y then acc + 1 else acc) 0 zippedGuesses
    leftOverPairs = filter (uncurry (/=)) zippedGuesses
    incorrectPositions = getIncorrectPositions leftOverPairs
  in
    AnswerResult correctPositions incorrectPositions


endGame :: String -> IO ()
endGame msg = do
  putStrLn msg
  exitSuccess

-- TODO: make this more generic, we should be able to handle many types
-- TODO: handle the case where our guess is of a different length than our answer....
playRound :: String -> Int -> IO ()
playRound answer roundNum = do
  putStr "Please enter a guess $ "
  hFlush stdout
  guess <- getLine
  case checkGuess answer guess of
    AnswerResult 4 0 -> endGame "You win!"
    result -> do
      putStr "Not quite... "
      print result
      if roundNum > numRounds then endGame "You lose" else playRound answer (roundNum + 1)
      