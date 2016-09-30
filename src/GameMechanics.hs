module GameMechanics where

import Data.List (delete)
import System.IO
import System.Exit (exitSuccess)
import Data.Char (toUpper)

numRounds :: Int
numRounds = 7

-- AnswerResult

data AnswerResult = AnswerResult Int Int deriving (Eq)

instance Show AnswerResult where
  show (AnswerResult x y) = "[" ++ show x ++ "," ++ show y ++ "]"

instance Monoid AnswerResult where
  mempty = AnswerResult 0 0
  mappend (AnswerResult x y) (AnswerResult x' y') = AnswerResult (x + x') (y + y')


incorrectPositionsCalc :: Eq a => [a] -> [a] -> Int
incorrectPositionsCalc _ [] = 0
incorrectPositionsCalc [] _ = 0
incorrectPositionsCalc answer (x':xs') = 
  if x' `elem` answer 
    then 1 + incorrectPositionsCalc (delete x' answer) xs'
    else 0 + incorrectPositionsCalc answer xs'


checkGuess :: Eq a => [a] -> [a] -> AnswerResult
checkGuess answer guess = 
  let 
    zippedGuesses = zip answer guess
    correctPositions = foldr (\(x, y) acc -> if x == y then acc + 1 else acc) 0 zippedGuesses
    leftOverPairs = filter (uncurry (/=)) zippedGuesses
    incorrectPositions = incorrectPositionsCalc (map fst leftOverPairs) (map snd leftOverPairs)
  in
    AnswerResult correctPositions incorrectPositions


endGame :: String -> IO ()
endGame msg = putStrLn msg >> exitSuccess


playRound :: String -> Int -> IO ()
playRound answer roundNum = do
  putStr "Please enter a guess $ "
  hFlush stdout
  guess <- getLine
  if length guess /= length answer then replayRound else checkRound guess
  where
    replayRound = do
      putStrLn $ "Your guess needs to be " ++ (show . length $ answer) ++ " characters long"
      playRound answer roundNum
    checkRound guess = 
      case checkGuess answer (map toUpper guess) of
        AnswerResult 4 0 -> endGame "You win!"
        result -> putStrLn ("Not quite... " ++ show result) >>
          if roundNum > numRounds then endGame ("Sorry, the correct code was: " ++ answer) else playRound answer (roundNum + 1)
      