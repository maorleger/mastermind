module GameMechanics where

import Data.List (delete)
import System.IO
import System.Exit (exitSuccess)
import Data.Char (toUpper)

numRounds :: Int
numRounds = 7

data AnswerResult = AnswerResult Int Int deriving (Eq)

instance Show AnswerResult where
  show (AnswerResult x y) = "[" ++ show x ++ "," ++ show y ++ "]"

instance Monoid AnswerResult where
  mempty = AnswerResult 0 0
  mappend (AnswerResult x y) (AnswerResult x' y') = AnswerResult (x + x') (y + y')

getIncorrectPositions :: Eq a => [a] -> [a] -> Int
getIncorrectPositions _ [] = 0
getIncorrectPositions [] _ = 0
getIncorrectPositions answer (x':xs') = if x' `elem` answer then 1 + getIncorrectPositions (delete x' answer) xs' else 0 + getIncorrectPositions answer xs'
  
  -- let 
  --   answer = map fst a
  --   guess = map snd a
  --   getIncorrectPositions' answer guess = 
  -- in length $ intersect answer guess

checkGuess :: Eq a => [a] -> [a] -> AnswerResult
checkGuess answer guess = 
  let 
    zippedGuesses = zip answer guess
    correctPositions = foldr (\(x, y) acc -> if x == y then acc + 1 else acc) 0 zippedGuesses
    leftOverPairs = filter (uncurry (/=)) zippedGuesses
    incorrectPositions = getIncorrectPositions (map fst leftOverPairs) (map snd leftOverPairs)
  in
    AnswerResult correctPositions incorrectPositions


endGame :: String -> IO ()
endGame msg = do
  putStrLn msg
  exitSuccess

-- TODO: make this more generic, we should be able to handle many types
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
        result -> do
          putStr "Not quite... "
          print result
          if roundNum > numRounds then endGame "You lose" else playRound answer (roundNum + 1)
      