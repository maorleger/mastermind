module GameMechanics where

import Data.List (delete)
import System.IO
import System.Exit (exitSuccess)
import Data.Char (toUpper)
import CodeBuilder (pegs)

numRounds :: Int
numRounds = 7

-- AnswerResult

type ValidationMessage = String
type Guess = String
type Answer = String

data AnswerResult = AnswerResult { 
  blackPegs :: Int,
  whitePegs :: Int
} deriving (Eq)

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


validateGuess :: Answer -> Guess -> Either ValidationMessage Guess
validateGuess answer guess
  | length answer /= length guess = Left $ "Your guess needs to be " ++ (show . length $ answer) ++ " characters long"
  | any (\x -> not $ elem x pegs) guess = Left $ "Your guess can only include the following letters: [" ++ pegs ++ "]"
  | otherwise = Right guess


checkGuess :: Eq a => [a] -> [a] -> AnswerResult
checkGuess answer guess =
  let
    zippedGuesses = zip answer guess
    correctPositions = foldr (\(x, y) acc -> if x == y then acc + 1 else acc) 0 zippedGuesses
    leftOverPairs = filter (uncurry (/=)) zippedGuesses
    incorrectPositions = incorrectPositionsCalc (map fst leftOverPairs) (map snd leftOverPairs)
  in
    AnswerResult correctPositions incorrectPositions


playRound :: Answer -> Int -> IO ()
playRound answer roundNum =
  let endGame msg = putStrLn msg >> exitSuccess
  in do
    putStr "Please enter a guess $ "
    hFlush stdout
    guess <- (fmap . fmap) toUpper getLine
    case checkGuess answer <$> validateGuess answer guess of
      Left msg -> do
        putStrLn msg
        playRound answer roundNum
      Right (AnswerResult 4 0) -> endGame "You win!"
      Right result -> putStrLn ("Not quite... " ++ show result) >>
        if roundNum >= numRounds
          then endGame ("Sorry, the correct code was: " ++ answer)
          else playRound answer (roundNum + 1)
