module GameMechanics where

import Data.List (delete)
import CodeBuilder (pegs)
import System.Exit (exitSuccess)

numRounds :: Int
numRounds = 10

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

endGame :: String -> IO ()
endGame msg = putStrLn msg >> exitSuccess

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


