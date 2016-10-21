module GameMechanics where

import Data.List (delete)
import CodeBuilder
import System.Exit (exitSuccess)

numRounds :: Int
numRounds = 10

type ValidationMessage = String
type Guess = [Peg]
type Answer = [Peg]

-- AnswerResult
data AnswerResult = AnswerResult { 
  blackPegs :: Int,
  whitePegs :: Int
} deriving (Eq)

-- Current Favorite Guess
data CFG = CFG Guess AnswerResult deriving (Eq, Show)

instance Ord AnswerResult where
  compare result result' = compare (computeScore result) (computeScore result')

instance Show AnswerResult where
  show (AnswerResult x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Monoid AnswerResult where
  mempty = AnswerResult 0 0
  mappend (AnswerResult x y) (AnswerResult x' y') = AnswerResult (x + x') (y + y')

-- hill climbing heuristic gives the distance to the goal by penalizing
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


endGame :: String -> IO ()
endGame msg = putStrLn msg >> exitSuccess

incorrectPositionsCalc :: Eq a => [a] -> [a] -> Int
incorrectPositionsCalc _ [] = 0
incorrectPositionsCalc [] _ = 0
incorrectPositionsCalc answer (x':xs') =
  if x' `elem` answer
    then 1 + incorrectPositionsCalc (delete x' answer) xs'
    else 0 + incorrectPositionsCalc answer xs'

toPeg :: Char -> Peg
toPeg c = 
  case c of
    'B' -> Blue
    'G' -> Green
    'R' -> Red
    'Y' -> Yellow
    'O' -> Orange
    'P' -> Pink

-- todo: the above is non exhaustive of a pattern
-- we need to validate the guess upstream and move
-- the entire logic over to HumanSolverGame

toGuess :: String -> [Peg]
toGuess = map toPeg


validateGuess :: Answer -> String -> Either ValidationMessage Guess
validateGuess answer guess =
  let
    toPeg c = 
      case c of
        'B' -> Right Blue
        'G' -> Right Green
        'R' -> Right Red
        'Y' -> Right Yellow
        'O' -> Right Orange
        'P' -> Right Pink
        _  -> Left "Your guess can only include the following pegs: [BGRYOP]"
         --length answer /= length guess = Left $ "Your guess needs to be " ++ (show . length $ answer) ++ " characters long"
    toGuess = sequence . (map toPeg)
    correctLength guess' = if length answer /= length guess' then Left $ "Your guess needs to be " ++ (show . length $ answer) ++ " characters long" else Right guess'
    -- correctLength guess' = if length answer /= length guess' then Left $ "Your guess needs to be " ++ (show . length $ answer) ++ " characters long" else Right guess'
  in toGuess guess >>= correctLength


checkGuess :: Eq a => [a] -> [a] -> AnswerResult
checkGuess answer guess =
  let
    zippedGuesses = zip answer guess
    correctPositions = foldr (\(x, y) acc -> if x == y then acc + 1 else acc) 0 zippedGuesses
    leftOverPairs = filter (uncurry (/=)) zippedGuesses
    incorrectPositions = incorrectPositionsCalc (map fst leftOverPairs) (map snd leftOverPairs)
  in
    AnswerResult correctPositions incorrectPositions


