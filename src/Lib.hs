module Lib where

import Data.List (elemIndex)

data AnswerResult = AnswerResult {
  correctPositions :: Int,
  wrongPositions :: Int
} deriving (Eq, Show)

instance Monoid AnswerResult where
  mempty = AnswerResult 0 0
  mappend (AnswerResult x y) (AnswerResult x' y') = AnswerResult (x + x') (y + y')

checkGuess :: Eq a => [a] -> [a] -> AnswerResult
checkGuess answer guess =
  let 
    checkLetter word letter pos = 
      case elemIndex letter word of 
        Nothing -> AnswerResult 0 0
        Just indx -> if indx == pos then AnswerResult 1 0 else AnswerResult 0 1
    go [] _ = AnswerResult 0 0
    go (x:xs) pos = mappend (go xs (pos + 1)) (checkLetter answer x pos)
  in go guess 0

makeCode :: String
makeCode = "ADFB"

