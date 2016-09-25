module Lib where

import Data.List (elemIndex)

data AnswerResult = AnswerResult {
  correctPositions :: Int,
  wrongPositions :: Int
} deriving (Eq, Show)

instance Monoid AnswerResult where
  mempty = AnswerResult 0 0
  mappend (AnswerResult x y) (AnswerResult x' y') = AnswerResult (x + x') (y + y')

-- for each letter in my guess:
-- is the letter position in the same position of the answer? If so, correctPositions + 1
-- is the letter position in a different position as the answer? If so, wrongPositions + 1
-- otherwise keep going
-- checkGuess :: String -> String -> AnswerResult
-- checkGuess [] _ = AnswerResult 0 0
-- checkGuess _ [] = AnswerResult 0 0
-- checkGuess answer guess = go answer guess (AnswerResult 0 0)
--   where go (x:xs) (x':xs') (AnswerResult correct wrong)
--           | x == x' = mappend (AnswerResult (correct + 1) wrong) (go xs xs')
--           | elem x' 

checkGuess :: String -> String -> AnswerResult
checkGuess answer guess = 
  go guess 0
  where 
    go [] _ = AnswerResult 0 0
    go (x:xs) pos = mappend (go xs (pos + 1)) (checkLetter answer x pos)

checkLetter :: String -> Char -> Int -> AnswerResult
checkLetter answer guess pos = 
  case elemIndex guess answer of 
    Nothing -> AnswerResult 0 0
    Just indx -> if indx == pos then AnswerResult 1 0 else AnswerResult 0 1


makeCode :: String
makeCode = "ADFB"

