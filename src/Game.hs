module Game (
    startGame
  ) where

--import CodeBuilder
import Data.List (elemIndex)
import System.IO

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

-- TODO: make this more generic, we should be able to handle many types
-- TODO: handle the case where our guess is of a different length than our answer....
playRound :: String -> Int -> IO ()
playRound answer roundNum = do
  putStr "Please enter a guess $ "
  hFlush stdout
  guess <- getLine
  case checkGuess answer guess of
    AnswerResult 4 0 -> putStrLn "You win!"
    AnswerResult x y -> do
      putStrLn "Not quite..."
      putStr "Letters in correct positions: "
      print x
      putStr "Letters in incorrect positions: "
      print y

startGame :: IO ()
startGame = do
  --code <- makeCode
  playRound "ABCD" 0
  return ()

