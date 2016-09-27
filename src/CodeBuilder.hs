module CodeBuilder (
    makeCode
  ) where

import System.Random (randomRIO)

possibilities :: [String]
possibilities = [[a,b,c,d] | a <- "abcdef", b <- "abcdef", c <- "abcdef", d <- "abcdef"]

makeCode :: IO String
makeCode = do
  randomIndex <- randomRIO (0, length possibilities - 1)
  return $ possibilities !! randomIndex
