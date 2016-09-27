module CodeBuilder (
    makeCode
  , possibilities
  ) where

import System.Random (randomRIO)

possibilities :: [String]
possibilities = [[a,b,c,d] | a <- "ABCDEF", b <- "ABCDEF", c <- "ABCDEF", d <- "ABCDEF"]

makeCode :: IO String
makeCode = do
  randomIndex <- randomRIO (0, length possibilities - 1)
  return $ possibilities !! randomIndex
