module CodeBuilder (
    makeCode
  , possibilities
  , pegs
  ) where

import System.Random (randomRIO)


pegs :: String
pegs = "ABCDEF"


possibilities :: [String]
possibilities = [[a,b,c,d] | a <- pegs, b <- pegs, c <- pegs, d <- pegs]


makeCode :: IO String
makeCode = do
  randomIndex <- randomRIO (0, length possibilities - 1)
  return $ possibilities !! randomIndex
