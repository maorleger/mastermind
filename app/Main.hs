module Main where

import qualified HumanSolverGame as Human
import qualified HillClimbingSolverGame as HillClimbing
import System.Environment (getArgs)

playGame :: [String] -> IO ()
playGame [] = Human.startGame
playGame (arg1:_) =
  if arg1 == "hill_climbing"
    then HillClimbing.startGame
    else Human.startGame


main :: IO ()
main = getArgs >>= playGame