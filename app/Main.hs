{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HumanSolverGame as Human
import qualified HillClimbingSolverGame as HillClimbing
import qualified WebHillClimbingApi as WebApi
import System.Environment (getArgs)


playGame :: [String] -> IO ()
playGame [] = Human.startGame
playGame (arg1:_) =
  case arg1 of
    "hill_climbing" -> HillClimbing.startGame
    "server_mode" -> WebApi.startServer
    _ -> Human.startGame



main :: IO ()
main = do
    args <- getArgs
    playGame args
