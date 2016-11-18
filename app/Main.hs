{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified HumanSolverGame as Human
import qualified HillClimbingSolverGame as HillClimbing
import qualified WebHillClimbingApi as WebApi
import CodeBuilder
--import System.Environment (getArgs)
--import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty
import GameMechanics
import Control.Monad.IO.Class (liftIO)

-- The old way of playing the game involved using the console.
-- for now, I'll leave it here, but eventually I plan to have the
-- entire thing a web API only
playGame :: [String] -> IO ()
playGame [] = Human.startGame
playGame (arg1:_) =
  if arg1 == "hill_climbing"
    then HillClimbing.startGame
    else Human.startGame


main :: IO ()
main =
  scotty 3000 $ do
    get "/rounds" $
      json
        [WebApi.ApiCFG [Green, Green, Green, Green] Nothing
        , WebApi.ApiCFG [Blue, Red, Blue, Red] $ Just (0, 0)
        , WebApi.ApiCFG [Yellow, Yellow, Pink, Pink] $ Just (2, 0)
        ]
    post "/echo" $ do
      rounds <- jsonData :: ActionM WebApi.Game
      json rounds
    post "/play" $ do
      rounds <- jsonData :: ActionM WebApi.Game
      game <- liftIO . WebApi.playRound $ rounds
      json game
