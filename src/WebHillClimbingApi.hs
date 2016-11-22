{-# LANGUAGE DeriveGeneric #-}
module WebHillClimbingApi where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import CodeBuilder
import HillClimbingSolverGame ( genCode )
import Web.Scotty
import GameMechanics
import Control.Monad.IO.Class (liftIO)

data ApiCFG = ApiCFG {guess :: Guess, score :: Maybe (Int, Int)} deriving (Eq, Show, Generic)

instance ToJSON ApiCFG
instance FromJSON ApiCFG

data Game = Game {rounds :: [ApiCFG]} deriving (Generic)

instance ToJSON Game
instance FromJSON Game

cfg'Convert :: CFG -> ApiCFG
cfg'Convert (CFG guess result) = ApiCFG {guess = guess, score = Just (blackPegs result, whitePegs result) }

cfgConvert :: ApiCFG -> CFG
cfgConvert (ApiCFG guess result) =
  case result of
    Nothing -> CFG guess (AnswerResult 0 0)
    Just (b, a) -> CFG guess $ AnswerResult b a

genInitialCFG :: IO ApiCFG
genInitialCFG = CodeBuilder.makeCode >>= (\code -> return $ ApiCFG code Nothing)


playRound :: Game -> IO Game
playRound (Game []) = do
  cfg <- genInitialCFG
  return $ Game [cfg]
playRound (Game game@(cfg:history)) = do
  let numRounds = length (cfg:history)
  guess <- genCode (cfgConvert cfg) (fmap cfgConvert (cfg : history)) 0
  return . Game $ ApiCFG guess Nothing : game


startServer :: IO ()
startServer = scotty 3000 $ do
  get (literal "/rounds") $
    json
      [ApiCFG [Green, Green, Green, Green] Nothing
      , ApiCFG [Blue, Red, Blue, Red] $ Just (0, 0)
      , ApiCFG [Yellow, Yellow, Pink, Pink] $ Just (2, 0)
      ]
  post (literal "/echo") $ do
    rounds <- jsonData :: ActionM Game
    json rounds
  post (literal "/play") $ do
    rounds <- jsonData :: ActionM Game
    game <- liftIO . playRound $ rounds
    json game