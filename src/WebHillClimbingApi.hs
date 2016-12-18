{-# LANGUAGE DeriveGeneric #-}
module WebHillClimbingApi where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import CodeBuilder
import HillClimbingSolverGame ( genCode )
import Web.Scotty
import GameMechanics
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types.Method


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



mastermindCors = cors $ const (Just mastermindResourcePolicy)


mastermindResourcePolicy :: CorsResourcePolicy
mastermindResourcePolicy =
  CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = []
      , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
      , corsExposedHeaders = Nothing
      , corsMaxAge = Nothing
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }

startServer :: IO ()
startServer =
  do
    port <- read <$> getEnv "PORT"
    scotty port $ do
      middleware mastermindCors
      middleware . staticPolicy $ addBase "/app/client/dist"
      middleware logStdoutDev
      get (literal "/") $ file "/app/client/dist/index.html"
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
