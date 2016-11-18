{-# LANGUAGE DeriveGeneric #-}
module WebHillClimbingApi where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import GameMechanics
import CodeBuilder
import HillClimbingSolverGame ( genCode )

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


-- TODO: playRound needs to be replicated here
-- 1. Figure out how to end the game using the length of history
-- 2. figure out how to choose a CFG
-- 3. Generate a new guess
playRound :: Game -> IO Game
playRound (Game []) = do
  cfg <- genInitialCFG
  return $ Game [cfg]
playRound (Game game@(cfg:history)) = do
  let numRounds = length (cfg:history)
  -- assume that a (4,0) score wouldnt even make it here
  -- because the elm frontend would declare win
  guess <- genCode (cfgConvert cfg) (fmap cfgConvert (cfg : history)) 0
  return . Game $ ApiCFG guess Nothing : game


  -- playRound :: CFG -> Int -> [CFG] -> IO ()
  -- playRound cfg@(CFG _ cfResult) roundNum history =
  --   let
  --     chooseCFG score guess =
  --       if uncurry AnswerResult score > cfResult
  --         then CFG guess (uncurry AnswerResult score)
  --         else cfg
  --   in do
  --     guess <- genCode cfg history 0
  --     putStrLn $ "Round: " <> show roundNum
  --     putStrLn $ "My guess is: " <> show guess
  --     putStrLn "How did I do?"
  --     score <- readScore--' guess
  --     checkForGameOver score roundNum
  --     playRound (chooseCFG score guess) (roundNum + 1) (CFG guess (uncurry AnswerResult score) : history)
