module Game (startGame) where

import CodeBuilder
import GameMechanics  
      
startGame :: IO ()
startGame = do
  code <- makeCode
  playRound code 1
  return ()
