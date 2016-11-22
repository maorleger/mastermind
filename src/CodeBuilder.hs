{-# LANGUAGE DeriveGeneric #-}

module CodeBuilder where

import System.Random
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Peg =
    Blue
  | Green
  | Red
  | Yellow
  | Orange
  | Pink
  deriving (Eq, Show, Ord, Enum, Generic)


instance ToJSON Peg
instance FromJSON Peg

instance Random Peg where
    random g = case randomR (0,5) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

pegs :: [Peg]
pegs = [Blue, Green, Red, Yellow, Orange, Pink]

possibilities :: [[Peg]]
possibilities = [[a,b,c,d] | a <- pegs, b <- pegs, c <- pegs, d <- pegs]


makeCode :: IO [Peg]
makeCode = do
  randomIndex <- randomRIO (0, length possibilities - 1)
  return $ possibilities !! randomIndex
