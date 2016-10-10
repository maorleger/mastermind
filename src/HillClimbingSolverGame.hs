module HillClimbingSolverGame where

import GameMechanics
import Data.Char (toUpper)
import qualified CodeBuilder
import System.IO
import System.Random
import Control.Applicative
import System.Exit (exitSuccess)

readScore :: IO (Int, Int)
readScore = readLn

readScore' :: Guess -> IO (Int, Int)
readScore' guess = 
  let
    answer = "BAAE"
    score = checkGuess guess answer
  in
    return (blackPegs score, whitePegs score)


-- Current Favorite Guess
data CFG = CFG Guess AnswerResult deriving (Eq, Show)


-- heuristic gives the distance to the goal by penalizing
-- the pegs with the right colours but in wrong position.
computeScore :: AnswerResult -> Int
computeScore (AnswerResult black white) = 
  case (black, white) of
    (0, 0) -> 0
    (0, 1) -> 1
    (1, 0) -> 2
    (0, 2) -> 3
    (1, 1) -> 4
    (2, 0) -> 5
    (0, 3) -> 6
    (1, 2) -> 7
    (2, 1) -> 8
    (3, 0) -> 9
    (0, 4) -> 10
    (1, 3) -> 11
    (2, 2) -> 12
    (4, 0) -> 13


-- TODO: I HAVE TO REMOVE THE ORPHANED INSTANCE HERE!!!!
instance Ord AnswerResult where
  compare result result' = compare (computeScore result) (computeScore result')


randomPegsToKeep :: [Int] -> Int -> IO [Int]
randomPegsToKeep positionsKept 0 = return positionsKept
randomPegsToKeep positionsKept pegsLeft = do
  randomNumber <- randomRIO (0, 3)
  case randomNumber `elem` positionsKept of
    True -> randomPegsToKeep positionsKept pegsLeft
    False -> randomPegsToKeep (randomNumber : positionsKept) (pegsLeft - 1)


randomPegsToShift :: [Int] -> [Int] -> Int -> IO [Int]
randomPegsToShift pegsToKeep positionsKept 0 = return positionsKept
randomPegsToShift pegsToKeep positionsKept pegsLeft = do
  randomNumber <- randomRIO (0, 3)
  case randomNumber `elem` (positionsKept ++ pegsToKeep) of
    True -> randomPegsToShift pegsToKeep positionsKept pegsLeft
    False -> randomPegsToShift pegsToKeep (randomNumber : positionsKept) (pegsLeft - 1)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex idx newElem ls = a ++ (newElem:b) 
  where (a, (_:b)) = splitAt idx ls

findIndexToShift :: Guess -> [Maybe Char] -> Int -> Int -> IO Int
findIndexToShift oldCode newCode posToShiftFrom attempts
  | attempts == 1000000 = putStrLn "more than 1000000 attempts have been made to shift a position" >> exitSuccess
  | otherwise = 
      let validPosition newPos oldPos newCode = 
            -- we got a random number that is the same as the position our peg is alrady in
            if newPos == oldPos
              then False
            else
              case newCode !! newPos of
                -- we got a random number but that position is already taken
                Just _ -> False
                Nothing -> True
      in randomRIO (0, length oldCode - 1) >>=
        (\proposedNewPos -> 
          if (validPosition proposedNewPos posToShiftFrom newCode) 
          then return proposedNewPos 
          -- if at first you dont succeed...
          else findIndexToShift oldCode newCode posToShiftFrom (attempts + 1)
        )
        
createBlackCode :: Guess -> [Maybe Char] -> [Int] -> [Maybe Char]
createBlackCode oldCode newCode [] = newCode
createBlackCode oldCode newCode (black:blacks) =
  createBlackCode oldCode (replaceAtIndex black (Just (oldCode !! black)) newCode) blacks

createWhiteCode :: Guess -> [Maybe Char] -> [Int] -> IO [Maybe Char]
createWhiteCode oldCode newCode [] = return newCode
createWhiteCode oldCode newCode (white:whites) = 
  let 
    charToShift = oldCode !! white
    indexToShift = findIndexToShift oldCode newCode white 0
  in do 
    idx <- indexToShift
    createWhiteCode oldCode  (replaceAtIndex idx (Just (oldCode !! white)) newCode) whites
    

createNewLetters :: [Maybe Char] -> [Char] -> IO [Char]
createNewLetters [] newCode = return . reverse $ newCode
createNewLetters (x:xs) newCode = 
  case x of
    Nothing -> do
      r <- randomRIO ('A', 'F')
      createNewLetters xs (r : newCode)
    Just c -> createNewLetters xs (c : newCode)


createCode :: Guess -> [Int] -> [Int] -> IO Guess
createCode guess pegsToKeep pegsToShift =
  let
    codeFromBlacks = createBlackCode guess (replicate (length guess) Nothing) pegsToKeep
    codeFromWhites = createWhiteCode guess codeFromBlacks pegsToShift
  in 
    codeFromWhites >>= (\newGuess -> createNewLetters newGuess [])


inconsistent :: Guess -> [CFG] -> Bool
inconsistent _ [] = False
inconsistent newGuess ((CFG guess result):cfgs)
  | newGuess == guess = True
  | (checkGuess newGuess guess) /= result = True
  | otherwise = False || inconsistent newGuess cfgs

genCode :: String -> CFG  -> [CFG] -> IO Guess
genCode pegs cfg@(CFG guess (AnswerResult blackPegs whitePegs)) history = 
  do
    pegsToKeep <- randomPegsToKeep [] blackPegs
    pegsToShift <- randomPegsToShift pegsToKeep [] whitePegs
    code <- createCode guess pegsToKeep pegsToShift
    putStrLn $ "old guess: " ++ guess
    putStrLn $ "new guess: " ++ code
    putStrLn "history below:"
    print history
    if inconsistent code history
      then putStrLn ("code of " ++ code ++ " is inconsistent as guess of " ++ guess) >>
        genCode pegs cfg history
      else return code

genInitialCFG :: IO CFG
genInitialCFG = do
  code <- CodeBuilder.makeCode 
  return $ CFG code (AnswerResult 0 0)


checkForGameOver :: (Int, Int) -> Int -> IO ()
checkForGameOver (4, 0) _ = endGame "I am the MACHINE!"
checkForGameOver _ roundNum | roundNum > numRounds = endGame "You lose"
                            | otherwise = return ()


playRound :: CFG -> String -> Int -> [CFG] -> IO ()
playRound cfg@(CFG cfGuess cfResult) pegs roundNum history =
  let 
    chooseCFG score guess = case (uncurry AnswerResult score) > cfResult of
                          True -> CFG guess (uncurry AnswerResult score)
                          False -> cfg
    choosePegs guess (0,0) oldPegs = filter (not . flip elem guess) oldPegs
    choosePegs _ _ oldPegs = oldPegs
  in do
    letters <- genRandomLetters
    positions <- genRandomPositions
    guess <- genCode pegs cfg history
    putStrLn $ "The pegs are: " ++ pegs
    putStrLn $ "My guess is: " ++ guess
    putStrLn "How did I do?"
    score <- readScore' guess
    checkForGameOver score roundNum
    playRound (chooseCFG score guess) (choosePegs guess score pegs) (roundNum + 1) ((CFG guess (uncurry AnswerResult score)) : history)
  
genRandomLetters :: IO [Char]
genRandomLetters = getStdGen >>= (\gen -> return $ randomRs ('A', 'F') gen)

genRandomPositions :: IO [Int]
genRandomPositions = getStdGen >>= (\gen -> return $ randomRs (0, 3) gen)

startGame :: IO ()
startGame = do
  putStrLn "Hi, I'm Lily! Let's see if I can solve your puzzle"
  cfg <- genInitialCFG
  playRound cfg CodeBuilder.pegs 1 []
  return ()