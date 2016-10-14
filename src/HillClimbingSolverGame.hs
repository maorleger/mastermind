module HillClimbingSolverGame where

import GameMechanics
import qualified CodeBuilder
import System.Random
import System.Exit (exitSuccess)
import Data.Maybe (isJust)


startGame :: IO ()
startGame = 
  let
    genInitialCFG = CodeBuilder.makeCode >>= (\code -> return $ CFG code (AnswerResult 0 0))
  in 
    putStrLn "Hi, I'm Lily! Let's see if I can solve your puzzle" >>
    putStrLn "I'll tell you my guess, and you can tell me how I did." >>
    putStrLn "For example, you can say (3,1) for 3 black pegs and 1 white peg" >> 
    genInitialCFG >>= 
      (\initialCFG -> playRound initialCFG 1 [])


playRound :: CFG -> Int -> [CFG] -> IO ()
playRound cfg@(CFG _ cfResult) roundNum history =
  let
    chooseCFG score guess = case (uncurry AnswerResult score) > cfResult of
                          True -> CFG guess (uncurry AnswerResult score)
                          False -> cfg
  in do
    guess <- genCode cfg history 0
    putStrLn $ "Round: " ++ (show roundNum)
    putStrLn $ "My guess is: " ++ guess
    putStrLn "How did I do?"
    score <- readScore' guess
    checkForGameOver score roundNum
    playRound (chooseCFG score guess) (roundNum + 1) ((CFG guess (uncurry AnswerResult score)) : history)


readScore :: IO (Int, Int)
readScore = readLn


readScore' :: Guess -> IO (Int, Int)
readScore' guess = 
  let
    answer = "BEEF"
    score = checkGuess guess answer
  in
    return (blackPegs score, whitePegs score)


checkForGameOver :: (Int, Int) -> Int -> IO ()
checkForGameOver (4, 0) _ = endGame "I am the MACHINE!"
checkForGameOver _ roundNum | roundNum > numRounds = endGame "You lose"
                            | otherwise = return ()


genCode :: CFG  -> [CFG] -> Int -> IO Guess
genCode cfg@(CFG guess (AnswerResult blacks whites)) history attempts 
  | attempts > 20000 = putStrLn "Cannot deduce the next guess, are you sure you scored my guesses correctly? " >> exitSuccess 
  | otherwise = do
      pegsToKeep <- randomPegsToKeep [] blacks
      pegsToShift <- randomPegsToShift pegsToKeep [] whites
      code <- createCode guess pegsToKeep pegsToShift
      if inconsistent code history
        then genCode cfg history (attempts + 1)
        else return code


randomPegsToKeep :: [Int] -> Int -> IO [Int]
randomPegsToKeep positionsKept 0 = return positionsKept
randomPegsToKeep positionsKept pegsLeft =
  randomRIO (0, 3) >>=
    (\randomNumber ->
      case randomNumber `elem` positionsKept of
        True -> randomPegsToKeep positionsKept pegsLeft
        False -> randomPegsToKeep (randomNumber : positionsKept) (pegsLeft - 1))


randomPegsToShift :: [Int] -> [Int] -> Int -> IO [Int]
randomPegsToShift _ positionsKept 0 = return positionsKept
randomPegsToShift pegsToKeep positionsKept pegsLeft =
  randomRIO (0, 3) >>=
  (\randomNumber -> 
    case randomNumber `elem` (positionsKept ++ pegsToKeep) of
      True -> randomPegsToShift pegsToKeep positionsKept pegsLeft
      False -> randomPegsToShift pegsToKeep (randomNumber : positionsKept) (pegsLeft - 1))


createCode :: Guess -> [Int] -> [Int] -> IO Guess
createCode guess pegsToKeep pegsToShift =
  let
    codeFromBlacks = createBlackCode guess (replicate (length guess) Nothing) pegsToKeep
    codeFromWhites = createWhiteCode guess codeFromBlacks pegsToShift
  in 
    codeFromWhites >>= (\newGuess -> createNewLetters newGuess [])

     
createBlackCode :: Guess -> [Maybe Char] -> [Int] -> [Maybe Char]
createBlackCode _ newCode [] = newCode
createBlackCode oldCode newCode (black:blacks) =
  let
    constructedCode = replaceAtIndex black (Just (oldCode !! black)) newCode
  in 
    createBlackCode oldCode constructedCode blacks


createWhiteCode :: Guess -> [Maybe Char] -> [Int] -> IO [Maybe Char]
createWhiteCode _ newCode [] = return newCode
createWhiteCode oldCode newCode (white:whites) = 
  let
    constructCode index = replaceAtIndex index (Just (oldCode !! white)) newCode
  in
    findIndexToShift oldCode newCode white 0 >>=
      (\index -> 
        createWhiteCode oldCode (constructCode index) whites)


findIndexToShift :: Guess -> [Maybe Char] -> Int -> Int -> IO Int
findIndexToShift oldCode newCode posToShiftFrom attempts
  | attempts == 20 = 
      putStrLn "more than 20 attempts have been made to shift a position, Keeping it in place" >> 
      return posToShiftFrom
  | otherwise = 
      let
        validPosition newPos oldPos constructedCode = 
            -- invalid IFF:
            -- we got a random number that is the same as the position our peg is alrady in
            -- or we got a new random number but that position is already taken
            not (newPos == oldPos || isJust (constructedCode !! newPos))
      in randomRIO (0, length oldCode - 1) >>=
        (\proposedNewPos -> 
          if (validPosition proposedNewPos posToShiftFrom newCode) 
          then return proposedNewPos 
          -- if at first you dont succeed...
          else findIndexToShift oldCode newCode posToShiftFrom (attempts + 1)
        )


replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex idx newElem ls = a ++ (newElem:b) 
  where (a, (_:b)) = splitAt idx ls

createNewLetters :: [Maybe Char] -> [Char] -> IO [Char]
createNewLetters [] newCode = return . reverse $ newCode
createNewLetters (x:xs) newCode = 
  let range = (minimum CodeBuilder.pegs, maximum CodeBuilder.pegs)
  in case x of
    Nothing -> do
      r <- randomRIO range
      createNewLetters xs (r : newCode)
    Just c -> createNewLetters xs (c : newCode)


inconsistent :: Guess -> [CFG] -> Bool
inconsistent _ [] = False
inconsistent newGuess ((CFG guess result):cfgs)
  | newGuess == guess = True
  | (checkGuess newGuess guess) /= result = True
  | otherwise = False || inconsistent newGuess cfgs

