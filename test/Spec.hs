module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid
import GameMechanics
import Data.List (nub)

instance Arbitrary AnswerResult where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ AnswerResult a b

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
monoidAssoc a b c = (a `mappend` (b `mappend` c)) == ((a `mappend` b) `mappend` c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a `mappend` mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty `mappend` a) == a

genGuess :: Gen String
genGuess = arbitrary `suchThat` (\x -> length x == 4)

genTuple :: Gen (String, String)
genTuple = do
  a <- arbitrary `suchThat` (\x -> length x == 4)
  b <- arbitrary `suchThat` (\x -> length x == 4)
  return (a, b)

genUnique :: Gen String
genUnique = arbitrary `suchThat` (\x -> (length x == 4) && nub x == x)

prop_guessesCorrectly :: Property
prop_guessesCorrectly =
  forAll genGuess
  (\x -> checkGuess x x === AnswerResult 4 0)

prop_guessesIncorrectPos :: Property
prop_guessesIncorrectPos =
  forAll genUnique
  (\x -> checkGuess x (reverse x) === AnswerResult 0 4)

prop_scoreSumsCorrectly :: Property
prop_scoreSumsCorrectly =
  forAll genTuple
  (\(answer, guess) -> (blackPegs (checkGuess answer guess)) + (whitePegs (checkGuess answer guess)) <= 4)

prop_scoreSumsCorrectly' :: Property
prop_scoreSumsCorrectly' =
  forAll genGuess
  (\x -> (blackPegs (checkGuess x (reverse x))) + (whitePegs (checkGuess x (reverse x))) === 4)

main :: IO ()
main = do
  quickCheck (monoidAssoc :: AnswerResult -> AnswerResult -> AnswerResult -> Bool)
  quickCheck (monoidRightIdentity :: AnswerResult -> Bool)
  quickCheck (monoidLeftIdentity :: AnswerResult -> Bool)
  quickCheck prop_guessesCorrectly
  quickCheck prop_guessesIncorrectPos
  quickCheck prop_scoreSumsCorrectly
  quickCheck prop_scoreSumsCorrectly'

  hspec $ do
    describe "GameMechanics" $ do
      it "Shows AnswerResult correctly" $ do
        show (AnswerResult 4 0) `shouldBe` "[4,0]"
        show (AnswerResult 0 4) `shouldBe` "[0,4]"
      it "Correctly calculates incorrect positions" $ do
        checkGuess "ABCD" "BCDA" `shouldBe` AnswerResult 0 4
      it "Correctly handles repeated characters" $ do
        checkGuess "AAAA" "AAAA" `shouldBe` AnswerResult 4 0
        checkGuess "AABB" "ABAA" `shouldBe` AnswerResult 1 2
        checkGuess "ABCF" "CBCA" `shouldBe` AnswerResult 2 1