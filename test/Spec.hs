module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import GameMechanics
import Data.List (nub)

instance Arbitrary AnswerResult where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ AnswerResult a b

instance EqProp AnswerResult where (=-=) = eq

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
  (\x -> checkGuess x x === Right (AnswerResult 4 0))

prop_guessesIncorrectPos :: Property
prop_guessesIncorrectPos =
  forAll genUnique
  (\x -> checkGuess x (reverse x) === Right (AnswerResult 0 4))

prop_scoreSumsCorrectly :: Property
prop_scoreSumsCorrectly =
  forAll genTuple
  (\(answer, guess) ->
    case checkGuess answer guess of
      Left _ -> property False
      Right (AnswerResult blackPegs whitePegs) -> property $ blackPegs + whitePegs <= 4
  )

prop_scoreSumsCorrectly' :: Property
prop_scoreSumsCorrectly' =
  forAll genGuess
  (\x ->
    case checkGuess x (reverse x) of
      Left _ -> property False
      Right (AnswerResult blackPegs whitePegs) -> property $ blackPegs + whitePegs === 4
  )

main :: IO ()
main = do
  quickBatch (monoid AnswerResult)
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
        checkGuess "ABCD" "BCDA" `shouldBe` Right (AnswerResult 0 4)
      it "Correctly handles repeated characters" $ do
        checkGuess "AAAA" "AAAA" `shouldBe` Right (AnswerResult 4 0)
        checkGuess "AABB" "ABAA" `shouldBe` Right (AnswerResult 1 2)
        checkGuess "ABCF" "CBCA" `shouldBe` Right (AnswerResult 2 1)
      it "Correctly handles mismatched guess length" $ do
        checkGuess "A" "AAAA" `shouldBe` Left "Your guess needs to be 1 characters long"
        checkGuess "A" "AAAAA" `shouldBe` Left "Your guess needs to be 1 characters long"
        checkGuess "AAAA" "A" `shouldBe` Left "Your guess needs to be 4 characters long"
        checkGuess "AAAA" "AAAAA" `shouldBe` Left "Your guess needs to be 4 characters long"