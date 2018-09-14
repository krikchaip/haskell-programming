module Main where

import Hangman
import Test.Hspec

getDs :: Puzzle -> Discovered
getDs (Puzzle _ ds _) = ds

getGs :: Puzzle -> Guessed
getGs (Puzzle _ _ gs) = gs

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    let puzzle = freshPuzzle "winner"
    context "right character" $ do
      it "discovered char should be Just" $ do
        getDs
          (fillInCharacter puzzle 'w')
          `shouldBe`
          [Just 'w', Nothing, Nothing, Nothing, Nothing, Nothing]
        getDs
          (fillInCharacter puzzle 'n')
          `shouldBe`
          [Nothing, Nothing, Just 'n', Just 'n', Nothing, Nothing]
      it "guessed should remain the same" $
        getGs (fillInCharacter puzzle 'w') `shouldBe` []
    context "wrong character" $
      it "guessed should contain that char" $
        getGs (fillInCharacter puzzle 'z') `shouldContain` ['z']

  describe "handleGuess" $
    it "..." $
      pendingWith "requires IO mock to test this"
