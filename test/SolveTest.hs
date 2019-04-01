module SolveTest (solveTest) where

import Test.Hspec
import Test.QuickCheck
import TrainGame

solveTest =
  describe "Testing solve..." $ do
    it "3 + 2 == 5" $ do
      solve [Num 3, Num 2, Op Pl] == Just 5
    it "3 - 2 == 1" $ do
      solve [Num 3, Num 2, Op Mi] == Just 1
    it "3 - (2 - 4) == 5" $ do
      solve [Num 3, Num 2, Num 4, Op Mi, Op Mi] == Just 5
    it "10 * (2 + 3) == 50" $ do
      solve [Num 2, Num 3, Op Pl, Num 10, Op Mu] == Just 50
    it "3 + (3 / 1) == 6" $ do
      solve [Num 3, Num 1, Op Di, Num 3, Op Pl] == Just 6
    it "1 * (1 / 1) == 1" $ do
      solve [Num 1, Num 1, Op Di, Num 1, Op Mu] == Just 1
    it "1 / 0 == Undefined" $ do
      solve [Num 1, Num 0, Op Di] == Nothing