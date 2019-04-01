module FindSolutionsTest (findSolutionsTest) where

import Test.Hspec
import Test.QuickCheck
import TrainGame

findSolutionsTest =
  describe "Testing findSolutions..." $ do
    it "should find 2*(3+2) == 10" $ do
     length (findSolutions [[Num 3, Num 2, Op Pl, Num 2, Op Mu]]) == 1
    it "should eliminate some simple non-answers" $ do
     length (findSolutions [
             [Num 2, Num 3, Op Pl], 
             [Num 10, Num 10, Op Mu],
             [Num 10, Num 2, Op Mi]
             ]) == 0