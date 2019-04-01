module SolveTest (solveTest) where

import Test.Hspec
import Test.QuickCheck
import TrainGame

solveTest =
  describe "Testing solve..." $ do
    it "Should solve some simple test cases" $ do
      solve [Num 3, Num 2, Op Pl] == Just 5
      &&
      solve [Num 3, Num 2, Op Mi] == Just 1
      && 
      solve [Num 3, Num 2, Num 4, Op Mi, Op Mi] == Just 5

{-
    describe "Addition" $ do
      it "should add 2 numbers normally" $ do
        property $ operationTest Pl

    describe "Multiplication" $ do
      it "should multiply 2 numbers normally" $ do
        property $ operationTest Mu

    describe "Subtraction" $ do
      it "should subtract 2 numbers normally" $ do
        property $ operationTest Mi

    describe "Division" $ do
      it "should divide 2 numbers normall" $ do
        property $ operationTest Di

operationTest :: SolveType -> Integer -> Integer -> Bool
operationTest nodeType a b =
  case nodeType of
    Pl -> solve (Plus (Const a) (Const b))     == Just (a + b)
    Mi -> solve (Minus (Const a) (Const b))    == Just (a - b)
    Mu -> solve (Multiply (Const a) (Const b)) == Just (a * b)
    Di -> 
      case solve (Divide (Const a) (Const b)) of
          Just i  -> i == a `quot` b
          Nothing -> b == 0
          -}
