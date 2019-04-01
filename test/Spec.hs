import Test.Hspec
import Test.QuickCheck
import TrainGame
import Control.Exception (evaluate)
import SolveTest

main :: IO ()
main = hspec $ do
  solveTest
  -- TODO: Add more tests
