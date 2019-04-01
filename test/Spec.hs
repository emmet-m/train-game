import Test.Hspec
import Test.QuickCheck
import TrainGame
import Control.Exception (evaluate)
import SolveTest
import FindSolutionsTest

main :: IO ()
main = hspec $ do
  solveTest
  findSolutionsTest
  -- TODO: Add more tests
