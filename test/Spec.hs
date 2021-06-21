import BookSpec (bookEvaluationSpec, bookParsingSpec)
import RecursionSpec (recursionSpec)
import ScopingSpec (scopingSpec)
import Test.Hspec (hspec)
import Toys (toysSpec)

main :: IO ()
main = do
  hspec scopingSpec
  hspec toysSpec
  hspec bookParsingSpec
  hspec bookEvaluationSpec
  hspec recursionSpec