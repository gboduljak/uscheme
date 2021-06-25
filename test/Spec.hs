import ArithmeticSpec (arithmeticSpec)
import ArithmeticWithIterationSpec (arithmeticWithIterSpec)
import BookSpec (bookEvaluationSpec, bookParsingSpec)
import ListsOpsSpec (listsOpsSpec)
import MixedSpec (mixedSpec)
import PeterNorvigSpec (peterNorvigSpec)
import RecursionSpec (recursionSpec)
import ScopingSpec (scopingSpec)
import Test.Hspec (hspec)
import Toys (toysSpec)

main :: IO ()
main = do
  hspec bookParsingSpec
  hspec bookEvaluationSpec
  hspec toysSpec
  hspec scopingSpec
  hspec arithmeticSpec
  hspec arithmeticWithIterSpec
  hspec mixedSpec
  hspec listsOpsSpec
  hspec peterNorvigSpec
  hspec recursionSpec