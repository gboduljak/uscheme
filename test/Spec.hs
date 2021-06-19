import Book (bookSpec)
import ScopingSpec (scopingSpec)
import Test.Hspec
import Toys (toysSpec)

main :: IO ()
main = do
  hspec scopingSpec
  hspec toysSpec
  hspec bookSpec
