import Book (bookSpec)
import Test.Hspec
import Toys (toysSpec)

main :: IO ()
main = do
  hspec toysSpec
  hspec bookSpec
