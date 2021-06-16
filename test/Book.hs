module Book (bookSpec) where

import Ast (LispVal (..))
import Helpers (fails)
import Parser (expr)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.ParserCombinators.Parsec (parse)

bookSpec :: Spec
bookSpec = do
  describe "bookSpec" $ do
    it "should parse list 1" $ do
      parse expr "" "(a test)" `shouldBe` Right (List [Atom "a", Atom "test"])
    it "should parse list 2" $ do
      parse expr "" "(a (nested) test)" `shouldBe` Right (List [Atom "a", List [Atom "nested"], Atom "test"])
    it "should parse list 3" $ do
      parse expr "" "(a (dotted . list) test)" `shouldBe` Right (List [Atom "a", DottedList [Atom "dotted"] (Atom "list"), Atom "test"])
    it "should parse list 4" $ do
      parse expr "" "( a '   ( quoted ( dotted . list )) test)" `shouldBe` Right (List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"])
    it "should parse list 5" $ do
      parse expr "" "( a '( quoted ( dotted . list )) test)" `shouldBe` Right (List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"])
    it "should fails to parse imbalanced parens" $ do
      fails (parse expr "" " ( a' (imbalanced parens)") `shouldBe` True