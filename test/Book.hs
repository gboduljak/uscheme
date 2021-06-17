module Book (bookSpec) where

import Ast (LispVal (..))
import Evaluator (eval)
import Helpers (fails)
import Parser (expr)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)

bookSpec :: Spec
bookSpec = do
  describe "bookSpec" $ do
    it "should parse list 1" $ do
      Parsec.parse expr "" "(a test)" `shouldBe` Right (List [Atom "a", Atom "test"])
    it "should parse list 2" $ do
      Parsec.parse expr "" "(a (nested) test)" `shouldBe` Right (List [Atom "a", List [Atom "nested"], Atom "test"])
    it "should parse list 3" $ do
      Parsec.parse expr "" "(a (dotted . list) test)" `shouldBe` Right (List [Atom "a", DottedList [Atom "dotted"] (Atom "list"), Atom "test"])
    it "should parse list 4" $ do
      Parsec.parse expr "" "( a '   ( quoted ( dotted . list )) test)" `shouldBe` Right (List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"])
    it "should parse list 5" $ do
      Parsec.parse expr "" "( a '( quoted ( dotted . list )) test)" `shouldBe` Right (List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"])
    it "should fails to parse imbalanced parens" $ do
      fails (Parsec.parse expr "" " ( a' (imbalanced parens)") `shouldBe` True
  describe "eval primitives" $ do
    it "should eval primitive 1" $ do
      Evaluator.eval (getVal $ Parsec.parse expr "" "(+ 2 2)") `shouldBe` (Number 4)
    it "should eval primitive 2" $ do
      Evaluator.eval (getVal $ Parsec.parse expr "" "(+ 2 (- 4 1))") `shouldBe` (Number 5)
    it "should eval primitive 3" $ do
      Evaluator.eval (getVal $ Parsec.parse expr "" "(- (+ 4 6 3) 3 5 2)") `shouldBe` (Number 3)
  where
    getVal (Left _) = error ""
    getVal (Right val) = val