module Book (bookSpec) where

import Ast (LispVal (..))
import Evaluator (LispError (..), performEvalEmpty)
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
      performEvalEmpty (getVal $ Parsec.parse expr "" "(+ 2 2)") `shouldBe` (Right (Number 4))
    it "should eval primitive 2" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(+ 2 (- 4 1))") `shouldBe` (Right (Number 5))
    it "should eval primitive 3" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(- (+ 4 6 3) 3 5 2)") `shouldBe` (Right (Number 3))
    it "should fail to eval primitive 4" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(+ 2 \"two\")") `shouldBe` (Left (TypeMismatch "number" (String "two")))
    it "should eval primitive 5" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(< 2 3)") `shouldBe` (Right (Bool True))
    it "should eval primitive 6" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(> 2 3)") `shouldBe` (Right (Bool False))
    it "should eval primitive 7" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(>= 3 3)") `shouldBe` (Right (Bool True))
    it "should eval primitive 8" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(string=? \"test\" \"test\")") `shouldBe` (Right (Bool True))
    it "should eval primitive 9" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(string<? \"abc\" \"bba\")") `shouldBe` (Right (Bool True))
    it "should eval primitive 10" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(if (> 2 3) \"no\" \"yes\")") `shouldBe` (Right (String "yes"))
    it "should eval primitive 11" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal \")") `shouldBe` (Right (Number 9))
    it "should eval list primitive 1" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(car '(2 3 4))") `shouldBe` (Right (Number 2))
    it "should eval list primitive 2" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(cdr '(2 3 4))") `shouldBe` (Right (List [Number 3, Number 4]))
    it "should eval list primitive 3" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(car (cdr (cons 2 '(3 4))))") `shouldBe` (Right (Number 3))
    it "should eval list primitive 4" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(cdr '(a simple test))") `shouldBe` (Right (List [Atom "simple", Atom "test"]))
    it "should eval list primitive 5" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(car (cdr '(a simple test)))") `shouldBe` (Right (Atom "simple"))
    it "should eval list primitive 6" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(car '((this is) a test))") `shouldBe` (Right (List [Atom "this", Atom "is"]))
    it "should eval list primitive 7" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(cons '(this is) 'test)") `shouldBe` Right (DottedList [List [Atom "this", Atom "is"]] (Atom "test"))
    it "should eval list primitive 8" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(cons '(this is) '())") `shouldBe` Right (List [List [Atom "this", Atom "is"]])
    it "should eval eq primitive 1" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(eqv? 1 3)") `shouldBe` Right (Bool False)
    it "should eval eq primitive 2" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(eqv? 3 3)") `shouldBe` Right (Bool True)
    it "should eval eq primitive 3" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(eqv? 'atom 'atom)") `shouldBe` Right (Bool True)
    it "should eval cond primitive 1" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(cond ((> 3 2) 'greater) ((< 3 2) 'less))") `shouldBe` Right (Atom "greater")
    it "should eval case primitive 1" $ do
      performEvalEmpty (getVal $ Parsec.parse expr "" "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))") `shouldBe` Right (Atom "composite")
  where
    getVal (Left _) = error ""
    getVal (Right val) = val