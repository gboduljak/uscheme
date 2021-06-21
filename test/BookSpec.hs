{-# LANGUAGE BlockArguments #-}

module BookSpec (bookParsingSpec, bookEvaluationSpec) where

import Ast (LispVal (..))
import Data.Either
import Evaluator (evaluateOnEmptyContext)
import Helpers (fails)
import LispError (LispError (TypeMismatch))
import Parser (parse)
import Scoping.ScopeResolver (getInitialScopeContext)
import Test.Hspec (Spec, describe, it, shouldBe)

bookParsingSpec :: Spec
bookParsingSpec =
  describe "parsing tests from Write Yourself a Scheme in 48 Hours..." $
    do
      it "should parse list 1" $
        parse "(a test)" `shouldBe` Right [List [Atom "a", Atom "test"]]
      it "should parse list 2" $
        parse "(a (nested) test)" `shouldBe` Right [List [Atom "a", List [Atom "nested"], Atom "test"]]
      it "should parse list 3" $
        parse "(a (dotted . list) test)" `shouldBe` Right [List [Atom "a", DottedList [Atom "dotted"] (Atom "list"), Atom "test"]]
      it "should parse list 4" $
        parse "( a '   ( quoted ( dotted . list )) test)" `shouldBe` Right [List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"]]
      it "should parse list 5" $
        parse "( a '( quoted ( dotted . list )) test)" `shouldBe` Right [List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"]]
      it "should fails to (head . parse) imbalanced parens" $
        fails (parse " ( a' (imbalanced parens)") `shouldBe` True

bookEvaluationSpec :: Spec
bookEvaluationSpec =
  describe
    "parsing and evaluation tests from Write Yourself a Scheme in 48 Hours..."
    $ do
      it "should eval primitive 1" $
        evaluate "(+ 2 (- 4 1))" `shouldBe` Right (Number 5)
      it "should eval primitive 2" $
        evaluate "(+ 2 (- 4 1))" `shouldBe` Right (Number 5)
      it "should eval primitive 3" $
        evaluate "(- (+ 4 6 3) 3 5 2)" `shouldBe` Right (Number 3)
      it "should fail to eval primitive 4" $
        evaluate "(+ 2 \"two\")" `shouldBe` Left (TypeMismatch "number" (String "two"))
      it "should eval primitive 5" $
        evaluate "(< 2 3)" `shouldBe` Right (Bool True)
      it "should eval primitive 6" $
        evaluate "(> 2 3)" `shouldBe` Right (Bool False)
      it "should eval primitive 7" $
        evaluate "(>= 3 3)" `shouldBe` Right (Bool True)
      it "should eval primitive 8" $
        evaluate "(string=? \"test\" \"test\")" `shouldBe` Right (Bool True)
      it "should eval primitive 9" $
        evaluate "(string<? \"abc\" \"bba\")" `shouldBe` Right (Bool True)
      it "should eval primitive 10" $
        evaluate "(if (> 2 3) \"no\" \"yes\")" `shouldBe` Right (String "yes")
      it "should eval primitive 11" $
        evaluate "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal \")" `shouldBe` Right (Number 9)
      it "should eval list primitive 1" $
        evaluate "(car '(2 3 4))" `shouldBe` Right (Number 2)
      it "should eval list primitive 2" $
        evaluate "(cdr '(2 3 4))" `shouldBe` Right (List [Number 3, Number 4])
      it "should eval list primitive 3" $
        evaluate "(car (cdr (cons 2 '(3 4))))" `shouldBe` Right (Number 3)
      it "should eval list primitive 4" $
        evaluate "(cdr '(a simple test))" `shouldBe` Right (List [Atom "simple", Atom "test"])
      it "should eval list primitive 5" $
        evaluate "(car (cdr '(a simple test)))" `shouldBe` Right (Atom "simple")
      it "should eval list primitive 6" $
        evaluate "(car '((this is) a test))" `shouldBe` Right (List [Atom "this", Atom "is"])
      it "should eval list primitive 7" $
        evaluate "(cons '(this is) 'test)" `shouldBe` Right (DottedList [List [Atom "this", Atom "is"]] (Atom "test"))
      it "should eval list primitive 8" $
        evaluate "(cons '(this is) '())" `shouldBe` Right (List [List [Atom "this", Atom "is"]])
      it "should eval eq primitive 1" $
        evaluate "(eqv? 1 3)" `shouldBe` Right (Bool False)
      it "should eval eq primitive 2" $
        evaluate "(eqv? 3 3)" `shouldBe` Right (Bool True)
      it "should eval eq primitive 3" $
        evaluate "(eqv? 'atom 'atom)" `shouldBe` Right (Bool True)
      it "should eval cond primitive 1" $
        evaluate "(cond ((> 3 2) 'greater) ((< 3 2) 'less))" `shouldBe` Right (Atom "greater")
      it "should eval case primitive 1" $
        evaluate "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))" `shouldBe` Right (Atom "composite")
  where
    evaluate = getEvaledValue . evaluateOnEmptyContext . head . getTree . parse
    getEvaledValue = fst
    getTree = fromRight [Atom ""]