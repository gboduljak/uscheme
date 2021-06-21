module Toys (toysSpec) where

import Ast (LispVal (..))
import Helpers (fails)
import Parser (expr)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.ParserCombinators.Parsec (parse)

toysSpec :: Spec
toysSpec = do
  describe "parsing Toys from the Little Schemer..." $ do
    it "should parse atom 1" $ do
      parse expr "" "atom" `shouldBe` Right (Atom "atom")
    it "should parse atom 2" $ do
      parse expr "" "turkey" `shouldBe` Right (Atom "turkey")
    it "should parse number" $ do
      parse expr "" "1492" `shouldBe` Right (Number 1492)
    it "should parse atom 3" $ do
      parse expr "" "u" `shouldBe` Right (Atom "u")
    it "should parse atom 4" $ do
      parse expr "" "*abc$" `shouldBe` Right (Atom "*abc$")
    it "should parse singleton list" $ do
      parse expr "" "(atom)" `shouldBe` Right (List [Atom "atom"])
    it "should parse atom list" $ do
      parse expr "" "(atom turkey or)" `shouldBe` Right (List [Atom "atom", Atom "turkey", Atom "or"])
    it "should parse list" $ do
      parse expr "" "((atom turkey) or)" `shouldBe` Right (List [List [Atom "atom", Atom "turkey"], Atom "or"])
    it "should parse S expr 1" $ do
      parse expr "" "xyz" `shouldBe` Right (Atom "xyz")
    it "should parse S expr 2" $ do
      parse expr "" "(x y z)" `shouldBe` Right (List [Atom "x", Atom "y", Atom "z"])
    it "should parse S expr 3" $ do
      parse expr "" "(how are you doing so far)" `shouldBe` Right (List [Atom "how", Atom "are", Atom "you", Atom "doing", Atom "so", Atom "far"])
    it "should parse S expr 4" $ do
      parse expr "" "(((how) are) ((you) (doing so)) far )" `shouldBe` Right (List [List [List [Atom "how"], Atom "are"], List [List [Atom "you"], List [Atom "doing", Atom "so"]], Atom "far"])
    it "should parse empty list" $ do
      parse expr "" "()" `shouldBe` Right (List [])
    it "should parse list of empty lists" $ do
      parse expr "" "(()()()())" `shouldBe` Right (List [List [], List [], List [], List []])
    it "should parse atom list 2" $ do
      parse expr "" "(a b c)" `shouldBe` Right (List [Atom "a", Atom "b", Atom "c"])
    it "should parse atom list 3" $ do
      parse expr "" "((a b c) x y z)" `shouldBe` Right (List [List [Atom "a", Atom "b", Atom "c"], Atom "x", Atom "y", Atom "z"])
