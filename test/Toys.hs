module Toys (toysSpec) where

import Ast (LispVal (..))
import Helpers (fails)
import Parser (expr)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.ParserCombinators.Parsec (parse)

toysSpec :: Spec
toysSpec = do
  describe "toysSpec" $ do
    it "should parse atom" $ do
      parse expr "" "atom" `shouldBe` Right (Atom "atom")
    it "should parse number" $ do
      parse expr "" "1492" `shouldBe` Right (Number 1492)
    it "should parse another atom" $ do
      parse expr "" "*abc$" `shouldBe` Right (Atom "*abc$")