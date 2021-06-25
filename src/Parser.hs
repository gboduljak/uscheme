{-# LANGUAGE BlockArguments #-}

module Parser
  ( string,
    atom,
    expr,
    list,
    number,
    Parser.parse,
  )
where

import Ast (LispVal (Atom, DottedList, List, Nil, Vector))
import Data.Char
import Lexer
  ( atom,
    boolean,
    closeParens,
    comma,
    dot,
    junk,
    nil,
    number,
    openParens,
    openVec,
    quasiquote,
    quote,
    string,
  )
import Numeric
import Text.Parsec.Language
import Text.ParserCombinators.Parsec (ParseError, Parser, many, try, (<|>))
import qualified Text.ParserCombinators.Parsec as Parsec (parse, string)

parse :: String -> Either ParseError [LispVal]
parse = Parsec.parse (junk >> many expr) ""

expr :: Parser LispVal
expr = quotedExpr <|> quasiQuotedExpr <|> commaExpr <|> ordinaryExp
  where
    quotedExpr = do
      quote
      x <- expr
      return (List [Atom "quote", x])
    quasiQuotedExpr = do
      quasiquote
      x <- expr
      return (List [Atom "quasiquote", x])
    commaExpr = do
      comma
      x <- expr
      return (List [Atom "unquote", x])
    ordinaryExp = do
      list
        <|> try vector
        <|> try number
        <|> try boolean
        <|> string
        <|> atom
        <|> Nil <$ nil

vector :: Parser LispVal
vector = do
  openVec
  vec <- Vector <$> many expr
  closeParens
  return vec

list :: Parser LispVal
list = do
  openParens
  xs <- many expr
  nonDotted xs <|> dotted xs
  where
    nonDotted :: [LispVal] -> Parser LispVal
    nonDotted xs = do
      closeParens
      return (List xs)
    dotted :: [LispVal] -> Parser LispVal
    dotted xs =
      do
        dot
        x <- expr
        closeParens
        return (DottedList xs x)