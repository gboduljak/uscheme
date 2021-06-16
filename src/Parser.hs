{-# LANGUAGE BlockArguments #-}

module Parser
  ( spaces,
    string,
    atom,
    expr,
    list,
    number,
  )
where

import Ast
import Data.Char
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces, string, symbol, token)
import qualified Text.ParserCombinators.Parsec as Parsec (string)

spaces :: Parser ()
spaces = skipMany1 space

string :: Parser LispVal
string =
  do
    char '"'
    x <- many (escapedCharacter <|> simpleCharacter)
    char '"'
    return (String x)
  where
    escapedCharacter = do
      char '\\'
      x <- oneOf "\\\"nrt"
      case x of
        '\\' -> return x
        '"' -> return x
        't' -> return '\t'
        'n' -> return '\n'
        'r' -> return '\r'
        _ -> error "not escaped"
    simpleCharacter = noneOf "\""

atom :: Parser LispVal
atom =
  do
    first <- initial
    rest <- many subsequent
    return (Atom (first : rest))
  where
    initial :: Parser Char
    initial = letter <|> oneOf "!$%&*/:<=>?-_^"
    subsequent = initial <|> digit <|> oneOf ".+-"

boolean :: Parser LispVal
boolean = do
  char '#'
  true <|> false
  where
    true = do
      char 't'
      return (Bool True)
    false = do
      char 'f'
      return (Bool False)

--todo
number :: Parser LispVal
number = Number <$> (try radix2 <|> try radix8 <|> try radix16 <|> try radix10)
  where
    radix2 :: Parser Integer
    radix2 = do
      Parsec.string "#b"
      fst . head . readInt 2 (`elem` "01") digitToInt <$> many1 (oneOf "01")
    radix8 :: Parser Integer
    radix8 = do
      Parsec.string "#o"
      fst . head . readOct <$> many1 (oneOf "01234567")
    radix16 :: Parser Integer
    radix16 = do
      Parsec.string "#h"
      fst . head . readHex <$> many1 (oneOf "0123456789abcdef")
    radix10 :: Parser Integer
    radix10 = do
      optional (Parsec.string "#d")
      read <$> many1 digit

expr :: Parser LispVal
expr =
  list
    <|> try vector
    <|> try number
    <|> try boolean
    <|> string
    <|> atom

vector :: Parser LispVal
vector = do
  char '#'
  char '('
  optional spaces
  vec <- Vector <$> sepEndBy expr spaces
  char ')'
  return vec

list :: Parser LispVal
list = do nonQuotedList <|> quotedList
  where
    quotedList =
      do
        char '\''
        optional spaces
        x <- expr
        return (List [Atom "quote", x])
    nonQuotedList =
      do
        char '('
        optional spaces
        xs <- sepEndBy expr spaces
        nonDotted xs <|> dotted xs
      where
        nonDotted :: [LispVal] -> Parser LispVal
        nonDotted xs = do
          char ')'
          return (List xs)
        dotted xs =
          do
            char '.'
            x <- spaces >> expr
            many spaces
            char ')'
            return (DottedList xs x)

spaced :: Parser a -> Parser a
spaced p = do
  spaces
  x <- p
  spaces
  return x
