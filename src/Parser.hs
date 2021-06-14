{-# LANGUAGE BlockArguments #-}

module Parser
  ( symbol,
    spaces,
    string,
    atom,
    expr,
  )
where

import Ast
import Data.Char
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces, string, symbol, token)
import qualified Text.ParserCombinators.Parsec as Parsec (string)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany space

string :: Parser LispVal
string =
  ( do
      char '"'
      x <- many (escapedCharacter <|> simpleCharacter)
      char '"'
      return (String x)
  )
    <?> "string"
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
  ( do
      first <- letter <|> symbol
      rest <- many (letter <|> symbol <|> digit)
      let atom = first : rest
      return
        ( case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom atom
        )
  )
    <?> "atom"

--todo
number :: Parser LispVal
number = Number <$> (radix2 <|> radix8 <|> radix16 <|> radix10)
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
  ( number
      <|> string
      <|> atom
      <|> list
  )
    <?> "expression"

list :: Parser LispVal
list = nonQuoted <|> quoted
  where
    quoted =
      ( do
          char '\''
          x <- expr
          return (List [Atom "quote", x])
      )
        <?> "quoted list"

    nonQuoted =
      do
        char '('
        (isDotted, head) <- arguments
        if isDotted
          then
            ( do
                tail <- argument
                char ')'
                return (DottedList head tail)
            )
              <?> "dotted list"
          else
            ( do
                char ')'
                return (List head)
            )
              <?> "list"

    arguments =
      ( do
          xs <- manyTill argument (lookAhead (char '.' <|> char ')'))
          look <- lookAhead (char '.' <|> char ')')
          if look == '.'
            then char '.' >> do return (True, xs)
            else return (False, xs)
      )
        <?> "arguments"

    argument =
      ( do
          spaces
          x <- expr
          spaces
          return x
      )
        <?> "argument"