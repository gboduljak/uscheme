module Lexer
  ( spaces,
    lexeme,
    dot,
    comma,
    openParens,
    closeParens,
    openVec,
    quote,
    quasiquote,
    Lexer.string,
    atom,
    boolean,
    number,
  )
where

import Ast
import Data.Char
import Numeric
import Text.Parsec hiding (spaces)
import qualified Text.Parsec as P (string)
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.ParserCombinators.Parsec as Parsec (string)

ignore :: Parser a -> Parser ()
ignore p = do p; return ()

spaces :: Parser ()
spaces = skipMany space

lexeme :: Parser a -> Parser a
lexeme p = do x <- p; spaces; return x

dot :: Parser Char
dot = lexeme (char '.')

comma :: Parser Char
comma = lexeme (char ',')

openParens :: Parser Char
openParens = lexeme (char '(')

closeParens :: Parser Char
closeParens = lexeme (char ')')

openVec :: Parser String
openVec = try (lexeme (Parsec.string "#("))

quote :: Parser Char
quote = lexeme (char '\'')

quasiquote :: Parser Char
quasiquote = lexeme (char '`')

unquote :: Parser Char
unquote = lexeme (char ',')

string :: Parser LispVal
string =
  do
    char '"'
    x <- many (escapedCharacter <|> simpleCharacter)
    char '"'
    spaces
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
atom = lexeme symbol <|> lexeme plus <|> lexeme minus <|> lexeme (try ellipsis)
  where
    symbol = do
      first <- initial
      rest <- many subsequent
      return (Atom (first : rest))
    initial = letter <|> oneOf "!$%&*/|:<=>?-_^"
    subsequent = initial <|> digit <|> oneOf ".+-"
    plus = do char '+'; return (Atom "+")
    minus = do char '-'; return (Atom "-")
    ellipsis = do Parsec.string "..."; return (Atom "...")

boolean :: Parser LispVal
boolean = true <|> false
  where
    true = do
      lexeme $ try (Parsec.string "#t")
      return (Bool True)
    false = do
      lexeme $ try (Parsec.string "#f")
      return (Bool False)

--todo
number :: Parser LispVal
number =
  Number
    <$> lexeme (try (radix2) <|> try (radix8) <|> try (radix16) <|> try (radix10))
  where
    radix2 :: Parser Integer
    radix2 = do
      Parsec.string "#b"
      xs <- many1 (oneOf "01")
      notFollowedBy shouldNotFollowNumber
      return (asRadix2 xs)
      where
        asRadix2 :: [Char] -> Integer
        asRadix2 = fst . head . readInt 2 (`elem` "01") digitToInt
    radix8 :: Parser Integer
    radix8 = do
      Parsec.string "#o"
      xs <- many1 (oneOf "01234567")
      notFollowedBy shouldNotFollowNumber
      return (asRadix8 xs)
      where
        asRadix8 :: [Char] -> Integer
        asRadix8 = fst . head . readOct

    radix16 :: Parser Integer
    radix16 = do
      Parsec.string "#h"
      xs <- many1 (oneOf "0123456789abcdef")
      notFollowedBy shouldNotFollowNumber
      return (asRadix16 xs)
      where
        asRadix16 :: [Char] -> Integer
        asRadix16 = fst . head . readHex

    radix10 :: Parser Integer
    radix10 = do
      optional (Parsec.string "#d")
      xs <- many1 digit
      notFollowedBy shouldNotFollowNumber
      return (read xs)

shouldNotFollowNumber :: Parser ()
shouldNotFollowNumber = ignore number <|> ignore atom <|> ignore boolean