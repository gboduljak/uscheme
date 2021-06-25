module Lexer
  ( junk,
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
    nil,
  )
where

import Ast
import Control.Monad (void)
import qualified Control.Monad
import Text.Parsec
  ( anyChar,
    char,
    digit,
    eof,
    letter,
    lookAhead,
    many,
    many1,
    noneOf,
    notFollowedBy,
    oneOf,
    option,
    optionMaybe,
    optional,
    skipMany,
    space,
    spaces,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Parsec as P (string)
import Text.ParserCombinators.Parsec (Parser, manyTill)
import qualified Text.ParserCombinators.Parsec as Parsec (string)

ignore :: Parser a -> Parser ()
ignore p = do p; return ()

spaces :: Parser ()
spaces = skipMany space

junk :: Parser ()
junk = skipMany (void space <|> comment)

comment :: Parser ()
comment = singleLine <|> multiLine
  where
    singleLine = do
      char ';'
      manyTill anyChar (char '\n')
      return ()
    multiLine = do
      try (Parsec.string "#|")
      manyTill (multiLine <|> void anyChar) (try (Parsec.string "|#"))
      return ()

lexeme :: Parser a -> Parser a
lexeme p = do x <- p; junk; return x

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
    junk
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
      lexeme (try (Parsec.string "#t") <|> try (Parsec.string "true"))
      return (Bool True)
    false = do
      lexeme (try (Parsec.string "#f") <|> try (Parsec.string "false"))
      return (Bool False)

nil :: Parser ()
nil = try (char '\'' *> Parsec.string "()") *> return () <?> "nil"

number :: Parser LispVal
number = Number <$> lexeme double
  where
    integer :: Parser Double
    integer = do
      optional (Parsec.string "#d")
      xs <- many1 digit
      return (read xs)

    double :: Parser Double
    double = do
      sign <- sign
      intPt <- integer
      maybeNext <- lookAhead (optionMaybe anyChar)
      case maybeNext of
        (Just next) ->
          if next == '.'
            then do
              char '.'
              afterDotDs <- many1 digit
              let afterDot = asDouble afterDotDs
              let fractPt = afterDot / (10 ^ length afterDotDs)
              notFollowedBy shouldNotFollowNumber
              return (sign * (intPt + fractPt))
            else do
              notFollowedBy shouldNotFollowNumber
              return (sign * intPt)
        _ -> return (sign * intPt)
      where
        asDouble :: String -> Double
        asDouble = read

    sign :: Parser Double
    sign = negative <|> positive
      where
        negative = do try (char '-'); return (-1)
        positive = do return 1

shouldNotFollowNumber :: Parser ()
shouldNotFollowNumber = ignore number <|> ignore atom <|> ignore boolean