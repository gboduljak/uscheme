{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Language.Scheme.Parser
-- Copyright   : Justin Ethier
-- Licence     : MIT (see LICENSE in the distribution)
--
-- Maintainer  : github.com/justinethier
-- Stability   : experimental
-- Portability : portable
--
-- This module implements parsing of Scheme code.
module HuskParser
  ( lispDef,

    -- * Higher level parsing
    mainParser,
    huskParse,

    -- * Low level parsing
    parseExpr,
    parseAtom,
    parseBool,
    parseOctalNumber,
    parseBinaryNumber,
    parseHexNumber,
    parseDecimalNumber,
    parseNumber,
    parseEscapedChar,
    parseString,
    parseVector,
    parseList,
    parseDottedList,
    parseQuoted,
    parseQuasiQuoted,
    parseUnquoted,
    parseUnquoteSpliced,
  )
where

import Ast
import qualified Data.Char as DC
import Data.Complex
import Data.Functor.Identity (Identity)
import Data.Ratio
import Data.Word
import GHC.Arr (listArray)
import Numeric
import Text.Parsec.Language
import Text.Parsec.Prim (ParsecT)
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding (spaces)

-- This was added by pull request #63 as part of a series of fixes
-- to get husk to build on ghc 7.2.2
--
-- For now this has been removed to allow husk to support the older
-- GHC 6.x.x series.
--
--import Data.Functor.Identity (Identity)

-- | Language definition for Scheme
lispDef :: LanguageDef ()
lispDef =
  emptyDef
    { P.commentStart = "#|",
      P.commentEnd = "|#",
      P.commentLine = ";",
      P.nestedComments = True,
      P.identStart = letter <|> symbol,
      P.identLetter = letter <|> digit <|> symbol,
      P.reservedNames = [],
      P.caseSensitive = True
    }

lexer :: P.GenTokenParser String () Data.Functor.Identity.Identity
lexer = P.makeTokenParser lispDef

dot :: ParsecT String () Identity String
dot = P.dot lexer

parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = P.parens lexer

brackets :: ParsecT String () Identity a -> ParsecT String () Identity a
brackets = P.brackets lexer

identifier :: ParsecT String () Identity String
identifier = P.identifier lexer

whiteSpace :: ParsecT String () Identity ()
whiteSpace = P.whiteSpace lexer

lexeme :: ParsecT String () Identity a -> ParsecT String () Identity a
lexeme = P.lexeme lexer

-- | Match a special character
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

-- | Parse an atom (scheme symbol)
parseAtom :: Parser LispVal
parseAtom = do
  atom <- identifier
  if atom == "."
    then pzero -- Do not match this form
    else return $ Atom atom

-- | Parse a boolean
parseBool :: Parser LispVal
parseBool = do
  _ <- string "#"
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False
    _ -> Bool False

-- | Parse an integer in octal notation, base 8
parseOctalNumber :: Parser LispVal
parseOctalNumber = do
  _ <- try (string "#o")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01234567")
  case (length sign) of
    0 -> return $ Number $ fst $ head (Numeric.readOct num)
    1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readOct num)
    _ -> pzero

-- | Parse an integer in binary notation, base 2
parseBinaryNumber :: Parser LispVal
parseBinaryNumber = do
  _ <- try (string "#b")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01")
  case (length sign) of
    0 -> return $ Number $ fst $ head (Numeric.readInt 2 (`elem` "01") DC.digitToInt num)
    1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readInt 2 (`elem` "01") DC.digitToInt num)
    _ -> pzero

-- | Parse an integer in hexadecimal notation, base 16
parseHexNumber :: Parser LispVal
parseHexNumber = do
  _ <- try (string "#x")
  sign <- many (oneOf "-")
  num <- many1 (digit <|> oneOf "abcdefABCDEF")
  case (length sign) of
    0 -> return $ Number $ fst $ head (Numeric.readHex num)
    1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ head (Numeric.readHex num)
    _ -> pzero

-- | Parser for Integer, base 10
parseDecimalNumber :: Parser LispVal
parseDecimalNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 digit
  if length sign > 1
    then pzero
    else return $ (Number . read) $ sign ++ num

-- | Parse an integer in any base
parseNumber :: Parser LispVal
parseNumber =
  parseDecimalNumber
    <|> parseHexNumber
    <|> parseBinaryNumber
    <|> parseOctalNumber
    <?> "Unable to parse number"

-- | Parse an escaped character
parseEscapedChar ::
  forall st.
  GenParser Char st Char
parseEscapedChar = do
  _ <- char '\\'
  c <- anyChar
  case c of
    'a' -> return '\a'
    'b' -> return '\b'
    'n' -> return '\n'
    't' -> return '\t'
    'r' -> return '\r'
    'x' -> do
      num <- many $ letter <|> digit
      _ <- char ';'
      parseHexScalar num
    _ -> return c

-- | Parse a hexidecimal scalar
parseHexScalar :: String -> GenParser Char st Char
parseHexScalar num = do
  let ns = Numeric.readHex num
  case ns of
    [] -> fail $ "Unable to parse hex value " ++ show num
    _ -> return $ DC.chr $ fst $ head ns

-- | Parse a string
parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (parseEscapedChar <|> noneOf "\"")
  _ <- char '"'
  return $ String x

-- | Parse a vector
parseVector :: Parser LispVal
parseVector = do
  vals <- sepBy parseExpr whiteSpace
  return $ Vector vals

-- | Parse a list
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr whiteSpace

-- TODO: wanted to use endBy (or a variant) above, but it causes an error such that dotted lists are not parsed

-- | Parse a dotted list (scheme pair)
parseDottedList :: Parser LispVal
parseDottedList = do
  phead <- endBy parseExpr whiteSpace
  case phead of
    [] -> pzero -- car is required; no match
    _ -> do
      ptail <- dot >> parseExpr
      case ptail of
        DottedList ls l -> return $ DottedList (phead ++ ls) l
        -- Issue #41
        -- Improper lists are tricky because if an improper list ends in a
        -- proper list, then it becomes proper as well. The following cases
        -- handle that, as well as preserving necessary functionality when
        -- appropriate, such as for unquoting.
        --
        -- FUTURE: I am not sure if this is complete, in fact the "unquote"
        -- seems like it could either be incorrect or one special case among
        -- others. Anyway, for the 3.3 release this is good enough to pass all
        -- test cases. It will be revisited later if necessary.
        --
        List (Atom "unquote" : _) -> return $ DottedList phead ptail
        List ls -> return $ List $ phead ++ ls
        {- Regarding above, see
           http://community.schemewiki.org/?scheme-faq-language#dottedapp

           Note, however, that most Schemes expand literal lists occurring in
           function applications, e.g. (foo bar . (1 2 3)) is expanded into
           (foo bar 1 2 3) by the reader. It is not entirely clear whether this
           is a consequence of the standard - the notation is not part of the
           R5RS grammar but there is strong evidence to suggest a Scheme
           implementation cannot comply with all of R5RS without performing this
           transformation. -}
        _ -> return $ DottedList phead ptail

-- | Parse a quoted expression
parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- lexeme $ char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- | Parse a quasi-quoted expression
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- lexeme $ char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

-- | Parse an unquoted expression (a quasiquotated expression preceded
--   by a comma)
parseUnquoted :: Parser LispVal
parseUnquoted = do
  _ <- try (lexeme $ char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- | Parse an unquote-spliced expression
parseUnquoteSpliced :: Parser LispVal
parseUnquoteSpliced = do
  _ <- try (lexeme $ string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

-- FUTURE: should be able to use the grammar from R5RS
-- to make parsing more efficient (mostly by minimizing
-- or eliminating the number of try's below)

-- | Parse an expression
parseExpr :: Parser LispVal
parseExpr =
  try (lexeme parseNumber)
    <|> parseUnquoteSpliced
    <|> do
      _ <- try (lexeme $ string "#(")
      x <- parseVector
      _ <- lexeme $ char ')'
      return x
    --  <|> do _ <- try (lexeme $ string "#hash(")
    --         x <- parseHashTable
    --         _ <- lexeme $ char ')'
    --         return x
    <|> try parseAtom
    <|> lexeme parseString
    <|> lexeme parseBool
    <|> parseQuoted
    <|> parseQuasiQuoted
    <|> parseUnquoted
    <|> try (parens parseList)
    <|> parens parseDottedList
    <|> try (brackets parseList)
    <|> brackets parseDottedList
    <?> "Expression"

-- | Initial parser used by the high-level parse functions
mainParser :: Parser LispVal
mainParser = do
  _ <- whiteSpace
  parseExpr

huskParse :: String -> Either ParseError LispVal
huskParse = parse mainParser ""