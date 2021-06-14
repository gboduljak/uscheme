module Lib
  ( someFunc,
    nestedInteger,
  )
where

import Data.Char
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

data NestedInteger = Simple Int | Nested [NestedInteger] deriving (Show)

nestedInteger :: Parser NestedInteger
nestedInteger = simple <|> nested
  where
    simple = do Simple <$> number
    nested = do
      item (char '[')
      ns <- nestedInteger `sepBy` item (char ',')
      item (char ']')
      return (Nested ns)

number :: Parser Int
number = negative <|> positive
  where
    positive = do
      xs <- many1 (digitToInt <$> item (satisfy isDigit))
      return (asInt xs)
    negative = do
      item (char '-')
      positive

    asInt :: [Int] -> Int
    asInt xs = sum [(10 ^ p) * x | (p, x) <- zip [0 ..] (reverse xs)]

item :: Parser a -> Parser a
item p = do
  many (satisfy isSpace)
  x <- p
  many (satisfy isSpace)
  return x

someFunc :: IO ()
someFunc = putStrLn "someFunc"
