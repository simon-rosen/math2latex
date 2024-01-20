{-|
Module      : Parser
Description :  This module defines a generic parser.

This module defines a generic parser that is used to construct more
specialiced parsers. This parser combines lexing and parsing. Right
now it only parses integers and one character symbols.

This is basically the parser from "Programming in Haskell".
-}

{-# LANGUAGE LambdaCase #-}
module Parser
  ( Parser (..)
  , parse
  , int
  , symbol
  ) where

import           Control.Applicative
import           Data.Char

{-|
    A parser takes a string and returns a parsed value along with the
    rest of the string.

    To allow it to be a part of classes it is defined as newtype
-}
newtype Parser a = Parser (String -> [(a, String)])
{-|
   the 'parse' function applies a parser to a input string
   (removes the constructor)
-}
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

{-|
    'item' is a parsing primitive that takes the first character of the input.
    It fails if the input is empty.
-}
item :: Parser Char
item = Parser (\case -- <-- LambdaCase
                []     -> []
                (x:xs) -> [(x, xs)])

-- make the parser part of the Functor class
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = Parser (\input -> case parse p input of
                  []         -> []
                  [(v, out)] -> [(g v, out)]
                  _          -> [])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = Parser (\input -> [(v, input)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = Parser (\input -> case parse pg input of
                  []         -> []
                  [(g, out)] -> parse (fmap g px) out
                  _          -> [])

-- make parser part of the Monad class
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser (\input -> case parse p input of
                []         -> []
                [(v, out)] -> parse (f v) out
                _          -> [])

-- make the parser part of the Alternative class
instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser (const []) -- the parser that allways fails

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser (\input -> case parse p input of
                []         -> parse q input
                [(v, out)] -> [(v, out)]
                _          -> [])
  {-
   - this monad makes it possible to use `many` and `some`
   - which is used to apply a parser multiple times
   - -}


-- HELPER PARSERS
-- PARSERS ON THE FIRST CHAR
-- | a parser that succeds if the first character fulfills a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- | the first char is
char :: Char -> Parser Char
char c = sat (== c)

-- | the first char is a digit
digit :: Parser Char
digit = sat isDigit

-- | PARSERS ON MULTIPLE CHARACTERS
whitespace :: Parser ()
whitespace = do
  _ <- many (sat isSpace)
  return ()

-- | this applies a parser and ignores whitespace before and after it
token :: Parser a -> Parser a
token p = do
  whitespace
  v <- p
  whitespace
  return v

-- | a natural number is at least one digit
nat :: Parser Int
nat = token $ do
  xs <- some digit
  return (read xs)

-- | an integer can have i '-' in front of a natural number
int :: Parser Int
int = token $ do
  _ <- char '-'
  n <- nat
  return (-n)
  <|> nat

-- | a symbol is parsed (could be sorounded by whitespace)
symbol :: Char -> Parser Char
symbol c = token $ do char c

