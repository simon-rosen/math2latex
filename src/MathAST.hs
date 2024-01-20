{-|
Module      : MathAST
Description :  Defines an AST for math expressions

This module defines an Abstract Syntax Tree for the parsed expressions in
a traditional math format.
-}


module MathAST
  ( parseExpr
  , Number
  , Expr (..)
  , Term (..)
  , Factor (..)
  ) where

import           Control.Applicative
import           Latex
import           Parser

-- | the function 'parseExpr' parses a string into an abstract syntax tree
--parseExpr :: String -> Parser Expr
parseExpr :: String -> Expr
parseExpr input = ast
  where (ast, []):_ = parse expr input


-- | the abstract syntax tree datatypes
type Number = Int

data Expr = ExprVal Term
          | Add Term Expr
          | Sub Term Expr
          deriving (Show)

data Term = TermVal Factor
          | Mult Factor Term
          | Div Factor Term
          deriving (Show)

data Factor = FactorVal Number
            | Factor Expr
            deriving (Show)


-- the parser combinators
-- | Expression combinator
expr :: Parser Expr
expr = do
  t <- term
  do
    symbol '+'
    e <- expr
    return (Add t e)
    <|> do
    symbol '-'
    e <- expr
    return (Sub t e)
    <|> return (ExprVal t)

-- | Term level combinator
term :: Parser Term
term = do
  f <- factor
  do
    symbol '*'
    t <- term
    return (Mult f t)
    <|> do
    symbol '/'
    t <- term
    return (Div f t)
    <|> return (TermVal f)

-- | Factor level combinator
factor :: Parser Factor
factor = do
  symbol '('
  e <- expr
  symbol ')'
  return (Factor e)
  <|> do
  n <- int
  return (FactorVal n)


-- make the types Latexable
instance Latexable Expr where
  toLatex (ExprVal term) = toLatex term
  toLatex (Add t e)      = toLatex t ++ " + " ++ toLatex e
  toLatex (Sub t e)      = toLatex t ++ " - " ++ toLatex e

instance Latexable Term where
  toLatex (TermVal f) = toLatex f
  toLatex (Mult f t)  = toLatex f ++ " \\times " ++ toLatex t
  toLatex (Div f t)   = "\\frac{" ++ toLatex f ++ "}{" ++ toLatex t ++ "}"

instance Latexable Factor where
  toLatex (FactorVal n) = show n
  toLatex (Factor e)    = "(" ++ toLatex e ++ ")"
