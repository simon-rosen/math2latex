{-|
Module      : Latex.MathAST
Description :  Defines an AST

This module defines an Abstract Syntax Tree for math expressions in a latex format.
-}


module Latex.MathAST
  ( toLatex
  ) where

import           Data.MathAST (Expr (..), Factor (..), Term (..))

class Latexable a where
  toLatex :: a -> String

instance Latexable Expr where
  toLatex (ExprVal term) = toLatex term
  toLatex (Add t e)      = toLatex t ++ " + " ++ toLatex e
  toLatex (Sub t e)      = toLatex t ++ " - " ++ toLatex e

instance Latexable Term where
  toLatex (TermVal f) = toLatex f
  toLatex (Mult f t)  = toLatex f ++ " \\cdot " ++ toLatex t
  toLatex (Div f t)   = "\\frac{" ++ toLatex f ++ "}{" ++ toLatex t ++ "}"

instance Latexable Factor where
  toLatex (FactorVal n) = show n
  toLatex (Factor e)    = "(" ++ toLatex e ++ ")"
