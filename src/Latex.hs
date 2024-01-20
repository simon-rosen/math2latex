{-|
Module      : Latex
Description :  Defines a typeclass that is able to be converted to Latex format.

This module defines a type class that is used to convert types to latex-math format.
-}


module Latex
  ( Latexable (..)
  , toLatex
  ) where

class Latexable a where
  toLatex :: a -> String

