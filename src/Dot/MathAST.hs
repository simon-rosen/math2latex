module Dot.MathAST where

import           Data.MathAST (Expr (..), Factor (..), Term (..))

class Dotable a where
  toDot :: a -> String

instance Dotable Expr where
  toDot (ExprVal t) = undefined
  toDot (Add t e)   = undefined
  toDot (Sub t e)   = undefined

instance Dotable Term where
  toDot (TermVal f) = undefined
  toDot (Mult f t)  = undefined
  toDot (Div f t)   = undefined

instance Dotable Factor where
  toDot (FactorVal n) = undefined
  toDot (Factor e)    = undefined

