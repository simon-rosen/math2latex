module Main (main) where

import           Data.MathAST
import           Data.Parser
import           Latex.MathAST

main :: IO ()
main = do
  input <- getLine
  let ast = parseExpr input
  putStrLn (toLatex ast)
