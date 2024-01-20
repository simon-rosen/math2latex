module Main (main) where

import           Latex
import           MathAST

main :: IO ()
main = do
  input <- getLine
  let ast = parseExpr input
  putStrLn (toLatex ast)
