module Main where

import AST
import Compiler

main :: IO ()
main = do
  -- Playing with making expressions manually
  print $ Add (IntLit 2) (IntLit 3) -- 2 + 3
  print $ Add (Mul (IntLit 2) (IntLit 5)) (IntLit 3) -- (2 * 5) + 3
  print $ If (Gt (Var "x") (IntLit 3)) (Add (Var "x") (IntLit 3)) (Mul (Var "x") (IntLit 3)) -- if x > 3 then x + 3 else x * 3
  putStrLn $ compile "1 + 2 * 3"
  putStrLn $ compile "(1 + 2) * 3"
  putStrLn $ compile "!(1 <= 2)"
  putStrLn $ compile "true | false & true"
