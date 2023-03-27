module Compiler where

import AST
import Lexer
import Parser

-- | Compile an expression.
compileExpr :: Expr -> String
compileExpr (Var x) = x
compileExpr (IntLit n) = show n
compileExpr (BoolLit b) = if b then "true" else "false"
compileExpr (Add e1 e2) = parenthesize $ compileExpr e1 ++ " + " ++ compileExpr e2
compileExpr (Sub e1 e2) = parenthesize $ compileExpr e1 ++ " - " ++ compileExpr e2
compileExpr (Mul e1 e2) = parenthesize $ compileExpr e1 ++ " * " ++ compileExpr e2
compileExpr (Div e1 e2) = parenthesize $ compileExpr e1 ++ " / " ++ compileExpr e2
compileExpr (And e1 e2) = parenthesize $ compileExpr e1 ++ " && " ++ compileExpr e2
compileExpr (Or e1 e2) = parenthesize $ compileExpr e1 ++ " || " ++ compileExpr e2
compileExpr (Not e) = "!" ++ parenthesize (compileExpr e)
compileExpr (Eq e1 e2) = parenthesize $ compileExpr e1 ++ " === " ++ compileExpr e2
compileExpr (Neq e1 e2) = parenthesize $ compileExpr e1 ++ " !== " ++ compileExpr e2
compileExpr (Lt e1 e2) = parenthesize $ compileExpr e1 ++ " < " ++ compileExpr e2
compileExpr (Gt e1 e2) = parenthesize $ compileExpr e1 ++ " > " ++ compileExpr e2
compileExpr (Leq e1 e2) = parenthesize $ compileExpr e1 ++ " <= " ++ compileExpr e2
compileExpr (Geq e1 e2) = parenthesize $ compileExpr e1 ++ " >= " ++ compileExpr e2
compileExpr (If condExpr thenExpr elseExpr) =
  "if (" ++ compileExpr condExpr ++ ") {" ++ compileExpr thenExpr ++ "} else {" ++ compileExpr elseExpr ++ "}"

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

-- | Compile a string of Scala code to JavaScript.
compile :: String -> String
compile input =
  case parseExpr (tokenize input) of
    (expr, []) -> compileExpr expr
    (expr, [TokEOF]) -> compileExpr expr
    (_, tokens) -> error $ "Unexpected tokens: " ++ show tokens
