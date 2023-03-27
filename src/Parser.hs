module Parser where

import AST
import Lexer

-- | Parse an input string into an expression.
parse :: [Token] -> Expr
parse tokens =
  case parseExpr tokens of
    (expr, []) -> expr
    (_, tok : _) -> error $ "Unexpected token: " ++ show tok

-- | Parse an expression.
parseExpr :: [Token] -> (Expr, [Token])
parseExpr = parseOrExpr

-- | Parse an OR expression.
parseOrExpr :: [Token] -> (Expr, [Token])
parseOrExpr tokens =
  case parseAndExpr tokens of
    (expr, TokOr : rest) ->
      let (expr', rest') = parseOrExpr rest
       in (Or expr expr', rest')
    result -> result

-- | Parse an AND expression.
parseAndExpr :: [Token] -> (Expr, [Token])
parseAndExpr tokens =
  case parseEqExpr tokens of
    (expr, TokAnd : rest) ->
      let (expr', rest') = parseAndExpr rest
       in (And expr expr', rest')
    result -> result

-- | Parse an equality expression.
parseEqExpr :: [Token] -> (Expr, [Token])
parseEqExpr tokens =
  case parseRelExpr tokens of
    (expr, TokEq : rest) ->
      let (expr', rest') = parseEqExpr rest
       in (Eq expr expr', rest')
    (expr, TokNeq : rest) ->
      let (expr', rest') = parseEqExpr rest
       in (Neq expr expr', rest')
    result -> result

-- | Parse a relational expression.
parseRelExpr :: [Token] -> (Expr, [Token])
parseRelExpr tokens =
  case parseAddExpr tokens of
    (expr, TokLt : rest) ->
      let (expr', rest') = parseRelExpr rest
       in (Lt expr expr', rest')
    (expr, TokGt : rest) ->
      let (expr', rest') = parseRelExpr rest
       in (Gt expr expr', rest')
    (expr, TokLeq : rest) ->
      let (expr', rest') = parseRelExpr rest
       in (Leq expr expr', rest')
    (expr, TokGeq : rest) ->
      let (expr', rest') = parseRelExpr rest
       in (Geq expr expr', rest')
    result -> result

-- | Parse an addition or subtraction expression.
parseAddExpr :: [Token] -> (Expr, [Token])
parseAddExpr tokens =
  case parseMulExpr tokens of
    (expr, TokPlus : rest) ->
      let (expr', rest') = parseAddExpr rest
       in (Add expr expr', rest')
    (expr, TokMinus : rest) ->
      let (expr', rest') = parseAddExpr rest
       in (Sub expr expr', rest')
    result -> result

-- | Parse a multiplication or division expression.
parseMulExpr :: [Token] -> (Expr, [Token])
parseMulExpr tokens =
  case parseUnaryExpr tokens of
    (expr, TokMult : rest) ->
      let (expr', rest') = parseMulExpr rest
       in (Mul expr expr', rest')
    (expr, TokDiv : rest) ->
      let (expr', rest') = parseMulExpr rest
       in (Div expr expr', rest')
    result -> result

-- | Parse a unary expression.
parseUnaryExpr :: [Token] -> (Expr, [Token])
parseUnaryExpr (TokNot : rest) =
  let (expr, rest') = parseUnaryExpr rest
   in (Not expr, rest')
parseUnaryExpr (TokOpenParen : rest) =
  let (expr, TokCloseParen : rest') = parseExpr rest
   in (expr, rest')
parseUnaryExpr (TokInt n : rest) = (IntLit n, rest)
parseUnaryExpr (TokTrue : rest) = (BoolLit True, rest)
parseUnaryExpr (TokFalse : rest) = (BoolLit False, rest)
parseUnaryExpr (TokVar x : rest) = (Var x, rest)
parseUnaryExpr tokens = error $ "Unexpected token: " ++ show tokens
