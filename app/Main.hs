module Main where

import Data.Char

-- | A token represents a unit of the input string.
data Token
  = TokInt Int -- Integer literal
  | TokTrue -- Boolean true literal
  | TokFalse -- Boolean false literal
  | TokPlus -- Addition operator
  | TokMinus -- Subtraction operator
  | TokMult -- Multiplication operator
  | TokDiv -- Division operator
  | TokAnd -- Logical AND operator
  | TokOr -- Logical OR operator
  | TokNot -- Logical NOT operator
  | TokEq -- Equality operator
  | TokNeq -- Inequality operator
  | TokLt -- Less than operator
  | TokGt -- Greater than operator
  | TokLeq -- Less than or equal to operator
  | TokGeq -- Greater than or equal to operator
  | TokIf -- If keyword
  | TokThen -- Then keyword
  | TokElse -- Else keyword
  | TokOpenParen -- Opening parenthesis
  | TokCloseParen -- Closing parenthesis
  | TokVar String -- Variable identifier
  | TokAssign -- Assignment operator
  | TokSemicolon -- Semicolon
  | TokEOF -- End-of-file marker
  deriving (Show, Eq)

-- | Tokenize an input string.
tokenize :: String -> [Token]
tokenize [] = [TokEOF]
tokenize (c : cs)
  | isSpace c = tokenize cs
  | isDigit c = TokInt (read num) : tokenize restNum
  | isAlpha c = identifier : tokenize restId
  | otherwise = case c of
      '+' -> TokPlus : tokenize cs
      '-' -> TokMinus : tokenize cs
      '*' -> TokMult : tokenize cs
      '/' -> TokDiv : tokenize cs
      '&' -> TokAnd : tokenize cs
      '|' -> TokOr : tokenize cs
      '=' ->
        if head cs == '='
          then TokEq : tokenize (tail cs)
          else TokAssign : tokenize cs
      '!' ->
        if head cs == '='
          then TokNeq : tokenize (tail cs)
          else TokNot : tokenize cs
      '<' ->
        if head cs == '='
          then TokLeq : tokenize (tail cs)
          else TokLt : tokenize cs
      '>' ->
        if head cs == '='
          then TokGeq : tokenize (tail cs)
          else TokGt : tokenize cs
      '(' -> TokOpenParen : tokenize cs
      ')' -> TokCloseParen : tokenize cs
      ';' -> TokSemicolon : tokenize cs
      _ -> error $ "Unexpected character '" ++ [c] ++ "'"
  where
    (num, restNum) = span isDigit (c : cs)
    (ident, restId) = span isAlphaNum (c : cs)
    identifier =
      case ident of
        "true" -> TokTrue
        "false" -> TokFalse
        "if" -> TokIf
        "then" -> TokThen
        "else" -> TokElse
        _ -> TokVar ident

-- | The AST for our subset of Scala.
data Expr
  = Var String -- Variable
  | IntLit Int -- Integer literal
  | BoolLit Bool -- Boolean literal
  | Add Expr Expr -- Addition
  | Sub Expr Expr -- Subtraction
  | Mul Expr Expr -- Multiplication
  | Div Expr Expr -- Division
  | And Expr Expr -- Logical AND
  | Or Expr Expr -- Logical OR
  | Not Expr -- Logical NOT
  | Eq Expr Expr -- Equality
  | Neq Expr Expr -- Inequality
  | Lt Expr Expr -- Less than
  | Gt Expr Expr -- Greater than
  | Leq Expr Expr -- Less than or equal to
  | Geq Expr Expr -- Greater than or equal to
  | If Expr Expr Expr -- If-then-else expression
  deriving (Show, Eq)

-- | Parse an input string into an expression.
parse :: [Token] -> Expr
parse tokens =
  case parseExpr tokens of
    (expr, []) -> expr
    -- (expr, [TokEOF]) -> expr
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

-- | Tokenize an input string.
-- tokenize :: String -> [Token]
-- tokenize input = reverse $ tokenize' [] input
--   where
--     tokenize' acc [] = acc
--     tokenize' acc ('(' : rest) = tokenize' (TokOpenParen : acc) rest
--     tokenize' acc (')' : rest) = tokenize' (TokCloseParen : acc) rest
--     tokenize' acc ('*' : rest) = tokenize' (TokMult : acc) rest
--     tokenize' acc ('/' : rest) = tokenize' (TokDiv : acc) rest
--     tokenize' acc ('+' : rest) = tokenize' (TokPlus : acc) rest
--     tokenize' acc ('-' : rest) = tokenize' (TokMinus : acc) rest
--     tokenize' acc ('<' : '=' : rest) = tokenize' (TokLeq : acc) rest
--     tokenize' acc ('<' : rest) = tokenize' (TokLt : acc) rest
--     tokenize' acc ('>' : '=' : rest) = tokenize' (TokGeq : acc) rest
--     tokenize' acc ('>' : rest) = tokenize' (TokGt : acc) rest
--     tokenize' acc ('=' : '=' : rest) = tokenize' (TokEq : acc) rest
--     tokenize' acc ('!' : '=' : rest) = tokenize' (TokNeq : acc) rest
--     tokenize' acc ('&' : '&' : rest) = tokenize' (TokAnd : acc) rest
--     tokenize' acc ('|' : '|' : rest) = tokenize' (TokOr : acc) rest
--     tokenize' acc (c : rest)
--       | isSpace c = tokenize' acc rest
--       | isDigit c =
--           let (digits, rest') = span isDigit (c : rest)
--            in tokenize' (TokInt (read digits) : acc) rest'
--       | isAlpha c =
--           let (ident, rest') = span isAlphaNum (c : rest)
--            in tokenize'
--                 ( if ident == "true"
--                     then TokTrue
--                     else
--                       if ident == "false"
--                         then TokFalse
--                         else TokVar ident : acc
--                 )
--                 rest'
--       | otherwise = error $ "Unexpected character: " ++ show c

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

main :: IO ()
main = do
  print $ Add (IntLit 2) (IntLit 3) -- 2 + 3
  print $ Add (Mul (IntLit 2) (IntLit 5)) (IntLit 3) -- (2 * 5) + 3
  print $ If (Gt (Var "x") (IntLit 3)) (Add (Var "x") (IntLit 3)) (Mul (Var "x") (IntLit 3)) -- if x > 3 then x + 3 else x * 3
  putStrLn $ compile "1 + 2 * 3"
  putStrLn $ compile "(1 + 2) * 3"
  putStrLn $ compile "!(1 <= 2)"
  putStrLn $ compile "true | false & true"
