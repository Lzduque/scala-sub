module Lexer where

import qualified Data.Char as Char

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
  | Char.isSpace c = tokenize cs
  | Char.isDigit c = TokInt (read num) : tokenize restNum
  | Char.isAlpha c = identifier : tokenize restId
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
    (num, restNum) = span Char.isDigit (c : cs)
    (ident, restId) = span Char.isAlphaNum (c : cs)
    identifier =
      case ident of
        "true" -> TokTrue
        "false" -> TokFalse
        "if" -> TokIf
        "then" -> TokThen
        "else" -> TokElse
        _ -> TokVar ident