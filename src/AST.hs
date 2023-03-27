module AST where

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