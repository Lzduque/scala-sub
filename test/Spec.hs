module Spec (spec) where

import AST
import Lexer
import Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "Lexer.tokenize" $ do
    it "tokenizes a simple expression" $ do
      let input = "val x = 1 + 2 * 3"
          expectedOutput = [TokVal, TokIdent "x", TokEq, TokIntLit 1, TokAdd, TokIntLit 2, TokMul, TokIntLit 3]
      tokenize input `shouldBe` Right expectedOutput

    it "tokenizes an if-else expression" $ do
      let input = "if (x < 10) true else false"
          expectedOutput = [TokIf, TokLParen, TokIdent "x", TokLt, TokIntLit 10, TokRParen, TokTrue, TokElse, TokFalse]
      tokenize input `shouldBe` Right expectedOutput

    it "returns an error for an invalid token" $ do
      let input = "val x = 1 $ 2"
      tokenize input `shouldBe` Left "Invalid token: $"

  describe "parseExpr" $ do
    it "parses variable expressions" $ do
      parseExpr "x" `shouldBe` Right (Var "x")
      parseExpr "foo" `shouldBe` Right (Var "foo")
    it "parses integer literal expressions" $ do
      parseExpr "42" `shouldBe` Right (IntLit 42)
      parseExpr "-123" `shouldBe` Right (IntLit (-123))
    it "parses boolean literal expressions" $ do
      parseExpr "true" `shouldBe` Right (BoolLit True)
      parseExpr "false" `shouldBe` Right (BoolLit False)
    it "parses binary expressions" $ do
      parseExpr "x + y" `shouldBe` Right (Add (Var "x") (Var "y"))
      parseExpr "x * y + z" `shouldBe` Right (Add (Mul (Var "x") (Var "y")) (Var "z"))
      parseExpr "x / y - z" `shouldBe` Right (Sub (Div (Var "x") (Var "y")) (Var "z"))
    it "parses unary expressions" $ do
      parseExpr "!x" `shouldBe` Right (Not (Var "x"))
      parseExpr "-42" `shouldBe` Right (Sub (IntLit 0) (IntLit 42))
    it "parses comparison expressions" $ do
      parseExpr "x < y" `shouldBe` Right (Lt (Var "x") (Var "y"))
      parseExpr "x > y" `shouldBe` Right (Gt (Var "x") (Var "y"))
      parseExpr "x <= y" `shouldBe` Right (Leq (Var "x") (Var "y"))
      parseExpr "x >= y" `shouldBe` Right (Geq (Var "x") (Var "y"))
      parseExpr "x == y" `shouldBe` Right (Eq (Var "x") (Var "y"))
      parseExpr "x != y" `shouldBe` Right (Neq (Var "x") (Var "y"))
    it "parses if-then-else expressions" $ do
      parseExpr "if x < y then z else w" `shouldBe` Right (If (Lt (Var "x") (Var "y")) (Var "z") (Var "w"))
