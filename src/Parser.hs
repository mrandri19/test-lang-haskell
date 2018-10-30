{-|
This module contains the language's parser.
-}
module Parser
  ( parseExpr
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Control.Monad.Combinators.Expr

import           Lexer                          (Parser, boolean, identifier,
                                                 integer, parens, reservedWord,
                                                 sc, symbol)
import           Syntax

-- | parse an expression, consuming eventual whitespace at the start and eof at
-- the end.
parseExpr :: Parser Expr
parseExpr = between sc eof expr

-- | parse an expression
expr = makeExprParser term operators

-- | defines a table of operators with their precedence, associativity and which
-- constructor to use to build the expr. The operators are writeen in descending
-- precedence
operators :: [[Operator Parser Expr]]
operators =
  [ [application]
  , [binary Times "*", binary Divide "/"]
  , [binary Plus "+", binary Minus "-"]
  , [binary Greater ">", binary Lesser "<"]
  ]

binary operation symbolChars = InfixL ((BinOp operation) <$ symbol symbolChars)

application = InfixL $ return (\x y -> Apply x y)

term :: Parser Expr
term =
  parens expr <|> Var <$> identifier <|> Number <$> integer <|>
  Boolean <$> boolean <|>
  letBind <|>
  ifExpr <|>
  funDecl

letBind :: Parser Expr
letBind = do
  reservedWord "let"
  name <- identifier
  symbol "="
  expr1 <- expr
  reservedWord "in"
  expr2 <- expr
  return (LetBind name expr1 expr2)

funDecl :: Parser Expr
funDecl = do
  reservedWord "fun"
  name <- identifier
  symbol "->"
  expr1 <- expr
  return (Lambda name expr1)

ifExpr :: Parser Expr
ifExpr = do
  reservedWord "if"
  expr1 <- expr
  reservedWord "then"
  expr2 <- expr
  reservedWord "else"
  expr3 <- expr
  return (IfExpr expr1 expr2 expr3)
