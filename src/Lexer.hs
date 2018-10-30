module Lexer
  ( sc
  , integer
  , Parser
  , symbol
  , parens
  , identifier
  , reservedWord
  , boolean
  ) where

import           Data.Functor               (($>))
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Type alias for the a Parser which has 'Void' error type and has takes 'String's
type Parser = Parsec Void String

-- | sc: Space Consumer, consumes all spaces and returns unit, skipping lines
-- that have line comments (which start with "//")
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

-- | Every lexeme will have the whitespace __after__ it consumed.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- TODO: make reserved symbol table
-- | Parse a symbol: a fixed string
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parses something between (parenthesis).
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse an integer
integer :: Parser Integer
integer = lexeme L.decimal

-- | Parse a boolean
boolean :: Parser Bool
boolean = symbol "true" $> True <|> symbol "false" $> False

-- | Parse a reserved word
reservedWord :: String -> Parser ()
reservedWord word = (lexeme . try) (string word *> notFollowedBy alphaNumChar)

-- | A list of reserved words
reservedWords :: [String]
reservedWords = ["let", "in", "fun", "if", "then", "else", "true", "false"]

-- | Parses an identifier, checking if it is a reserved word
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x
