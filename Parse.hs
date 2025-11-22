{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -Wall #-}

module Parse
  ( parseProgram,
    Expr (..), -- (..) exports all constructors
    TopLevel (..),
    Op (..),
    UnaryOp (..),
    sc,
    expr,
  )
where

import ComplexRational (ComplexRational (CR))
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Either (isRight)
import Data.Ratio ((%))
import Data.Void (Void)
import Test.QuickCheck (Arbitrary (arbitrary), Property, Testable (property), choose, counterexample, elements, vectorOf, (==>))
import Test.QuickCheck.Test
  ( Args (chatty),
    quickCheck,
    quickCheckWith,
    stdArgs,
  )
import TestUtils
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec, SourcePos (SourcePos, sourceColumn, sourceLine, sourceName), between, choice, getSourcePos, many, mkPos, option, optional, parse, satisfy, sepEndBy1, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme $ do
  first <- letterChar
  rest <- many alphaNumChar
  pure (first : rest)

-- Parse a proper rational number (with decimal point)
rationalParser :: Parser Rational
rationalParser = do
  intPart <- L.decimal
  _ <- char '.'
  fractionalStr <- some (satisfy isDigit)
  let fractionalPart = read fractionalStr :: Integer
  let denominator = 10 ^ length fractionalStr

  -- Combine integer and fractional parts
  return $ intPart % 1 + fractionalPart % denominator

integerAsRationalParser :: Parser Rational
integerAsRationalParser = do
  n <- L.decimal
  return $ n % 1

-- Parse a number as ComplexRational
number :: Parser ComplexRational
number = lexeme $ do
  sign <- option 1 (-1 <$ char '-') -- Parse optional minus sign

  -- Ensure there's no space after the minus sign
  when (sign == -1) $ notFollowedBy space1

  -- Parse the number as rational
  n <- try scientificParser <|> try rationalParser <|> integerAsRationalParser

  -- Return a ComplexRational with zero imaginary part
  pure $ CR (sign * n) 0

scientificParser :: Parser Rational
scientificParser = do
  -- Parse the base number (either decimal or integer)
  base <- try rationalParser <|> integerAsRationalParser

  -- Parse the exponent part
  _ <- char 'e' <|> char 'E'
  expSign <- option 1 (-1 <$ char '-') <|> 1 <$ optional (char '+')
  expVal <- L.decimal

  -- Calculate the result: base * 10^exp
  let exp = expSign * expVal
  return $ base * 10 ^^ exp

data Expr
  = BinOp Op Expr Expr
  | UnOp UnaryOp Expr
  | Lit ComplexRational
  | Ref String SourcePos
  | Pipeline Expr Expr
  deriving (Show, Eq)

data Op = Add | Sub | Mul | Div | Pow
  deriving (Show, Eq)

data UnaryOp = Neg | Sqrt | Abs | OpAsCombinator Op
  deriving (Show, Eq)

complexOpEndingExpr :: Parser (Expr -> Expr)
complexOpEndingExpr = do
  e <- addExpr
  op <- Div <$ symbol "/" <|> Mul <$ symbol "*" <|> Add <$ symbol "+" <|> Sub <$ symbol "-"
  -- e goes LEFT, piped value goes RIGHT (to match original placeholder position)
  return $ \pipedValue -> BinOp op e pipedValue

-- Replace your pipelineOp with this:
pipelineOp :: Parser (Expr -> Expr) -- Returns a FUNCTION instead of Expr
pipelineOp =
  choice
    [ try prefixBinaryOp, -- /2 meaning x/2
      try postfixBinaryOp, -- 2/ meaning 2/x
      try incompleteOp, -- +2 meaning x+2
      try unaryPipeOp, -- sqrt meaning sqrt x
      (\x -> Pipeline x) <$> addExpr -- Default: keep as pipeline
    ]
  where
    -- Handle incomplete binary operations - NO PLACEHOLDERS!
    incompleteOp = do
      op <-
        choice
          [ Add <$ symbol "+",
            Sub <$ symbol "-",
            Mul <$ symbol "*",
            Div <$ symbol "/"
          ]
      expr <- addExpr
      -- Return FUNCTION that takes left operand
      pure $ \leftExpr -> BinOp op leftExpr expr

    -- Postfix operator (e.g., "2/") - NO PLACEHOLDERS!
    postfixBinaryOp = do
      val <- number
      op <-
        choice
          [ Div <$ symbol "/",
            Mul <$ symbol "*",
            Add <$ symbol "+",
            Sub <$ symbol "-"
          ]
      -- Return FUNCTION that takes right operand
      pure $ \rightExpr -> BinOp op (Lit val) rightExpr

    -- Prefix operator (e.g., "/2") - NO PLACEHOLDERS!
    prefixBinaryOp = do
      op <-
        choice
          [ Div <$ symbol "/",
            Mul <$ symbol "*",
            Add <$ symbol "+"
          ]
      val <- number
      -- Return FUNCTION that takes left operand
      pure $ \leftExpr -> BinOp op leftExpr (Lit val)

    -- Unary operators - NO PLACEHOLDERS!
    unaryPipeOp =
      choice
        [ (\x -> UnOp Sqrt x) <$ try (string "sqrt" <* notFollowedBy alphaNumChar),
          (\x -> UnOp Abs x) <$ try (string "abs" <* notFollowedBy alphaNumChar),
          (\x -> UnOp Neg x) <$ try (string "-" *> notFollowedBy (satisfy isDigit))
        ]

expr :: Parser Expr
expr = do
  initial <- addExpr
  pipelineFuncs <- many $ try $ do
    _ <- symbol "|>"
    sc
    pipelineOp -- Now returns (Expr -> Expr)
  pure $ foldl (\acc f -> f acc) initial pipelineFuncs

addExpr :: Parser Expr
addExpr = do
  initial <- mulExpr
  rest <- many $ try $ do
    op <- Add <$ symbol "+" <|> Sub <$ symbol "-"
    term <- mulExpr
    pure (op, term)
  pure $ foldl (\acc (op, term) -> BinOp op acc term) initial rest

mulExpr :: Parser Expr
mulExpr = do
  initial <- powExpr
  rest <- many $ try $ do
    op <- Mul <$ symbol "*" <|> Div <$ symbol "/"
    term' <- term
    pure (op, term')
  pure $ foldl (\acc (op, term') -> BinOp op acc term') initial rest

powExpr :: Parser Expr
powExpr = do
  initial <- term
  rest <- many $ try $ do
    op <- Pow <$ symbol "^"
    term' <- powExpr
    pure (op, term')
  pure $ foldl (\acc (op, term') -> BinOp op acc term') initial rest

term :: Parser Expr
term =
  choice
    [ UnOp Sqrt <$> (try (symbol "sqrt" <* notFollowedBy alphaNumChar) *> term),
      UnOp Abs <$> (try (symbol "abs" <* notFollowedBy alphaNumChar) *> term),
      Lit <$> number,
      do
        pos <- getSourcePos
        name <- identifier
        return $ Ref name pos
    ]

data TopLevel = NamedValue String Expr
  deriving (Show, Eq)

topLevel :: Parser TopLevel
topLevel = do
  name <- identifier
  _ <- symbol "="
  NamedValue name <$> expr

program :: Parser [TopLevel]
program = between sc eof (sepEndBy1 topLevel sc)

parseProgram :: String -> Either (ParseErrorBundle String Void) [TopLevel]
parseProgram = parse program ""
