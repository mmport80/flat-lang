{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseProgram,
    Expr (..), -- (..) exports all constructors
    TopLevel (..),
    Op (..),
    UnaryOp (..),
  )
where

import Control.Monad (void)
import Data.Complex (Complex (..))
import Data.Functor (void, ($>))
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    many,
    option,
    parse,
    sepEndBy1,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L

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

number :: Parser (Complex Double)
number = lexeme $ do
  sign <- option 1 (-1 <$ char '-') -- Parse optional minus sign
  n <- try L.float <|> fromIntegral <$> L.decimal
  pure (sign * n :+ 0)

data Expr
  = BinOp Op Expr Expr
  | UnOp UnaryOp Expr
  | Lit (Complex Double)
  | Ref String
  | Pipeline Expr Expr
  deriving (Show, Eq)

data Op = Add | Sub | Mul | Div
  deriving (Show, Eq)

data UnaryOp = Neg | Sqrt | Abs | OpAsCombinator Op
  deriving (Show, Eq)

pipelineOp :: Parser Expr
pipelineOp =
  choice
    [ try incompleteOp,
      try prefixBinaryOp, -- /2 means x/2
      try postfixBinaryOp, -- 2/ means 2/x
      try unaryPipeOp, -- sqrt means sqrt x
      addExpr -- fallback to normal expression
    ]
  where
    -- Handle incomplete binary operations
    incompleteOp = do
      val <- try number
      op <-
        (Add <$ symbol "+")
          <|> (Sub <$ symbol "-")
          <|> (Mul <$ symbol "*")
          <|> (Div <$ symbol "/")
      -- Return a binary operation with a placeholder for the right operand
      pure $ BinOp op (Lit val) (Lit (0 :+ 0))

    postfixBinaryOp = try postfixWithNegative <|> postfixNormal
      where
        postfixWithNegative = do
          _ <- char '-'
          _ <- sc -- handle whitespace after minus sign
          n <- try L.float <|> fromIntegral <$> L.decimal
          _ <- sc -- handle whitespace before operator
          op <- (Div <$ symbol "/") <|> (Mul <$ symbol "*") <|> (Add <$ symbol "+") <|> (Sub <$ symbol "-")
          pure $ BinOp op (Lit ((-n) :+ 0)) (Lit (0 :+ 0))

        postfixNormal = do
          n <- number
          op <- (Div <$ symbol "/") <|> (Mul <$ symbol "*") <|> (Add <$ symbol "+") <|> (Sub <$ symbol "-")
          pure $ BinOp op (Lit n) (Lit (0 :+ 0))

    prefixBinaryOp = do
      op <- (Div <$ symbol "/") <|> (Mul <$ symbol "*") <|> (Add <$ symbol "+") <|> (Sub <$ symbol "-")
      BinOp op (Lit (0 :+ 0)) . Lit <$> number

    unaryPipeOp =
      choice
        [ UnOp Sqrt <$> (symbol "sqrt" $> Lit (0 :+ 0)),
          UnOp Abs <$> (symbol "abs" $> Lit (0 :+ 0))
        ]

expr :: Parser Expr
expr = do
  initial <- addExpr
  rest <- many $ try $ do
    _ <- symbol "|>"
    pipelineOp
  pure $ foldl makePipeline initial rest
  where
    -- Replace placeholder zeros with the piped value
    makePipeline :: Expr -> Expr -> Expr
    makePipeline pipedVal (BinOp op (Lit (0 :+ 0)) right) =
      BinOp op pipedVal right
    makePipeline pipedVal (BinOp op left (Lit (0 :+ 0))) =
      BinOp op left pipedVal
    makePipeline pipedVal (UnOp uop (Lit (0 :+ 0))) =
      UnOp uop pipedVal
    makePipeline pipedVal expr =
      Pipeline pipedVal expr

addExpr :: Parser Expr
addExpr = do
  initial <- mulExpr
  rest <- many $ try $ do
    op <- (Add <$ symbol "+") <|> (Sub <$ symbol "-")
    term <- mulExpr
    pure (op, term)
  pure $ foldl (\acc (op, term) -> BinOp op acc term) initial rest

mulExpr :: Parser Expr
mulExpr = do
  initial <- term
  rest <- many $ try $ do
    op <- (Mul <$ symbol "*") <|> (Div <$ symbol "/")
    term' <- term
    pure (op, term')
  pure $ foldl (\acc (op, term') -> BinOp op acc term') initial rest

term :: Parser Expr
term =
  choice
    [ between (symbol "(") (symbol ")") expr,
      UnOp Sqrt <$> (symbol "sqrt" *> term),
      UnOp Abs <$> (symbol "abs" *> term),
      Lit <$> number,
      Ref <$> identifier
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