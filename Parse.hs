{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseProgram,
    Expr (..), -- (..) exports all constructors
    TopLevel (..),
    Op (..),
    UnaryOp (..),
    test,
  )
where

import ComplexRational (ComplexRational (CR))
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Complex (Complex (..))
import Data.Functor (void, ($>))
import Data.Ratio ((%))
import Data.Void (Void)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Property,
    Testable (property),
    choose,
    counterexample,
    elements,
    quickCheck,
    vectorOf,
    (==>),
  )
import Text.Megaparsec (MonadParsec (eof, try), ParseErrorBundle, Parsec, between, choice, many, option, optional, parse, satisfy, sepEndBy1, some, (<|>))
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

-- Parse a proper rational number (with decimal point)
rationalParser :: Parser Rational
rationalParser = do
  intPart <- L.decimal
  _ <- char '.'
  fractionalStr <- some (satisfy isDigit)
  let fractionalPart = read fractionalStr :: Integer
  let denominator = 10 ^ length fractionalStr

  -- Combine integer and fractional parts
  return $ (intPart % 1) + (fractionalPart % denominator)

integerAsRationalParser :: Parser Rational
integerAsRationalParser = do
  n <- L.decimal
  return $ n % 1

-- Parse a number as ComplexRational
number :: Parser ComplexRational
number = lexeme $ do
  sign <- option 1 (-1 <$ char '-') -- Parse optional minus sign

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
  expSign <- option 1 (-1 <$ char '-') <|> (1 <$ optional (char '+'))
  expVal <- L.decimal

  -- Calculate the result: base * 10^exp
  let exp = expSign * expVal
  return $ base * (10 ^^ exp)

data Expr
  = BinOp Op Expr Expr
  | UnOp UnaryOp Expr
  | Lit ComplexRational
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
      pure $ BinOp op (Lit val) (Lit (CR 0 0))

    postfixBinaryOp = try postfixWithNegative <|> postfixNormal
      where
        postfixWithNegative = do
          _ <- char '-'
          _ <- sc -- handle whitespace after minus sign
          n <- try L.float <|> fromIntegral <$> L.decimal
          _ <- sc -- handle whitespace before operator
          op <- (Div <$ symbol "/") <|> (Mul <$ symbol "*") <|> (Add <$ symbol "+") <|> (Sub <$ symbol "-")
          pure $ BinOp op (Lit (CR (toRational (-n)) 0)) (Lit (CR 0 0))

        postfixNormal = do
          n <- number
          op <- (Div <$ symbol "/") <|> (Mul <$ symbol "*") <|> (Add <$ symbol "+") <|> (Sub <$ symbol "-")
          pure $ BinOp op (Lit n) (Lit (CR 0 0))

    prefixBinaryOp = do
      op <- (Div <$ symbol "/") <|> (Mul <$ symbol "*") <|> (Add <$ symbol "+") <|> (Sub <$ symbol "-")
      BinOp op (Lit (CR 0 0)) . Lit <$> number

    unaryPipeOp =
      choice
        [ UnOp Sqrt <$> (symbol "sqrt" $> Lit (CR 0 0)),
          UnOp Abs <$> (symbol "abs" $> Lit (CR 0 0))
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
    makePipeline pipedVal (BinOp op (Lit (CR 0 0)) right) =
      BinOp op pipedVal right
    makePipeline pipedVal (BinOp op left (Lit (CR 0 0))) =
      BinOp op left pipedVal
    makePipeline pipedVal (UnOp uop (Lit (CR 0 0))) =
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

---------- Tests

-- Generate valid identifier names
newtype ValidName = ValidName String
  deriving (Show)

instance Arbitrary ValidName where
  arbitrary = do
    firstChar <- elements ['a' .. 'z']
    restLength <- choose (0, 10)
    rest <- vectorOf restLength $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    return $ ValidName (firstChar : rest)

-- Pipeline equivalence properties
prop_pipelineDivEquiv :: ValidName -> Double -> Property
prop_pipelineDivEquiv (ValidName x) n =
  n
    /= 0
      ==> let expr1 = x ++ " |> /" ++ show n
              expr2 = x ++ " / " ++ show n
           in parseProgram (makeAssignment "result" expr1) == parseProgram (makeAssignment "result" expr2)

prop_pipelinePostDivEquiv :: ValidName -> Double -> Property
prop_pipelinePostDivEquiv (ValidName x) n =
  n
    /= 0
      ==> let expr1 = x ++ " |> " ++ show n ++ "/"
              expr2 = show n ++ " / " ++ x
           in parseProgram (makeAssignment "result" expr1) == parseProgram (makeAssignment "result" expr2)

-- Correct property test for whitespace invariance
prop_whitespaceInvariance :: ValidName -> Double -> Double -> Property
prop_whitespaceInvariance (ValidName x) n1 n2 =
  let -- The different spacing patterns to test
      spacings = [" ", "\t", "\n", " \n ", "\t\n\t"]

      -- Create a proper expression using the identifier as a variable name
      -- and then use n1 and n2 in a simple addition expression
      makeExpr :: String -> String
      makeExpr spaces = x ++ spaces ++ "=" ++ spaces ++ show n1 ++ spaces ++ "+" ++ spaces ++ show n2

      -- The expressions to parse (without nesting them in another assignment)
      expressions = map makeExpr spacings

      -- Parse each expression
      results = map parseProgram expressions

      -- Show exact output values for debugging
      showParsed :: String -> Either (ParseErrorBundle String Void) [TopLevel] -> String
      showParsed expr result = "Input: \"" ++ expr ++ "\"\nParsed as: " ++ show result ++ "\n"

      -- Create detailed debug output
      debugInfo = unlines $ zipWith showParsed expressions results

      -- Check if all results are equal to the first result
      allEqual = all (== head results) (tail results)
   in counterexample
        ( "Expressions with different whitespace produced different results:\n"
            ++ "Identifier: "
            ++ show x
            ++ "\n"
            ++ "Number 1: "
            ++ show n1
            ++ "\n"
            ++ "Number 2: "
            ++ show n2
            ++ "\n"
            ++ debugInfo
        )
        allEqual

-- Helper to make a complete assignment
makeAssignment :: String -> String -> String
makeAssignment name expr = name ++ " = " ++ expr

-- Main property test function
test :: IO ()
test = do
  putStrLn "Testing pipeline division equivalence..."
  quickCheck prop_pipelineDivEquiv
  putStrLn "Testing pipeline postfix division equivalence..."
  quickCheck prop_pipelinePostDivEquiv
  putStrLn "Testing whitespace invariance..."
  quickCheck prop_whitespaceInvariance
