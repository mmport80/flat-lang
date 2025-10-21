{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseProgram,
    Expr (..), -- (..) exports all constructors
    TopLevel (..),
    Op (..),
    UnaryOp (..),
    test,
    testDebug,
    testPipelineWithAssignment,
  )
where
 
import ComplexRational (ComplexRational (CR))
import Control.Monad (foldM, void, when)
import Data.Char (isDigit)
import Data.Complex (Complex (..))
import Data.Either (isRight)
import Data.Functor (void, ($>))
import Data.Ratio ((%))
import Data.Void (Void)
import Test.QuickCheck (Arbitrary (arbitrary), Property, Testable (property), choose, counterexample, elements, forAll, frequency, oneof, quickCheck, sized, vectorOf, withMaxSuccess, (==>))
import Test.QuickCheck.Gen (Gen)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec, between, choice, many, option, optional, parse, satisfy, sepEndBy1, some, (<|>), SourcePos (sourceName, sourceColumn, sourceLine, SourcePos), getSourcePos, mkPos)
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
pipelineOp :: Parser (Expr -> Expr)  -- Returns a FUNCTION instead of Expr
pipelineOp =
  choice
    [ 
      try prefixBinaryOp,     -- /2 meaning x/2
      try postfixBinaryOp,    -- 2/ meaning 2/x  
      try incompleteOp,       -- +2 meaning x+2
      try unaryPipeOp,        -- sqrt meaning sqrt x
      (\x -> Pipeline x) <$> addExpr  -- Default: keep as pipeline
    ]
  where
    -- Handle incomplete binary operations - NO PLACEHOLDERS!
    incompleteOp = do
      op <- choice [Add <$ symbol "+", Sub <$ symbol "-", 
                   Mul <$ symbol "*", Div <$ symbol "/"]
      expr <- addExpr
      -- Return FUNCTION that takes left operand
      pure $ \leftExpr -> BinOp op leftExpr expr
      
    -- Postfix operator (e.g., "2/") - NO PLACEHOLDERS!
    postfixBinaryOp = do
      val <- number
      op <- choice [Div <$ symbol "/", Mul <$ symbol "*",
                   Add <$ symbol "+", Sub <$ symbol "-"]
      -- Return FUNCTION that takes right operand  
      pure $ \rightExpr -> BinOp op (Lit val) rightExpr
      
    -- Prefix operator (e.g., "/2") - NO PLACEHOLDERS!
    prefixBinaryOp = do
      op <- choice [Div <$ symbol "/", Mul <$ symbol "*",
                   Add <$ symbol "+"]
      val <- number
      -- Return FUNCTION that takes left operand
      pure $ \leftExpr -> BinOp op leftExpr (Lit val)
      
    -- Unary operators - NO PLACEHOLDERS!
    unaryPipeOp = choice
      [
        (\x -> UnOp Sqrt x) <$ try (string "sqrt" <* notFollowedBy alphaNumChar),
        (\x -> UnOp Abs x) <$ try (string "abs" <* notFollowedBy alphaNumChar),
        (\x -> UnOp Neg x) <$ try (string "-" *> notFollowedBy (satisfy isDigit))
      ]
      

-- Update your expr function:
expr :: Parser Expr
expr = do
  initial <- addExpr
  pipelineFuncs <- many $ try $ do
    _ <- symbol "|>"
    sc
    pipelineOp  -- Now returns (Expr -> Expr)
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
    [ --between (symbol "(") (symbol ")") expr,
      UnOp Sqrt <$> (try (symbol "sqrt" <* notFollowedBy alphaNumChar) *> term),
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

---------- Helper functions for tests

-- Dummy SourcePos for comparing expressions
dummyPos :: SourcePos
dummyPos = SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}

-- Strip SourcePos from all Ref nodes for comparison
stripSourcePos :: Expr -> Expr
stripSourcePos (BinOp op e1 e2) = BinOp op (stripSourcePos e1) (stripSourcePos e2)
stripSourcePos (UnOp op e) = UnOp op (stripSourcePos e)
stripSourcePos (Lit c) = Lit c
stripSourcePos (Ref name _) = Ref name dummyPos
stripSourcePos (Pipeline e1 e2) = Pipeline (stripSourcePos e1) (stripSourcePos e2)

{- -- Pipeline equivalence properties
prop_pipelineDivEquiv :: ValidName -> Double -> Property
prop_pipelineDivEquiv (ValidName x) n =
  n
    /= 0
      ==> let expr1 = x ++ " |> /" ++ show n
              expr2 = x ++ " / " ++ show n
           in parseProgram (makeAssignment "result" expr1) == parseProgram (makeAssignment "result" expr2)
 -}
prop_pipelinePostDivEquiv :: ValidName -> Double -> Property
prop_pipelinePostDivEquiv (ValidName x) n =
  n
    /= 0
      ==> let expr1 = x ++ " |> " ++ show n ++ "/"
              expr2 = show n ++ " / " ++ x
              result1 = parseProgram (makeAssignment "result" expr1)
              result2 = parseProgram (makeAssignment "result" expr2)
              -- Extract and compare expressions after stripping SourcePos
              extractExpr (Right [NamedValue _ e]) = Just e
              extractExpr _ = Nothing
              compareExprs = case (extractExpr result1, extractExpr result2) of
                (Just e1, Just e2) -> stripSourcePos e1 == stripSourcePos e2
                _ -> result1 == result2
           in counterexample
                ( "Expr1: " ++ show result1 ++ "\nExpr2: " ++ show result2 )
                compareExprs

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

-- Property test for prefix operations in pipelines (e.g., "x |> +n")
prop_pipelinePrefixOp :: ValidName -> Double -> Double -> Property
prop_pipelinePrefixOp (ValidName x) a b =
  b /= 0 ==> isRight $
    parseProgram $
      makeAssignment "result" $
        x ++ " |> +" ++ show a ++ " |> *" ++ show b

-- Property test for postfix operations in pipelines (e.g., "x |> n+")
prop_pipelinePostfixOp :: ValidName -> Double -> Double -> Property
prop_pipelinePostfixOp (ValidName x) a b =
  b /= 0 ==> isRight $
    parseProgram $
      makeAssignment "result" $
        x ++ " |> " ++ show a ++ "+ |> " ++ show b ++ "*"

-- Property test for mixed operations in pipelines
prop_pipelineMixedOps :: ValidName -> Double -> Double -> Property
prop_pipelineMixedOps (ValidName x) a b =
  b /= 0 ==> isRight $
    parseProgram $
      makeAssignment "result" $
        x ++ " |> +" ++ show a ++ " |> " ++ show b ++ "/"

-- Property test for multi-step pipelines
prop_pipelineMultiStep :: ValidName -> Double -> Double -> Double -> Property
prop_pipelineMultiStep (ValidName x) a b c =
  b /= 0 && c /= 0 ==> isRight $
    parseProgram $
      makeAssignment "result" $
        x ++ " |> +" ++ show a ++ " |> /" ++ show b ++ " |> *" ++ show c

-- Property test for unary operations in pipelines
prop_pipelineUnaryOp :: ValidName -> Property
prop_pipelineUnaryOp (ValidName x) =
  property $
    isRight $
      parseProgram $
        makeAssignment "result" $
          x ++ " |> sqrt |> *2"

-- Property test for multi-stage pipelines
prop_pipelineNestedExpr :: ValidName -> Double -> Double -> Property
prop_pipelineNestedExpr (ValidName x) a b =
  a /= 0 && b /= 0 ==> isRight $
    parseProgram $
      makeAssignment "result" $
        x ++ " |> *" ++ show a ++ " |> /" ++ show b ++ " |> +" ++ show a

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

  putStrLn "Testing pipeline prefix operations..."
  quickCheck prop_pipelinePrefixOp

  putStrLn "Testing pipeline postfix operations..."
  quickCheck prop_pipelinePostfixOp

  putStrLn "Testing mixed pipeline operations..."
  quickCheck prop_pipelineMixedOps

  putStrLn "Testing multi-step pipelines..."
  quickCheck prop_pipelineMultiStep

  {-   putStrLn "Testing unary operations in pipelines..."
    quickCheck prop_pipelineUnaryOp
   -}
  putStrLn "Testing nested expressions in pipelines..."
  quickCheck prop_pipelineNestedExpr

-- Debugging function to examine parse results for pipeline division
debugPipelineDivEquiv :: String -> Double -> IO ()
debugPipelineDivEquiv varName n = do
  let expr1 = varName ++ " |> /" ++ show n
      expr2 = varName ++ " / " ++ show n
      assignment1 = makeAssignment "result" expr1
      assignment2 = makeAssignment "result" expr2
      result1 = parseProgram assignment1
      result2 = parseProgram assignment2

  putStrLn "Testing equivalence of:"
  putStrLn $ "  1) " ++ assignment1
  putStrLn $ "  2) " ++ assignment2
  putStrLn ""

  putStrLn $ "ParseResult 1: " ++ show result1
  putStrLn $ "ParseResult 2: " ++ show result2
  putStrLn ""

  case (result1, result2) of
    (Right [NamedValue _ pipeExpr], Right [NamedValue _ divExpr]) -> do
      putStrLn $ "Pipeline expression structure: " ++ showExprStructure pipeExpr
      putStrLn $ "Division expression structure: " ++ showExprStructure divExpr

      -- Now check literals specifically
      inspectLiterals pipeExpr
      inspectLiterals divExpr
    _ ->
      putStrLn "Failed to parse both expressions"

-- Helper to show expression structure without full details of literals
showExprStructure :: Expr -> String
showExprStructure (BinOp op e1 e2) =
  "BinOp " ++ show op ++ " (" ++ showExprStructure e1 ++ ") (" ++ showExprStructure e2 ++ ")"
showExprStructure (UnOp op e) =
  "UnOp " ++ show op ++ " (" ++ showExprStructure e ++ ")"
showExprStructure (Lit _) = "Lit <value>" -- Abstract away the literal details
showExprStructure (Ref name _) = "Ref " ++ name
showExprStructure (Pipeline e1 e2) =
  "Pipeline (" ++ showExprStructure e1 ++ ") (" ++ showExprStructure e2 ++ ")"

-- Helper to specifically inspect literal values in expressions
inspectLiterals :: Expr -> IO ()
inspectLiterals expr = do
  let literals = collectLiterals expr
  putStrLn $ "Literal values in expression: " ++ show literals

-- Collect all literal values from an expression
collectLiterals :: Expr -> [ComplexRational]
collectLiterals (Lit c) = [c]
collectLiterals (BinOp _ e1 e2) = collectLiterals e1 ++ collectLiterals e2
collectLiterals (UnOp _ e) = collectLiterals e
collectLiterals (Pipeline e1 e2) = collectLiterals e1 ++ collectLiterals e2
collectLiterals _ = []

-- Example usage in your test function
testDebug :: IO ()
testDebug = do
  -- Test with the failing case
  debugPipelineDivEquiv "t5rzlqaha" 0.1

  -- Also test with some other values for comparison
  debugPipelineDivEquiv "x" 2.0
  debugPipelineDivEquiv "y" 1

  -- Test specifically with values that might be problematic for floating-point
  debugPipelineDivEquiv "z" 0.3 -- 0.3 has no exact binary representation

-- Modify the test to use actual parsed expressions instead of constructing strings
-- First, let's create a function to manually parse expressions
parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (between sc eof expr) ""

-- Now update the property test to use this function
prop_pipelineDivEquiv :: ValidName -> Double -> Property
prop_pipelineDivEquiv (ValidName x) n =
  n
    /= 0
      ==> let
              -- Parse expressions directly using the expr parser
              expr1 = parseExpr (x ++ " |> /" ++ show n)
              expr2 = parseExpr (x ++ " / " ++ show n)
           in -- Compare the results with detailed error message
              counterexample
                ( "Expr1 ("
                    ++ x
                    ++ " |> /"
                    ++ show n
                    ++ "): "
                    ++ show expr1
                    ++ "\nExpr2 ("
                    ++ x
                    ++ " / "
                    ++ show n
                    ++ "): "
                    ++ show expr2
                )
                (expr1 == expr2)

testPipelineWithAssignment :: IO ()
testPipelineWithAssignment = do
  let testCases =
        [ "result = x |> /0.1", -- Original failing test
          "result = x / 0.1", -- Regular division for comparison
          "a = 1 |> + 2", -- Basic prefix operation
          "b = a |> 2 -", -- Basic postfix operation
          "c = 1 / 1 |> 1 + 1 /", -- Complex expression
          "d = c |> 2 / 2 + |> 0 /", -- Chained pipelines
          "e = 2 ^ 2",
          "f = -1 ^ 2"
        ]

  putStrLn "=== Testing Complete Pipeline Statements ==="
  mapM_ testProgram testCases
  where
    testProgram :: String -> IO ()
    testProgram input = do
      putStrLn $ "\nInput: " ++ input
      case parseProgram input of
        Left err -> putStrLn $ "Error: " ++ errorBundlePretty err
        Right result -> do
          putStrLn "Success!"
          putStrLn $ "AST: " ++ show result

{-
-- Arbitrary instance for Op
instance Arbitrary Op where
  arbitrary = elements [Add, Sub, Mul, Div]

-- Arbitrary instance for UnaryOp
instance Arbitrary UnaryOp where
  arbitrary = elements [Sqrt, Abs]

instance Arbitrary Expr where
  arbitrary = sized genExpr
    where
      genExpr :: Int -> Gen Expr
      genExpr 0 =
        oneof
          [ genLiteral,
            Ref <$> genIdent
          ]
      genExpr n
        | n > 0 =
            frequency
              [ (3, genExpr 0),
                ( 2,
                  do
                    left <- genExpr (n `div` 2)
                    right <- genExpr (n `div` 2)
                    op <- arbitrary
                    return $ BinOp op left right
                ),
                ( 1,
                  do
                    expr <- genExpr (n - 1)
                    op <- elements [Sqrt, Abs]
                    return $ UnOp op expr
                ),
                (3, genComplexPipeline n)
              ]

      -- Generate pipelines with valid stages
      genComplexPipeline :: Int -> Gen Expr
      genComplexPipeline n = do
        initial <- genExpr (n `div` 2)
        numStages <- choose (1, min 3 n)
        foldM addPipelineStage initial [1 .. numStages]

      -- Generate a valid pipeline stage (key fix is here)
      addPipelineStage :: Expr -> Int -> Gen Expr
      addPipelineStage expr _ = do
        -- Generate only valid pipeline stages for Flat Lang
        stage <-
          oneof
            [ -- References are always safe
              Ref <$> genIdent,
              -- Operator with placeholder - safe for pipelines
              do
                op <- arbitrary
                val <- genExpr 0
                return $ BinOp op (Lit (CR 0 0)) val, -- Like /5

              -- Standalone unary op (no argument) - this is key fix
              -- abs means "apply abs to piped value" not "abs followed by something"
              do
                uop <- elements [Sqrt, Abs]
                return $ UnOp uop (Lit (CR 0 0)) -- Like just "abs"
            ]

        return $ Pipeline expr stage

      -- Generate a literal
      genLiteral :: Gen Expr
      genLiteral = do
        -- Avoid exact zeros to prevent parser edge cases
        n <- oneof [choose (-10.0, -0.001), choose (0.001, 10.0)] :: Gen Double
        return $ Lit (CR (toRational n) 0)

      -- Generate an identifier
      genIdent :: Gen String
      genIdent = do
        c <- elements ['a' .. 'z']
        len <- choose (1, 5)
        cs <- vectorOf len $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
        return (c : cs)

-- Helper comparison function for tests
exprApproxEqual :: Expr -> Expr -> Bool
exprApproxEqual (Lit (CR r1 i1)) (Lit (CR r2 i2)) =
  abs (fromRational r1 - fromRational r2) < 1e-10
    && abs (fromRational i1 - fromRational i2) < 1e-10
exprApproxEqual (Ref n1) (Ref n2) = n1 == n2
exprApproxEqual (BinOp op1 l1 r1) (BinOp op2 l2 r2) =
  op1 == op2 && exprApproxEqual l1 l2 && exprApproxEqual r1 r2
exprApproxEqual (UnOp op1 e1) (UnOp op2 e2) =
  op1 == op2 && exprApproxEqual e1 e2
exprApproxEqual (Pipeline l1 r1) (Pipeline l2 r2) =
  exprApproxEqual l1 l2 && exprApproxEqual r1 r2
exprApproxEqual _ _ = False
 -}
{- -- Add this function to Parse.hs for property tests
showExpr :: Expr -> String
showExpr (Lit (CR r _)) =
  -- Format rational as floating point, avoiding % syntax
  show (fromRational r :: Double)
showExpr (Ref name) = name
showExpr (BinOp Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (BinOp Sub e1 e2) = showExpr e1 ++ " - " ++ showExpr e2
showExpr (BinOp Mul e1 e2) = showExpr e1 ++ " * " ++ showExpr e2
showExpr (BinOp Div e1 e2) = showExpr e1 ++ " / " ++ showExpr e2
showExpr (UnOp Neg e) = "-" ++ showExpr e
showExpr (UnOp Sqrt e) = "sqrt " ++ showExpr e
showExpr (UnOp Abs e) = "abs " ++ showExpr e
showExpr (Pipeline e1 e2) = showExpr e1 ++ " |> " ++ showExpr e2
 -}
{- -- Test property using approximate equality
prop_roundTripParsing :: Property
prop_roundTripParsing = forAll (sized genExpr) $ \expr ->
  let exprStr = showExpr expr
   in case parseExpr exprStr of
        Right parsed ->
          counterexample
            ("Original: " ++ exprStr ++ "\nParsed as: " ++ show parsed)
            (exprApproxEqual expr parsed)
        Left err ->
          counterexample
            ("Failed to parse: " ++ exprStr ++ "\nError: " ++ errorBundlePretty err)
            False
  where
    genExpr :: Int -> Gen Expr
    genExpr n = arbitrary

-- Add these to your test function
testArbitraryExpressions :: IO ()
testArbitraryExpressions = do
  putStrLn "Testing round-trip parsing of expressions..."
  quickCheck (withMaxSuccess 100 prop_roundTripParsing) -}
