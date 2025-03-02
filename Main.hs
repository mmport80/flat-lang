module Main (main, runParserTests) where

import Data.List (intercalate)
-- Replace the existing whitespace invariance test with this one
-- Import required modules if not already imported

-- In Main.hs, add:

import Data.Map qualified as Map
import Data.Void (Void)
import Eval (evalProgram)
import NameValidator (validateProgram)
import Ops (from)
import Parse (Expr, TopLevel, parseProgram)
import System.Environment (getArgs)
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
import Text.Megaparsec (ParseErrorBundle)

evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename
  case parseProgram contents of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast ->
      case validateProgram ast of
        Left err -> putStrLn $ "Validation error: " ++ err
        Right () ->
          case evalProgram ast of
            Left err -> putStrLn $ "Evaluation error: " ++ err
            Right env -> do
              putStrLn "Evaluated program:"
              mapM_
                ( \(k, v) -> do
                    putStr $ k ++ " = "
                    case from v of
                      Right val -> print val
                      Left err -> putStrLn $ "Error: " ++ err
                )
                $ Map.toList env

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
runParserTests :: IO ()
runParserTests = do
  putStrLn "Testing pipeline division equivalence..."
  quickCheck prop_pipelineDivEquiv
  putStrLn "Testing pipeline postfix division equivalence..."
  quickCheck prop_pipelinePostDivEquiv
  putStrLn "Testing whitespace invariance..."
  quickCheck prop_whitespaceInvariance

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      case parseProgram contents of
        Left parseError ->
          putStrLn $ "Parse error: " ++ show parseError
        Right ast ->
          case validateProgram ast of
            Left validationError ->
              putStrLn $ "Validation error: " ++ validationError
            Right () -> do
              putStrLn "Program is valid!"
    -- Here you could continue with evaluation
    _ -> putStrLn "Usage: program <filename>  "