module Main (main) where

import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Void (Void)
import Eval (evalProgram)
import NameValidator (validateProgram)
import Ops (from)
import Parse (Expr, TopLevel, parseProgram)
import System.Environment (getArgs)
import ErrorReporting (prettyParseError)
import Text.Megaparsec ( errorBundlePretty )


evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename
  case parseProgram contents of
    Left err -> putStrLn $ errorBundlePretty err 
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
                    putStr $ k ++ " = " ++ 
                      case (from v :: Either String Double) of
                        Right val -> show val
                        Left err -> "Error: " ++ err
                )
                $ Map.toList env


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> evalFile filename
    -- Here you could continue with evaluation
    _ -> putStrLn "Usage: program <filename>  "
