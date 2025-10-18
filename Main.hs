{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Data.Biapplicative (Bifunctor (first))
import Data.Map qualified as Map
import Eval (evalProgram)
import NameValidator (validateProgram)
import Ops (from)
import Parse (parseProgram)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

evalFile :: FilePath -> IO ()
evalFile filename = do
  contents <- readFile filename

  let result' = do
        ast <- first errorBundlePretty $ parseProgram contents
        _ <- first ("Validation error: " ++) $ validateProgram ast
        first ("Evaluation error: " ++) $ evalProgram ast

  case result' of
    Left err -> putStrLn err
    Right env -> do
      putStrLn "Evaluated program:"
      mapM_ printBinding $ Map.toList env
  where
    printBinding (k, v) =
      putStrLn $
        k ++ " = " ++ case from v of
          Right (val :: Double) -> show val
          Left err -> "Error: " ++ err

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> evalFile filename
    _ -> putStrLn "Usage: program <filename>  "
