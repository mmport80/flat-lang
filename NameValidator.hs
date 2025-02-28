module NameValidator where

import Data.Set (Set)
import Data.Set qualified as Set
import Parse (Expr (..), TopLevel (..))

-- Validation result type
type ValidationError = String

type ValidationResult = Either ValidationError ()

-- Validate all name references in a program
validateProgram :: [TopLevel] -> ValidationResult
validateProgram topLevels = validate topLevels Set.empty
  where
    validate [] _ = Right ()
    validate (NamedValue name expr : rest) definedNames = do
      -- Check if name is already defined
      if Set.member name definedNames
        then Left $ "Name '" ++ name ++ "' is defined multiple times"
        else do
          -- Validate references in this expression
          validateExpr expr definedNames
          -- Continue with rest of program, adding this name to defined set
          validate rest (Set.insert name definedNames)

-- Validate references in an expression
validateExpr :: Expr -> Set String -> ValidationResult
validateExpr expr definedNames = case expr of
  BinOp _ e1 e2 -> do
    validateExpr e1 definedNames
    validateExpr e2 definedNames
  UnOp _ e -> validateExpr e definedNames
  Lit _ -> Right ()
  Ref name ->
    if Set.member name definedNames
      then Right ()
      else Left $ "Reference to undefined name '" ++ name ++ "'"
  Pipeline e1 e2 -> do
    validateExpr e1 definedNames
    validateExpr e2 definedNames