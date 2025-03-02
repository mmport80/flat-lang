module Eval
  ( evalProgram,
    evalExpr,
    EvalEnv,
    EvalResult,
  )
where

import Data.Complex (Complex ((:+)), realPart)
import Data.Map (Map)
import Data.Map qualified as Map
import Ops (Cell (..), abs', from, negate', sqrt', to, (⊕), (⊖), (⊗), (⊘))
import Parse (Expr (..), Op (..), TopLevel (..), UnaryOp (..))

-- Type aliases for clarity
type EvalEnv = Map String (Cell Double)

type EvalResult = Either String (Cell Double)

-- Evaluate a full program, returning the final environment
evalProgram :: [TopLevel] -> Either String EvalEnv
evalProgram = foldl evalDef (Right Map.empty)
  where
    evalDef :: Either String EvalEnv -> TopLevel -> Either String EvalEnv
    evalDef (Left err) _ = Left err
    evalDef (Right env) (NamedValue name expr) = do
      value <- evalExpr env expr
      Right $ Map.insert name value env

-- Evaluate a single expression in the given environment
evalExpr :: EvalEnv -> Expr -> EvalResult
evalExpr env expr = case expr of
  -- Handle literal values (convert to Cell)
  Lit val -> Right $ to (realPart val)
  -- Look up variable references
  Ref name -> case Map.lookup name env of
    Just value -> Right value
    Nothing -> Left $ "Undefined reference: " ++ name
  -- Handle binary operations
  BinOp op e1 e2 -> do
    v1 <- evalExpr env e1
    v2 <- evalExpr env e2
    Right $ applyBinOp op v1 v2

  -- Handle unary operations
  UnOp uop e -> do
    v <- evalExpr env e
    Right $ applyUnaryOp uop v

  -- Handle pipeline operator
  Pipeline e1 e2 -> do
    v1 <- evalExpr env e1
    case e2 of
      -- If right side is binary op with placeholder zero, replace with v1
      BinOp op (Lit (0 :+ 0)) right -> do
        rightVal <- evalExpr env right
        Right $ applyBinOp op v1 rightVal
      BinOp op left (Lit (0 :+ 0)) -> do
        leftVal <- evalExpr env left
        Right $ applyBinOp op leftVal v1
      -- If right side is unary op with placeholder zero, apply to v1
      UnOp uop (Lit (0 :+ 0)) ->
        Right $ applyUnaryOp uop v1
      -- Otherwise treat as function application
      _ -> do
        -- Create temp env with piped value available as special var
        let pipeEnv = Map.insert "_" v1 env
        evalExpr pipeEnv e2

-- Apply binary operations using Ops.hs operations
applyBinOp :: Op -> Cell Double -> Cell Double -> Cell Double
applyBinOp Add = (⊕)
applyBinOp Sub = (⊖)
applyBinOp Mul = (⊗)
applyBinOp Div = (⊘)

-- Apply unary operations using Ops.hs operations
applyUnaryOp :: UnaryOp -> Cell Double -> Cell Double
applyUnaryOp Neg = negate'
applyUnaryOp Sqrt = sqrt'
applyUnaryOp Abs = abs'
applyUnaryOp (OpAsCombinator op) = \x -> to 0 -- Placeholder for combinators