{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ops (Cell (..), abs', from, negate', sqrt', to, (⊕), (⊖), (⊗), (⊗⊗), (⊘), (=~), sqrtCR, Operation (..)) where

import ComplexRational (ComplexRational (..), addCR, divCR, magnitudeCR, maybePowCR, mulCR, sqrtCR, subCR)
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Either (isLeft, isRight)
import Data.Function (on, (&))
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio (Rational, denominator, numerator, (%))
import Data.Sequence qualified as Seq
import GHC.Base (when)
import GHC.IO (unsafePerformIO)
import Test.QuickCheck (Arbitrary, Property, quickCheck, (==>))
import TestUtils

-----------------------------------------

newtype Cell = Cell
  {operation :: Operation}

instance Eq Cell where
  -- Two cells are equal if their results are equal
  (Cell op1) == (Cell op2) = op1.result == op2.result

-- Update Operation to use this
data Operation = Operation
  { opName :: String,
    result :: Maybe ComplexRational,
    inputs :: [Operation]
  }

--
---'Fat' Ops

liftCell :: (Maybe ComplexRational -> Maybe ComplexRational -> Maybe ComplexRational) -> String -> Cell -> Cell -> Cell
liftCell f name (Cell op1) (Cell op2) =
  let newResult = f op1.result op2.result
   in Cell (Operation name newResult [op1, op2])

liftCellUnary :: (Maybe ComplexRational -> Maybe ComplexRational) -> String -> Cell -> Cell
liftCellUnary f name (Cell op1) =
  let newResult = f op1.result
   in Cell (Operation name newResult [op1])

-- Infix Operators

infixl 6 ⊕

(⊕) :: Cell -> Cell -> Cell
(⊕) = liftCell (liftA2 addCR) "sum"

infixl 6 ⊖

(⊖) :: Cell -> Cell -> Cell
(⊖) = liftCell (liftA2 subCR) "sub"

infixl 7 ⊗

(⊗) :: Cell -> Cell -> Cell
(⊗) = liftCell (liftA2 mulCR) "multiply"

infixl 7 ⊘

(⊘) :: Cell -> Cell -> Cell
(⊘) = liftCell (\ma mb -> ma >>= \a -> mb >>= \b -> divCR a b) "divide"

sqrt' :: Cell -> Cell
sqrt' = liftCellUnary (>>= sqrtCR) "sqrt"

(⊗⊗) :: Cell -> Cell -> Cell
(⊗⊗) = liftCell maybePowCR "exponent"

exp' :: Cell -> Cell
exp' = (⊗⊗) (to (exp 1))

sq :: Cell -> Cell
sq a = a ⊗ a

-- Update comparison operators
infix 4 ≪

(≪) :: Cell -> Cell -> Maybe Bool
(≪) = liftA2 (\(CR ar _) (CR br _) -> ar < br) `on` (\(Cell op) -> op.result)

infix 4 ≫

(≫) :: Cell -> Cell -> Maybe Bool
(≫) = liftA2 (\(CR ar _) (CR br _) -> ar > br) `on` (\(Cell op) -> op.result)

-- Update absolute value
abs' :: Cell -> Cell
abs' = liftCellUnary (fmap (\cr -> CR (magnitudeCR cr) 0)) "abs"

negate' :: Cell -> Cell
negate' = liftCellUnary (fmap negate) "negate"

infix 4 =~

(=~) :: Cell -> Cell -> Maybe Bool
(=~) a b = do
  -- Extract values from cells
  aVal <- (\(Cell op) -> op.result) a
  bVal <- (\(Cell op) -> op.result) b

  -- Get magnitudes
  let magnitudeA = magnitudeCR aVal
  let magnitudeB = magnitudeCR bVal

  -- Special case: if both are zero, they're equal
  if magnitudeA == 0 && magnitudeB == 0
    then return True
    else do
      -- Calculate the absolute difference
      let diff = magnitudeCR (aVal `subCR` bVal)

      -- Calculate the scale (using the maximum magnitude)
      let scale = max magnitudeA magnitudeB

      -- For large numbers, use relative tolerance
      -- For small numbers, still use absolute tolerance
      if scale > 1
        then return (diff / scale < 1e-10) -- Relative tolerance: diff/max < threshold
        else return (diff < 1e-10) -- Absolute tolerance for small numbers

to :: (Real a) => a -> Cell
to a = Cell (Operation "constant" (Just $ CR (toRational a) 0) [])

-- TODO: from 'fat number' to non complex number.. this could be a problem..
from :: (Fractional b) => Cell -> Either String b
from (Cell op) = case op.result of
  Just (CR r i) | i == 0 -> Right (fromRational r)
  Just (CR _ _) -> Left "Error: complex result"
  Nothing -> Left "Error: divide by zero or 0 ^ 0"
