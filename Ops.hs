{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ops (test, Cell (..), abs', from, negate', sqrt', to, (⊕), (⊖), (⊗), (⊘), sqrtCR, Operation (..)) where

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

------

to :: (Real a) => a -> Cell
to a = Cell (Operation "constant" (Just $ CR (toRational a) 0) [])

-- TODO: from 'fat number' to non complex number.. this could be a problem..
from :: (Fractional b) => Cell -> Either String b
from (Cell op) = case op.result of
  Just (CR r i) | i == 0 -> Right (fromRational r)
  Just (CR _ _) -> Left "Error: complex result"
  Nothing -> Left "Error: divide by zero or 0 ^ 0"

-----------------------------------------
-----------------------------------------
-----------------------------------------
-----------------------------------------
-----------------------------------------
-----------------------------------------
-----------------------------------------

-- 'test 1'
t1 :: Cell -> Cell -> Maybe Bool
t1 a b = (a ⊘ b) ⊗ b ⊖ a ⊕ a ≫ abs' (sqrt' b)

pythC :: Cell -> Cell -> Cell
pythC a b = sqrt' $ sq a ⊕ sq b

i :: Cell
i = sqrt' $ to (-1)

p :: Cell
p = to pi

eulerID :: Bool
eulerID =
  fromMaybe False $ exp' (i ⊗ p) ⊕ to 1 =~ to 0

quad :: Cell -> Cell -> Cell -> (Cell, Cell)
quad a b c = (solution1, solution2)
  where
    solution1 = (negate' b ⊕ discriminantRoot) ⊘ divisor
    solution2 = (negate' b ⊖ discriminantRoot) ⊘ divisor
    discriminantRoot = sqrt' discriminant
    discriminant = sq b ⊖ to 4 ⊗ a ⊗ c
    divisor = to 2 ⊗ a

circleArea :: Cell -> Cell
circleArea r = p ⊗ sq r

e :: Cell -> Cell
e m = m ⊗ sq c
  where
    c = to 299792458

f :: Cell -> Cell -> Cell -> Cell
f m1 m2 r = g ⊗ m1 ⊗ m2 ⊘ sq r
  where
    g = to 6.67 ⊗ (to 10 ⊗⊗ negate' (to 11))

{- fib :: Cell -> Cell
fib (Cell op) = case op.result of
  Just (0 :+ 0) -> to 0
  Just (1 :+ 0) -> to 1
  Just n -> fib (Cell op ⊖ to 1) ⊕ fib (Cell op ⊖ to 2)
  Nothing -> to 0 -}

-----------------------------------------

-- -- Property test for the identity property of addition
prop_addIdentity :: (RealFloat a, Fractional a) => a -> Bool
prop_addIdentity x =
  from (to x ⊕ to 0) == from (to x)
    && from (to 0 ⊕ to x) == from (to x)

prop_addCommutative :: (RealFloat a, Fractional a) => a -> a -> Bool
prop_addCommutative x y =
  from (to x ⊕ to y) == from (to y ⊕ to x)

prop_mulCommutative :: (RealFloat a, Fractional a) => a -> a -> Bool
prop_mulCommutative x y =
  from (to x ⊗ to y) == from (to y ⊗ to x)

prop_addAssociative :: (RealFloat a, Fractional a) => a -> a -> a -> Bool
prop_addAssociative x y z =
  fromMaybe False $ (to x ⊕ to y) ⊕ to z =~ to x ⊕ (to y ⊕ to z)

prop_mulAssociative :: (RealFloat a, Fractional a) => a -> a -> a -> Bool
prop_mulAssociative x y z =
  fromMaybe False $ (to x ⊗ to y) ⊗ to z =~ to x ⊗ (to y ⊗ to z)

prop_distributiveMulOverAdd :: (RealFloat a, Fractional a) => a -> a -> a -> Bool
prop_distributiveMulOverAdd x y z =
  fromMaybe False $ to x ⊗ (to y ⊕ to z) =~ (to x ⊗ to y) ⊕ (to x ⊗ to z)

prop_divByItself :: (RealFloat a, Fractional a) => a -> Property
prop_divByItself x = x /= 0 ==> from (to x ⊘ to x) == from (to 1)

prop_divByZero :: (RealFloat a, Fractional a) => a -> Bool
prop_divByZero x = isLeft $ from $ to x ⊘ to 0

prop_addSubInverse :: (RealFloat a, Fractional a) => a -> a -> Bool
prop_addSubInverse a b =
  fromMaybe False $ (to a ⊕ to b) ⊖ to b =~ to a

prop_mulDivInverse :: (RealFloat a, Fractional a) => a -> a -> Property
prop_mulDivInverse a b =
  b
    /= 0
      ==> fromMaybe False
    $ (to a ⊗ to b)
      ⊘ to b
      =~ to a

-- TODO: include (0 :+ 0) ** (0 :+ 0) test case?
-- TODO: include base less than zero case
prop_expNonZero :: (Show a, Fractional a, RealFloat a, Arbitrary a) => a -> a -> Property
prop_expNonZero base exponent =
  ( base /= 0
      && exponent /= 0
      && base > 0
      && exponent < 20 -- Limiting exponent to prevent extremely large values
  )
    ==> do
      fromMaybe False $ (⊗⊗) (to base) (to exponent) =~ to (base ** exponent)

-- Property test to check that 0 ** 0 returns Nothing
prop_zeroToZero :: Bool
prop_zeroToZero =
  isLeft $ from $ to 0 ⊗⊗ to 0

-- Division of zero by zero (should be an error)
prop_zeroByZero :: Bool
prop_zeroByZero = isLeft $ from $ to 0 ⊘ to 0

-- Division by very small number (may overflow)
prop_smallDenominator :: Bool
prop_smallDenominator = isRight $ from $ to 1 ⊘ to 1e-308

-- Negative base with integer exponent is defined and real
prop_negativePowerInteger :: Bool
prop_negativePowerInteger =
  isRight $ from $ to (-2) ⊗⊗ to (2 :: Int)

-- Negative base with non-integer exponent results in a complex number
prop_negativePowerNonInteger :: Bool
prop_negativePowerNonInteger =
  isLeft $ from $ to (-2) ⊗⊗ to (2.5 :: Double)

-- Negative base with even integer exponent gives positive result
prop_negativePowerEvenInteger :: Bool
prop_negativePowerEvenInteger =
  from (to (-2) ⊗⊗ to (2 :: Int)) == Right 4.0

-- Negative base with odd integer exponent gives negative result
prop_negativePowerOddInteger :: Bool
prop_negativePowerOddInteger =
  from (to (-2) ⊗⊗ to (3 :: Int)) == Right (-8.0)

prop_exactAddition :: Bool
prop_exactAddition =
  let a = to (1 % 10 :: Rational)
      b = to (2 % 10 :: Rational)
      c = to (3 % 10 :: Rational)
   in fromMaybe True (a ⊕ b =~ c)

prop_precision :: Bool
prop_precision =
  (to (1 % 3) ⊕ to (1 % 3) ⊕ to (1 % 3)) == to 1

-- Error propagation - operations after division by zero
prop_errorPropagation :: Bool
prop_errorPropagation = isLeft $ from $ (to 1 ⊘ to 0) ⊕ to 5

-- Square root of negative number
prop_sqrtNegative :: Double -> Property
prop_sqrtNegative x = x > 0 ==> isLeft $ from $ sqrt' $ to (-x)

-- Multiple error conditions
prop_multipleErrors :: Bool
prop_multipleErrors = isLeft $ from $ sqrt' $ to (-1) ⊕ (to 1 ⊘ to 0)

-- Testing the identity a * (1/a) = 1 for values approaching zero
prop_mulDivIdentityNearZero :: Property
prop_mulDivIdentityNearZero =
  let smallValue = to 1e-150
   in True ==> fromMaybe False $ (smallValue ⊗ (to 1 ⊘ smallValue)) =~ to 1

test :: IO ()
test = do
  -- TODO: use complex double instead of just double
  testQuiet "prop_addIdentity" (prop_addIdentity :: Double -> Bool)
  testQuiet "prop_addCommutative" (prop_addCommutative :: Double -> Double -> Bool)
  testQuiet "prop_mulCommutative" (prop_mulCommutative :: Double -> Double -> Bool)
  testQuiet "prop_addAssociative" (prop_addAssociative :: Double -> Double -> Double -> Bool)
  testQuiet "prop_mulAssociative" (prop_mulAssociative :: Double -> Double -> Double -> Bool)
  testQuiet "prop_distributiveMulOverAdd" (prop_distributiveMulOverAdd :: Double -> Double -> Double -> Bool)
  testQuiet "prop_divByItself" (prop_divByItself :: Double -> Property)
  testQuiet "prop_divByZero" (prop_divByZero :: Double -> Bool)
  testQuiet "prop_addSubInverse" (prop_addSubInverse :: Double -> Double -> Bool)
  testQuiet "prop_mulDivInverse" (prop_mulDivInverse :: Double -> Double -> Property)

  testQuiet "prop_expNonZero" (prop_expNonZero :: Double -> Double -> Property)

  testQuiet "prop_expZero" (prop_zeroToZero :: Bool)

  testQuiet "prop_zeroByZero" prop_zeroByZero
  testQuiet "prop_smallDenominator" prop_smallDenominator

  testQuiet "prop_negativePowerInteger" prop_negativePowerInteger
  testQuiet "prop_negativePowerNonInteger" prop_negativePowerNonInteger
  testQuiet "prop_negativePowerEvenInteger" prop_negativePowerEvenInteger
  testQuiet "prop_negativePowerOddInteger" prop_negativePowerOddInteger

  testQuiet "prop_exactAddition" prop_exactAddition

  testQuiet "prop_precision" prop_precision
  testQuiet "prop_errorPropagation" prop_errorPropagation
  testQuiet "prop_sqrtNegative" prop_sqrtNegative
  testQuiet "prop_multipleErrors" prop_multipleErrors
  testQuiet "prop_mulDivIdentityNearZero" prop_mulDivIdentityNearZero
