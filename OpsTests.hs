module OpsTests (test) where

import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Ops (from, sqrt', to, (=~), (⊕), (⊖), (⊗), (⊗⊗), (⊘))
import Test.QuickCheck (Arbitrary, Property, (==>))
import TestUtils (testQuiet)

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
