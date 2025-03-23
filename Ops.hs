{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ops (Cell, abs', from, negate', sqrt', to, (⊕), (⊖), (⊗), (⊘)) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Complex (Complex ((:+)), imagPart, magnitude, mkPolar, polar, realPart)
import Data.Either (isLeft, isRight)
import Data.Function (on, (&))
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio (Rational, denominator, numerator, (%))
import Data.Sequence qualified as Seq
import GHC.Base (when)
import GHC.IO (unsafePerformIO)
import Test.QuickCheck (Arbitrary, Property, quickCheck, (==>))

-----------------------------------------

data ComplexRational = CR Rational Rational -- real and imaginary parts
  deriving (Eq, Show)

newtype Cell = Cell
  {operation :: Operation}

-- Update Operation to use this
data Operation = Operation
  { opName :: String,
    result :: Maybe ComplexRational,
    inputs :: [Operation]
  }

-- Implement arithmetic operations
addCR :: ComplexRational -> ComplexRational -> ComplexRational
addCR (CR r1 i1) (CR r2 i2) = CR (r1 + r2) (i1 + i2)

subCR :: ComplexRational -> ComplexRational -> ComplexRational
subCR (CR r1 i1) (CR r2 i2) = CR (r1 - r2) (i1 - i2)

divCR :: ComplexRational -> ComplexRational -> Maybe ComplexRational
divCR _ (CR 0 0) = Nothing
divCR (CR r1 i1) (CR r2 i2) =
  let d = r2 * r2 + i2 * i2
   in if d == 0
        then Nothing
        else Just $ CR ((r1 * r2 + i1 * i2) / d) ((i1 * r2 - r1 * i2) / d)

mulCR :: ComplexRational -> ComplexRational -> ComplexRational
mulCR (CR r1 i1) (CR r2 i2) = CR (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)

powCR :: ComplexRational -> ComplexRational -> Maybe ComplexRational
powCR (CR 0 0) (CR 0 0) = Nothing -- 0^0 = Nothing (undefined)
powCR (CR 0 0) _ = Just (CR 0 0) -- 0^n = 0 for n ≠ 0
powCR (CR 0 i) _ | i /= 0 = Nothing -- (0+i)^n is undefined in our implementation

-- CASE 1: Integer exponents - use exact rational arithmetic
powCR base (CR exp 0)
  | denominator exp == 1 =
      let n = numerator exp
       in if n >= 0
            then Just $ intPowCR base (fromIntegral n)
            else divCR (CR 1 0) (intPowCR base (fromIntegral (-n)))
-- CASE 2: Integer base, simple roots (1/n) - check for exact integer roots
powCR (CR base 0) (CR exp 0)
  | denominator base == 1 && numerator exp == 1 =
      let baseInt = numerator base
          rootN = denominator exp
       in case exactIntegerRoot baseInt rootN of
            Just root -> Just $ CR (fromInteger root) 0
            Nothing -> directPowCR (CR (toRational base) 0) (CR exp 0)
-- CASE 3: Simple rational base, simple roots - check for exact rational roots
powCR (CR a 0) (CR b 0)
  | numerator b == 1 =
      let n = denominator b
          numRoot = exactIntegerRoot (numerator a) n
          denRoot = exactIntegerRoot (denominator a) n
       in case (numRoot, denRoot) of
            (Just numR, Just denR) -> Just $ CR (fromInteger numR % fromInteger denR) 0
            _ -> directPowCR (CR a 0) (CR b 0)
-- CASE 4: All other cases - use floating point approximation
powCR base exp = directPowCR base exp

-- Check if an integer has an exact integer nth root
exactIntegerRoot :: Integer -> Integer -> Maybe Integer
exactIntegerRoot base n
  | n <= 0 = Nothing
  | base < 0 && even n = Nothing -- Negative base with even root
  | base < 0 = fmap negate $ exactIntegerRoot (negate base) n
  | base == 0 = Just 0
  | base == 1 = Just 1
  | otherwise =
      -- Try a direct computation first
      let root = floor $ fromInteger base ** (1 / fromInteger n)
       in if root ^ n == base
            then Just root
            else Nothing -- Not a perfect power

-- Direct floating-point calculation for when exact methods aren't possible
directPowCR :: ComplexRational -> ComplexRational -> Maybe ComplexRational
directPowCR base exp =
  let baseD = toComplexDouble base
      expD = toComplexDouble exp
      resultD = baseD ** expD
   in if isNaN (realPart resultD)
        || isNaN (imagPart resultD)
        || isInfinite (realPart resultD)
        || isInfinite (imagPart resultD)
        then Nothing
        else Just (fromComplexDouble resultD)

-- Helper to convert to Complex Double for computation
toComplexDouble :: ComplexRational -> Complex Double
toComplexDouble (CR a b) = (fromRational a :+ fromRational b)

-- Helper to convert from Complex Double back to ComplexRational
fromComplexDouble :: Complex Double -> ComplexRational
fromComplexDouble (a :+ b) = CR (toRational a) (toRational b)

-- Calculate exact roots where possible
exactRoot :: ComplexRational -> Int -> Maybe ComplexRational
exactRoot (CR r 0) n
  | r >= 0 =
      -- Perfect integer root
      let rootR = toRational (fromRational r ** (1 / fromIntegral n :: Double))
          -- Check if this is exact by raising back to power
          checkPower = rootR ^ n
       in if checkPower == r
            then Just (CR rootR 0)
            else complexRoot (CR r 0) n
-- For negative reals, exact roots may have imaginary parts
exactRoot (CR r 0) n
  | r < 0 && odd n =
      Just (CR (negate (toRational ((-fromRational r) ** (1 / fromIntegral n :: Double)))) 0)
-- For other cases, use complex root calculation
exactRoot z n = complexRoot z n

-- Proper complex root calculation
complexRoot :: ComplexRational -> Int -> Maybe ComplexRational
complexRoot (CR a b) n =
  -- For pure real numbers, can do direct calculation
  if b == 0
    then
      let rootVal = fromRational a ** (1 / fromIntegral n :: Double)
       in if isNaN rootVal || isInfinite rootVal
            then Nothing
            else Just (CR (toRational rootVal) 0)
    else -- For complex numbers, convert to polar form

      let aD = fromRational a
          bD = fromRational b
          -- r = magnitude, theta = argument
          r = sqrt (aD * aD + bD * bD)
          theta = atan2 bD aD
          -- nth root in polar form: r^(1/n) * e^(i*theta/n)
          rootR = r ** (1 / fromIntegral n)
          newTheta = theta / fromIntegral n
          -- Convert back to Cartesian
          newReal = rootR * cos newTheta
          newImag = rootR * sin newTheta
       in if isNaN newReal || isNaN newImag
            then Nothing
            else Just (CR (toRational newReal) (toRational newImag))

-- Helper for integer powers
intPowCR :: ComplexRational -> Int -> ComplexRational
intPowCR _ 0 = CR 1 0
intPowCR z 1 = z
intPowCR z n
  | even n = let half = intPowCR z (n `div` 2) in mulCR half half
  | otherwise = mulCR z (intPowCR z (n - 1))

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

sqrtCR :: ComplexRational -> Maybe ComplexRational
sqrtCR (CR 0 0) = Just (CR 0 0)
sqrtCR (CR r i) =
  let mag = sqrt (fromRational r * fromRational r + fromRational i * fromRational i)
      theta = atan2 (fromRational i) (fromRational r)
      newR = toRational (mag * cos (theta / 2))
      newI = toRational (mag * sin (theta / 2))
   in Just (CR newR newI)

sqrt' :: Cell -> Cell
sqrt' = liftCellUnary (>>= sqrtCR) "sqrt"

maybePowCR :: Maybe ComplexRational -> Maybe ComplexRational -> Maybe ComplexRational
maybePowCR Nothing _ = Nothing
maybePowCR _ Nothing = Nothing
maybePowCR (Just a) (Just b) = powCR a b

(⊗⊗) :: Cell -> Cell -> Cell
(⊗⊗) = liftCell maybePowCR "exponent"

exp' :: Cell -> Cell
exp' = (⊗⊗) (to (exp 1))

sq :: Cell -> Cell
sq a = a ⊗ a

-- This returns a maybe Bool because we are never sure whether the Cells have a valid number of course..
-- TODO: only worries about real part, should use magnitude instead
-- Define magnitude for ComplexRational (approximated)
magnitudeCR :: ComplexRational -> Rational
magnitudeCR (CR r i) =
  toRational (sqrt (fromRational (r * r + i * i) :: Double))

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

instance Num ComplexRational where
  (CR r1 i1) + (CR r2 i2) = CR (r1 + r2) (i1 + i2)
  (CR r1 i1) - (CR r2 i2) = CR (r1 - r2) (i1 - i2)
  (CR r1 i1) * (CR r2 i2) = CR (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)
  negate (CR r i) = CR (negate r) (negate i)
  abs z@(CR r i) =
    let magDouble = sqrt (fromRational r * fromRational r + fromRational i * fromRational i)
     in CR (toRational magDouble) 0
  signum z@(CR r i) =
    if r == 0 && i == 0
      then CR 0 0
      else
        let magDouble = sqrt (fromRational r * fromRational r + fromRational i * fromRational i)
            magnitude = toRational magDouble
         in CR (r / magnitude) (i / magnitude)
  fromInteger n = CR (fromInteger n) 0

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
prop_expNonZero :: (Show a, RealFloat a, Fractional a, Arbitrary a) => a -> a -> Property
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

-- Floating point precision issues
prop_floatingPointAddition :: Bool
prop_floatingPointAddition =
  maybe False not ((to 0.1 ⊕ to 0.2) =~ to 0.3)

-- Precision loss in large numbers
prop_precisionLoss :: Bool
prop_precisionLoss =
  maybe False not ((to 1e15 ⊕ to 1 ⊖ to 1e15) =~ to 1)

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
  -- print "1 + 1"
  -- print $ from (to 1 ⊕ to 1)

  -- print "Euler ID"
  -- print eulerID

  -- print "'f' 2 2 2"
  -- print $ from $ f (to (2 :: Double)) (to (2 :: Double)) (to (2 :: Double))

  -- print "fib 10"
  -- print $ from $ fib $ to 10

  -- TODO: use complex double instead of just double
  putStrLn "prop_addIdentity"
  quickCheck (prop_addIdentity :: Double -> Bool)
  putStrLn "prop_addCommutative"
  quickCheck (prop_addCommutative :: Double -> Double -> Bool)
  putStrLn "prop_mulCommutative"
  quickCheck (prop_mulCommutative :: Double -> Double -> Bool)
  putStrLn "prop_addAssociative"
  quickCheck (prop_addAssociative :: Double -> Double -> Double -> Bool)
  putStrLn "prop_mulAssociative"
  quickCheck (prop_mulAssociative :: Double -> Double -> Double -> Bool)
  putStrLn "prop_distributiveMulOverAdd"
  quickCheck (prop_distributiveMulOverAdd :: Double -> Double -> Double -> Bool)
  putStrLn "prop_divByItself"
  quickCheck (prop_divByItself :: Double -> Property)
  putStrLn "prop_divByZero"
  quickCheck (prop_divByZero :: Double -> Bool)
  putStrLn "prop_addSubInverse"
  quickCheck (prop_addSubInverse :: Double -> Double -> Bool)
  putStrLn "prop_mulDivInverse"
  quickCheck (prop_mulDivInverse :: Double -> Double -> Property)

  putStrLn "prop_expNonZero"
  quickCheck (prop_expNonZero :: Double -> Double -> Property)

  putStrLn "prop_expZero"
  quickCheck (prop_zeroToZero :: Bool)

  putStrLn "prop_zeroByZero"
  quickCheck prop_zeroByZero
  putStrLn "prop_smallDenominator"
  quickCheck prop_smallDenominator

  putStrLn "prop_negativePowerInteger"
  quickCheck prop_negativePowerInteger
  putStrLn "prop_negativePowerNonInteger"
  quickCheck prop_negativePowerNonInteger
  putStrLn "prop_negativePowerEvenInteger"
  quickCheck prop_negativePowerEvenInteger
  putStrLn "prop_negativePowerOddInteger"
  quickCheck prop_negativePowerOddInteger

  putStrLn "prop_exactAddition"
  quickCheck prop_exactAddition

  putStrLn "prop_floatingPointAddition"
  quickCheck prop_floatingPointAddition

  putStrLn "prop_precisionLoss"
  quickCheck prop_precisionLoss
  putStrLn "prop_errorPropagation"
  quickCheck prop_errorPropagation
  putStrLn "prop_sqrtNegative"
  quickCheck prop_sqrtNegative
  putStrLn "prop_multipleErrors"
  quickCheck prop_multipleErrors
  putStrLn "prop_mulDivIdentityNearZero"
  quickCheck prop_mulDivIdentityNearZero