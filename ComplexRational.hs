module ComplexRational
  ( ComplexRational (..),
    addCR,
    subCR,
    mulCR,
    divCR,
    sqrtCR,
    magnitudeCR,
    maybePowCR,
  )
where

import Data.Complex (Complex ((:+)), imagPart, realPart)
import Data.Ratio (Rational, denominator, numerator, (%))
import GHC.Float (isInfinite, isNaN)

-- Define ComplexRational type
data ComplexRational = CR Rational Rational -- real and imaginary parts
  deriving (Eq, Show)

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

-- Helper for integer powers
intPowCR :: ComplexRational -> Int -> ComplexRational
intPowCR _ 0 = CR 1 0
intPowCR z 1 = z
intPowCR z n
  | even n = let half = intPowCR z (n `div` 2) in mulCR half half
  | otherwise = mulCR z (intPowCR z (n - 1))

-- Calculate magnitude of ComplexRational
magnitudeCR :: ComplexRational -> Rational
magnitudeCR (CR r i) =
  toRational (sqrt (fromRational (r * r + i * i) :: Double))

-- Square root of ComplexRational
sqrtCR :: ComplexRational -> Maybe ComplexRational
sqrtCR (CR 0 0) = Just (CR 0 0)
sqrtCR (CR r i) =
  let mag = sqrt (fromRational r * fromRational r + fromRational i * fromRational i)
      theta = atan2 (fromRational i) (fromRational r)
      newR = toRational (mag * cos (theta / 2))
      newI = toRational (mag * sin (theta / 2))
   in Just (CR newR newI)

-- Conversions between ComplexRational and Complex Double
toComplexDouble :: ComplexRational -> Complex Double
toComplexDouble (CR a b) = fromRational a :+ fromRational b

fromComplexDouble :: Complex Double -> ComplexRational
fromComplexDouble (a :+ b) = CR (toRational a) (toRational b)

-- Power operation
powCR :: ComplexRational -> ComplexRational -> Maybe ComplexRational
powCR (CR 0 0) (CR 0 0) = Nothing -- 0^0 = Nothing (undefined)
powCR (CR 0 0) _ = Just (CR 0 0) -- 0^n = 0 for n ≠ 0

-- Integer exponents - use exact rational arithmetic
powCR base (CR exp 0)
  | denominator exp == 1 =
      let n = numerator exp
       in if n >= 0
            then Just $ intPowCR base (fromIntegral n)
            else divCR (CR 1 0) (intPowCR base (fromIntegral (-n)))
-- For all other cases, use floating point approximation
powCR base exp =
  let baseD = toComplexDouble base
      expD = toComplexDouble exp
      resultD = baseD ** expD
   in if isNaN (realPart resultD)
        || isNaN (imagPart resultD)
        || isInfinite (realPart resultD)
        || isInfinite (imagPart resultD)
        then Nothing
        else Just (fromComplexDouble resultD)

-- For Num instance
instance Num ComplexRational where
  (+) :: ComplexRational -> ComplexRational -> ComplexRational
  (+) = addCR
  (-) :: ComplexRational -> ComplexRational -> ComplexRational
  (-) = subCR
  (*) :: ComplexRational -> ComplexRational -> ComplexRational
  (*) = mulCR
  negate :: ComplexRational -> ComplexRational
  negate (CR r i) = CR (negate r) (negate i)
  abs :: ComplexRational -> ComplexRational
  abs z@(CR r i) =
    let magDouble = sqrt (fromRational r * fromRational r + fromRational i * fromRational i)
     in CR (toRational magDouble) 0
  signum :: ComplexRational -> ComplexRational
  signum z@(CR r i) =
    if r == 0 && i == 0
      then CR 0 0
      else
        let magDouble = sqrt (fromRational r * fromRational r + fromRational i * fromRational i)
            magnitude = toRational magDouble
         in CR (r / magnitude) (i / magnitude)
  fromInteger :: Integer -> ComplexRational
  fromInteger n = CR (fromInteger n) 0

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

maybePowCR :: Maybe ComplexRational -> Maybe ComplexRational -> Maybe ComplexRational
maybePowCR Nothing _ = Nothing
maybePowCR _ Nothing = Nothing
maybePowCR (Just a) (Just b) = powCR a b
