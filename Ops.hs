{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ops (Cell, abs', from, negate', sqrt', to, (⊕), (⊖), (⊗), (⊘)) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Complex (Complex ((:+)), magnitude, mkPolar, polar)
import Data.Either (isLeft)
import Data.Function (on, (&))
import Data.Maybe (fromMaybe, isNothing)
import Data.Sequence qualified as Seq
import Test.QuickCheck (Arbitrary, Property, quickCheck, (==>))

-----------------------------------------

data Operation a = Operation
  { opName :: String,
    result :: Maybe (Complex a),
    inputs :: [Operation a]
  }

newtype Cell a = Cell
  {operation :: Operation a}

--
---'Fat' Ops

liftCell :: (Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)) -> String -> Cell a -> Cell a -> Cell a
liftCell f name (Cell op1) (Cell op2) =
  let newResult = f op1.result op2.result
   in Cell (Operation name newResult [op1, op2])

liftCellUnary :: (Maybe (Complex a) -> Maybe (Complex a)) -> String -> Cell a -> Cell a
liftCellUnary f name (Cell op1) =
  let newResult = f op1.result
   in Cell (Operation name newResult [op1])

-- Infix Operators

infixl 6 ⊕

(⊕) :: (RealFloat a) => Cell a -> Cell a -> Cell a
(⊕) = liftCell (liftA2 (+)) "sub"

infixl 6 ⊖

(⊖) :: (RealFloat a) => Cell a -> Cell a -> Cell a
(⊖) = liftCell (liftA2 (-)) "sub"

infixl 7 ⊗

(⊗) :: (RealFloat a) => Cell a -> Cell a -> Cell a
(⊗) = liftCell (liftA2 (*)) "multiply"

infixl 7 ⊘

(⊘) :: (RealFloat a) => Cell a -> Cell a -> Cell a
(⊘) = liftCell (liftA2 (/)) "divide"

sqrt' :: (RealFloat a) => Cell a -> Cell a
sqrt' = liftCellUnary (fmap sqrt) "sqrt"

(⊗⊗) :: (RealFloat a, Eq a) => Cell a -> Cell a -> Cell a
(⊗⊗) =
  liftCell
    ( \mc mw -> do
        c <- mc
        w <- mw
        if c == 0 :+ 0 && (w == 0 :+ 0)
          then Nothing
          else -- Special case for real numbers to improve accuracy
          case (c, w) of
            (re :+ 0, ex :+ 0) | re > 0 -> Just (re ** ex :+ 0)
            _ -> Just $ exp (w * log c) -- General complex case
    )
    "exponent"

exp' :: (RealFloat a) => Cell a -> Cell a
exp' = (⊗⊗) (to (exp 1))

sq :: (RealFloat a) => Cell a -> Cell a
sq a = a ⊗ a

-- Lexicographic (real only) ordering

infix 4 ≪

-- This returns a maybe Bool because we are never sure whether the Cells have a valid number of course..
-- TODO: only worries about real part, should use magnitude instead
(≪) :: (Ord a, Eq a, RealFloat a) => Cell a -> Cell a -> Maybe Bool
(≪) = liftA2 (\(ar :+ _) (br :+ _) -> ar < br) `on` (\(Cell op) -> op.result)

infix 4 ≫

(≫) :: (Ord a, Eq a, RealFloat a) => Cell a -> Cell a -> Maybe Bool
(≫) = liftA2 (\(ar :+ _) (br :+ _) -> ar > br) `on` (\(Cell op) -> op.result)

abs' :: (RealFloat a) => Cell a -> Cell a
abs' = liftCellUnary (fmap (\a -> magnitude a :+ 0)) "abs"

negate' :: (RealFloat a) => Cell a -> Cell a
negate' = liftCellUnary (fmap negate) "negate"

infix 4 =~

(=~) :: (RealFloat a) => Cell a -> Cell a -> Maybe Bool
(=~) a b = abs' (a ⊖ b) ≪ epsilon
  where
    epsilon = to 1e-9 -- Define your tolerance level here

------

to :: (Num a) => a -> Cell a
to a = Cell (Operation "constant" (Just $ a :+ 0) [])

-- TODO: from 'fat number' to non complex number.. this could be a problem..
from :: (Eq b, Num b) => Cell b -> Either String b
from (Cell op) = case op.result of
  Just (r :+ 0) -> Right r
  Just (_ :+ _) -> Left "Error: root of a negative number"
  Nothing -> Left "Error: divide by zero or 0 ^ 0"

-----------------------------------------

{- main :: IO ()
main = do
  print "Hello World"
 -}
-- 'test 1'
t1 :: (RealFloat a) => Cell a -> Cell a -> Maybe Bool
t1 a b = (a ⊘ b) ⊗ b ⊖ a ⊕ a ≫ abs' (sqrt' b)

pythC :: (RealFloat a, Num a) => Cell a -> Cell a -> Cell a
pythC a b = sqrt' $ sq a ⊕ sq b

i :: (RealFloat a) => Cell a
i = sqrt' $ to (-1)

p :: (RealFloat a) => Cell a
p = to pi

eulerID :: Bool
eulerID =
  fromMaybe False $ exp' (i ⊗ p) ⊕ to 1 =~ to 0

quad :: (RealFloat a) => Cell a -> Cell a -> Cell a -> (Cell a, Cell a)
quad a b c = (solution1, solution2)
  where
    solution1 = (negate' b ⊕ discriminantRoot) ⊘ divisor
    solution2 = (negate' b ⊖ discriminantRoot) ⊘ divisor
    discriminantRoot = sqrt' discriminant
    discriminant = sq b ⊖ to 4 ⊗ a ⊗ c
    divisor = to 2 ⊗ a

circleArea :: (RealFloat a) => Cell a -> Cell a
circleArea r = p ⊗ sq r

e :: (RealFloat a) => Cell a -> Cell a
e m = m ⊗ sq c
  where
    c = to 299792458

f :: (RealFloat a) => Cell a -> Cell a -> Cell a -> Cell a
f m1 m2 r = g ⊗ m1 ⊗ m2 ⊘ sq r
  where
    g = to 6.67 ⊗ (to 10 ⊗⊗ negate' (to 11))

fib :: (RealFloat a) => Cell a -> Cell a
fib (Cell op) = case op.result of
  Just (0 :+ 0) -> to 0
  Just (1 :+ 0) -> to 1
  Just n -> fib (Cell op ⊖ to 1) ⊕ fib (Cell op ⊖ to 2)
  Nothing -> to 0

-----------------------------------------

-- -- Property test for the identity property of addition
prop_addIdentity :: (RealFloat a) => a -> Bool
prop_addIdentity x =
  from (to x ⊕ to 0) == from (to x)
    && from (to 0 ⊕ to x) == from (to x)

prop_addCommutative :: (RealFloat a) => a -> a -> Bool
prop_addCommutative x y =
  from (to x ⊕ to y) == from (to y ⊕ to x)

prop_mulCommutative :: (RealFloat a) => a -> a -> Bool
prop_mulCommutative x y =
  from (to x ⊗ to y) == from (to y ⊗ to x)

prop_addAssociative :: (RealFloat a) => a -> a -> a -> Bool
prop_addAssociative x y z =
  fromMaybe False $ (to x ⊕ to y) ⊕ to z =~ to x ⊕ (to y ⊕ to z)

prop_mulAssociative :: (RealFloat a) => a -> a -> a -> Bool
prop_mulAssociative x y z =
  fromMaybe False $ (to x ⊗ to y) ⊗ to z =~ to x ⊗ (to y ⊗ to z)

prop_distributiveMulOverAdd :: (RealFloat a) => a -> a -> a -> Bool
prop_distributiveMulOverAdd x y z =
  fromMaybe False $ to x ⊗ (to y ⊕ to z) =~ (to x ⊗ to y) ⊕ (to x ⊗ to z)

prop_divByItself :: (RealFloat a) => a -> Property
prop_divByItself x = x /= 0 ==> from (to x ⊘ to x) == from (to 1)

prop_divByZero :: (RealFloat a) => a -> Bool
prop_divByZero x = isLeft $ from $ to x ⊘ to 0

prop_addSubInverse :: (RealFloat a) => a -> a -> Bool
prop_addSubInverse a b =
  fromMaybe False $ (to a ⊕ to b) ⊖ to b =~ to a

prop_mulDivInverse :: (RealFloat a) => a -> a -> Property
prop_mulDivInverse a b =
  b
    /= 0
      ==> fromMaybe False
    $ (to a ⊗ to b)
      ⊘ to b
      =~ to a

-- TODO: include (0 :+ 0) ** (0 :+ 0) test case?
-- TODO: include base less than zero case
prop_expNonZero :: (RealFloat a, Arbitrary a) => a -> a -> Property
prop_expNonZero base exponent =
  (base /= 0 && exponent /= 0 && base > 0)
    ==> fromMaybe False
    $ (⊗⊗) (to base) (to exponent) =~ to (base ** exponent)

-- Property test to check that 0 ** 0 returns Nothing
prop_zeroToZero :: Bool
prop_zeroToZero =
  isLeft $ from $ to 0 ⊗⊗ to 0

test :: IO ()
test = do
  print "1 + 1"
  print $ from (to 1 ⊕ to 1)

  print "Euler ID"
  print eulerID

  print "'f' 2 2 2"
  print $ from $ f (to (2 :: Double)) (to (2 :: Double)) (to (2 :: Double))

  print "fib 10"
  print $ from $ fib $ to 10

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
  quickCheck (prop_expNonZero :: Double -> Double -> Property) -- }
  putStrLn "prop_expZero"
  quickCheck (prop_zeroToZero :: Bool)
