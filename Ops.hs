import Data.Maybe (isNothing)
import Test.QuickCheck

-----------------------------------------

--
--- Maybe Ops

-- Infix Operators
infixl 6 +?

infixl 6 -?

infixl 7 *?

infixl 7 /?

maybeAdd :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeAdd (Just x) (Just y) = Just (x + y)
maybeAdd _ _ = Nothing

x +? y = maybeAdd x y -- Just an alias for convenience

maybeSub :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeSub (Just x) (Just y) = Just (x - y)
maybeSub _ _ = Nothing

x -? y = maybeSub x y

maybeMul :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeMul (Just x) (Just y) = Just (x * y)
maybeMul _ _ = Nothing

x *? y = maybeMul x y

maybeDiv :: (Eq a, Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeDiv (Just n) (Just 0) = Nothing
maybeDiv (Just n) (Just d) = Just (n / d)
maybeDiv Nothing _ = Nothing
maybeDiv _ Nothing = Nothing

x /? y = maybeDiv x y

-----------------------------------------

main :: IO ()
main = do
  print "Hello World"

-----------------------------------------

-- Property test for the identity property of addition
prop_addIdentity :: Rational -> Bool
prop_addIdentity x =
  Just x +? Just 0 == Just x
    && Just 0 +? Just x == Just x

prop_addCommutative :: Rational -> Rational -> Bool
prop_addCommutative x y =
  Just x +? Just y == Just y +? Just x

prop_mulCommutative :: Rational -> Rational -> Bool
prop_mulCommutative x y =
  Just x *? Just y == Just y *? Just x

prop_addAssociative :: Rational -> Rational -> Rational -> Bool
prop_addAssociative x y z =
  (Just x +? Just y) +? Just z == Just x +? (Just y +? Just z)

prop_mulAssociative :: Rational -> Rational -> Rational -> Bool
prop_mulAssociative x y z =
  (Just x *? Just y) *? Just z == Just x *? (Just y *? Just z)

prop_distributiveMulOverAdd :: Rational -> Rational -> Rational -> Bool
prop_distributiveMulOverAdd x y z =
  Just x *? (Just y +? Just z) == (Just x *? Just y) +? (Just x *? Just z)

prop_operationWithNothing :: (Maybe Rational -> Maybe Rational -> Maybe Rational) -> Rational -> Bool
prop_operationWithNothing op x =
  isNothing (op (Just x) Nothing)
    && isNothing (op Nothing (Just x))
    && isNothing (op Nothing Nothing)

prop_divByItself :: Rational -> Property
prop_divByItself x = x /= 0 ==> Just x /? Just x == Just 1

prop_divByZero :: Rational -> Bool
prop_divByZero x = isNothing (Just x /? Just 0)

prop_addSubInverse :: Rational -> Rational -> Bool
prop_addSubInverse a b =
  (Just a +? Just b) -? Just b == Just a

prop_mulDivInverse :: Rational -> Rational -> Property
prop_mulDivInverse a b =
  b
    /= 0
      ==> (Just a *? Just b)
      /? Just b
    == Just a

test :: IO ()
test = do
  quickCheck prop_addIdentity
  quickCheck prop_addCommutative
  quickCheck prop_mulCommutative
  quickCheck prop_addAssociative
  quickCheck prop_mulAssociative
  quickCheck prop_distributiveMulOverAdd
  quickCheck (prop_operationWithNothing maybeAdd)
  quickCheck (prop_operationWithNothing maybeSub)
  quickCheck (prop_operationWithNothing maybeMul)
  quickCheck (prop_operationWithNothing maybeDiv)
  quickCheck prop_divByItself
  quickCheck prop_divByZero
  quickCheck prop_addSubInverse
  quickCheck prop_mulDivInverse
