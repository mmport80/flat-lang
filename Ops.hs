import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Complex (Complex ((:+)), magnitude, mkPolar, polar)
import Data.Maybe (fromMaybe, isNothing)
import Test.QuickCheck

-----------------------------------------

--
---'Fat' Ops

-- Infix Operators

infixl 6 +?

(+?) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(+?) = liftA2 (+)

infixl 6 -?

(-?) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(-?) = liftA2 (-)

infixl 7 *?

(*?) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(*?) = liftA2 (*)

infixl 7 /?

(/?) :: (Eq a, RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(/?) mx my = do
  x <- mx
  y <- my
  guard (y /= 0) -- Prevent division by zero
  return (x / y)

sqrt' :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a)
sqrt' = fmap sqrt

infixl 8 **?

(**?) :: (RealFloat a, Eq a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(**?) mc mw = do
  c <- mc
  w <- mw
  -- Check for 0^0 explicitly
  if c == 0 :+ 0 && (w == 0 :+ 0)
    then Nothing -- or Just (1 :+ 0) depending on your choice
    else return $ exp (w * log c)

exp' :: Maybe (Complex Double) -> Maybe (Complex Double)
exp' = (**?) (realToFat (exp 1))

sq' :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a)
sq' a = a **? realToFat 2

-- Lexicographic (real only) ordering

infix 4 <?

(<?) :: (Ord a, Eq a, RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe Bool
(<?) = liftA2 (\(ar :+ _) (br :+ _) -> ar < br)

infix 4 >?

(>?) :: (Ord a, Eq a, RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe Bool
(>?) = liftA2 (\(ar :+ _) (br :+ _) -> ar > br)

abs' :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a)
abs' = fmap (\a -> magnitude a :+ 0)

negate' :: (Num a, RealFloat a) => Maybe (Complex a) -> Maybe (Complex a)
negate' = fmap negate

infix 4 =~

(=~) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe Bool
(=~) a b = abs' (a -? b) <? epsilon
  where
    epsilon = Just 1e-9 -- Define your tolerance level here

realToFat :: (Num a) => a -> Maybe (Complex a)
realToFat a = Just $ a :+ 0

-- (>>) a = Just $ a :+ 0

cToFat :: Complex a -> Maybe (Complex a)
cToFat = Just

toReal :: (Eq b, Num b) => Maybe (Complex b) -> Either String b
toReal (Just (r :+ 0)) = Right r
toReal (Just (r :+ i)) = Left "Error: root of a negative number"
toReal Nothing = Left "Error: divide by zero or 0 ^ 0"

-----------------------------------------

main :: IO ()
main = do
  print "Hello World"

t1 :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe Bool
t1 a b = (a /? b) *? b -? a +? a >? abs' (sqrt' b)

pythC :: (RealFloat a, Num (Maybe (Complex a))) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
pythC a b = sqrt' $ sq' a +? sq' b

eulerID :: Bool
eulerID =
  fromMaybe False $ exp' (i *? p) =~ realToFat (-1)
  where
    i = sqrt' (realToFat (-1))
    p = realToFat pi

quad :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a) -> (Maybe (Complex a), Maybe (Complex a))
quad a b c = (solution1, solution2)
  where
    solution1 = (negate' b +? discriminantRoot) /? divisor
    solution2 = (negate' b -? discriminantRoot) /? divisor
    discriminantRoot = sqrt' discriminant
    discriminant = sq' b -? realToFat 4 *? a *? c
    divisor = realToFat 2 *? a

circleArea r = p *? (r **? realToFat 2)
  where
    p = realToFat pi

-----------------------------------------

-- -- Property test for the identity property of addition
prop_addIdentity :: (RealFloat a) => Complex a -> Bool
prop_addIdentity x =
  Just x +? Just (0 :+ 0) == Just x
    && Just (0 :+ 0) +? Just x == Just x

prop_addCommutative :: (RealFloat a) => Complex a -> Complex a -> Bool
prop_addCommutative x y =
  Just x +? Just y == Just y +? Just x

prop_mulCommutative :: (RealFloat a) => Complex a -> Complex a -> Bool
prop_mulCommutative x y =
  Just x *? Just y == Just y *? Just x

prop_addAssociative :: (RealFloat a) => Complex a -> Complex a -> Complex a -> Bool
prop_addAssociative x y z =
  fromMaybe False $ (Just x +? Just y) +? Just z =~ Just x +? (Just y +? Just z)

prop_mulAssociative :: (RealFloat a) => Complex a -> Complex a -> Complex a -> Bool
prop_mulAssociative x y z =
  fromMaybe False $ (Just x *? Just y) *? Just z =~ Just x *? (Just y *? Just z)

prop_distributiveMulOverAdd :: (RealFloat a) => Complex a -> Complex a -> Complex a -> Bool
prop_distributiveMulOverAdd x y z =
  fromMaybe False $ Just x *? (Just y +? Just z) =~ (Just x *? Just y) +? (Just x *? Just z)

prop_operationWithNothing :: (RealFloat a) => (Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)) -> Complex a -> Bool
prop_operationWithNothing op x =
  isNothing (op (Just x) Nothing)
    && isNothing (op Nothing (Just x))
    && isNothing (op Nothing Nothing)

prop_divByItself :: (RealFloat a) => Complex a -> Property
prop_divByItself x = x /= 0 ==> Just x /? Just x == Just 1

prop_divByZero :: (RealFloat a) => Complex a -> Bool
prop_divByZero x = isNothing (Just x /? Just 0)

prop_addSubInverse :: (RealFloat a) => Complex a -> Complex a -> Bool
prop_addSubInverse a b =
  fromMaybe False $ (Just a +? Just b) -? Just b =~ Just a

prop_mulDivInverse :: (RealFloat a) => Complex a -> Complex a -> Property
prop_mulDivInverse a b =
  b
    /= 0
      ==> fromMaybe False
    $ (Just a *? Just b)
      /? Just b
      =~ Just a

-- Modified property test to avoid (0 :+ 0) ** (0 :+ 0)
prop_expNonZero :: (RealFloat a, Arbitrary a) => Complex a -> Complex a -> Property
prop_expNonZero base exponent =
  (base /= 0 :+ 0 && exponent /= 0 :+ 0)
    ==> fromMaybe False
    $ (**?) (Just base) (Just exponent) =~ Just (base ** exponent)

-- Property test to check that 0 ** 0 returns Nothing
prop_zeroToZero :: Bool
prop_zeroToZero =
  isNothing $ (**?) (Just $ 0 :+ 0) (Just $ 0 :+ 0)

test :: IO ()
test = do
  print $ Just (1 :+ 1) +? Just (1 :+ 1)

  print eulerID

  putStrLn "prop_addIdentity"
  quickCheck (prop_addIdentity :: Complex Double -> Bool)
  putStrLn "prop_addCommutative"
  quickCheck (prop_addCommutative :: Complex Double -> Complex Double -> Bool)
  putStrLn "prop_mulCommutative"
  quickCheck (prop_mulCommutative :: Complex Double -> Complex Double -> Bool)
  putStrLn "prop_addAssociative"
  quickCheck (prop_addAssociative :: Complex Double -> Complex Double -> Complex Double -> Bool)
  putStrLn "prop_mulAssociative"
  quickCheck (prop_mulAssociative :: Complex Double -> Complex Double -> Complex Double -> Bool)
  putStrLn "prop_distributiveMulOverAdd"
  quickCheck (prop_distributiveMulOverAdd :: Complex Double -> Complex Double -> Complex Double -> Bool)
  putStrLn "prop_operationWithNothing (+?)"
  quickCheck (prop_operationWithNothing (+?) :: Complex Double -> Bool)
  putStrLn "prop_operationWithNothing (-?)"
  quickCheck (prop_operationWithNothing (-?) :: Complex Double -> Bool)
  putStrLn "prop_operationWithNothing (*?)"
  quickCheck (prop_operationWithNothing (*?) :: Complex Double -> Bool)
  putStrLn "prop_operationWithNothing (/?)"
  quickCheck (prop_operationWithNothing (/?) :: Complex Double -> Bool)
  putStrLn "prop_divByItself"
  quickCheck (prop_divByItself :: Complex Double -> Property)
  putStrLn "prop_divByZero"
  quickCheck (prop_divByZero :: Complex Double -> Bool)
  putStrLn "prop_addSubInverse"
  quickCheck (prop_addSubInverse :: Complex Double -> Complex Double -> Bool)
  putStrLn "prop_mulDivInverse"
  quickCheck (prop_mulDivInverse :: Complex Double -> Complex Double -> Property)
  putStrLn "prop_expNonZero"
  quickCheck (prop_expNonZero :: Complex Double -> Complex Double -> Property)
  putStrLn "prop_expZero"
  quickCheck (prop_zeroToZero :: Bool)

-- putStrLn "prop_expIdentity"
-- quickCheck (prop_expIdentity :: Complex Double -> Bool)
-- putStrLn "prop_expIdentity'"
-- quickCheck (prop_expIdentity' :: Complex Double -> Bool)
