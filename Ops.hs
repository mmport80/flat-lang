import Control.Monad (guard)
import Data.Complex (Complex ((:+)), magnitude)
import Data.Maybe (fromMaybe, isNothing)
import Test.QuickCheck

-----------------------------------------

--
--- Maybe Ops

-- Infix Operators

infixl 6 +?

(+?) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(+?) (Just x) (Just y) = Just (x + y)
(+?) _ _ = Nothing

infixl 6 -?

(-?) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(-?) (Just x) (Just y) = Just (x - y)
(-?) _ _ = Nothing

infixl 7 *?

(*?) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(*?) (Just x) (Just y) = Just (x * y)
(*?) _ _ = Nothing

infixl 7 /?

(/?) :: (Eq a, RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe (Complex a)
(/?) mx my = do
  x <- mx
  y <- my
  guard (y /= 0) -- Prevent division by zero
  return (x / y)

sqrtMaybe :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a)
sqrtMaybe (Just a) = Just (sqrt a)
sqrtMaybe Nothing = Nothing

-- Lexicographic (real only) ordering

infix 4 <?

(<?) :: (Ord a, Eq a, RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe Bool
(<?) Nothing _ = Nothing
(<?) _ Nothing = Nothing
(<?) (Just (ar :+ _)) (Just (br :+ _)) = Just $ ar < br

infix 4 >?

(>?) :: (Ord a, Eq a, RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe Bool
(>?) Nothing _ = Nothing
(>?) _ Nothing = Nothing
(>?) (Just (ar :+ _)) (Just (br :+ _)) = Just $ ar > br

absMaybe :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a)
absMaybe (Just a) = Just $ magnitude a :+ 0
absMaybe _ = Nothing

infix 4 ~=

(~=) :: (RealFloat a) => Maybe (Complex a) -> Maybe (Complex a) -> Maybe Bool
(~=) a b = absMaybe (a -? b) <? epsilon
  where
    epsilon = Just 1e-9 -- Define your tolerance level here

-----------------------------------------

main :: IO ()
main = do
  print "Hello World"

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
  fromMaybe False $ (Just x +? Just y) +? Just z ~= Just x +? (Just y +? Just z)

prop_mulAssociative :: (RealFloat a) => Complex a -> Complex a -> Complex a -> Bool
prop_mulAssociative x y z =
  fromMaybe False $ (Just x *? Just y) *? Just z ~= Just x *? (Just y *? Just z)

prop_distributiveMulOverAdd :: (RealFloat a) => Complex a -> Complex a -> Complex a -> Bool
prop_distributiveMulOverAdd x y z =
  fromMaybe False $ Just x *? (Just y +? Just z) ~= (Just x *? Just y) +? (Just x *? Just z)

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
  fromMaybe False $ (Just a +? Just b) -? Just b ~= Just a

prop_mulDivInverse :: (RealFloat a) => Complex a -> Complex a -> Property
prop_mulDivInverse a b =
  b
    /= 0
      ==> fromMaybe False
    $ (Just a *? Just b)
      /? Just b
      ~= Just a

test :: IO ()
test = do
  print $ Just (1 :+ 1) +? Just (1 :+ 1)

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
