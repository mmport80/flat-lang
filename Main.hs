import Control.Exception (SomeException, catch)
import Control.Monad (join)
import NonZero (NonZero (..), mkNonZero)

--
--- 1) Imperative style?

-- Attempt division and catch any exceptions, returning Nothing in case of an exception

safeDivEx :: Int -> Int -> Int -> IO (Maybe Int)
safeDivEx _ _ 0 = return Nothing -- Immediate return of Nothing if dividing by 0
safeDivEx _ 0 _ = return Nothing -- Immediate return of Nothing if dividing by 0
safeDivEx a b c =
  catch
    ( do
        let result = a `div` b `div` c
        return $ Just result
    )
    handler
  where
    handler :: SomeException -> IO (Maybe Int)
    handler _ = return Nothing

--
--- 3) Idiomatic style

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

-- likeable apart from the join

fmapEx :: (Eq a, Fractional a) => a -> a -> a -> Maybe a
fmapEx a b c = join $ fmap (a `safeDiv`) (b `safeDiv` c)

-- Clunky

doEx :: (Eq b, Fractional b) => b -> b -> b -> Maybe b
doEx a b c = do
  a' <- a `safeDiv` b
  a' `safeDiv` c

-- most idiomatic?
bindEx :: (Eq b, Fractional b) => b -> b -> b -> Maybe b
bindEx a b c = (a `safeDiv`) =<< (b `safeDiv` c)

--
--- 4) Handling NaN funkiness

-- NaN is part of Float type

simpleEx :: (RealFloat a) => a -> a -> a -> Maybe a
simpleEx a b c =
  let result = a / b / c
   in if isNaN result then Nothing else Just result

--
--- 5) Naive Example

-- Can cause exception. Bad idea
simpleDivEx :: (Integral a) => a -> a -> a -> a
simpleDivEx a b c = a `div` b `div` c

--
--- 6) Smart Constructor

nonZeroDiv :: (Fractional a, Eq a) => a -> NonZero a -> a
nonZeroDiv numerator (NonZero denominator) = numerator / denominator

-- not convinced
nonZeroEx :: (Eq b, Fractional b) => b -> b -> b -> Maybe b
nonZeroEx a b c = do
  d1 <- mkNonZero b
  d2 <- mkNonZero c
  return $ a `nonZeroDiv` d1 `nonZeroDiv` d2

--
--- Maybe Ops

maybeAdd :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeAdd (Just x) (Just y) = Just (x + y)
maybeAdd _ _ = Nothing

maybeSub :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeSub (Just x) (Just y) = Just (x - y)
maybeSub _ _ = Nothing

maybeMul :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeMul (Just x) (Just y) = Just (x * y)
maybeMul _ _ = Nothing

maybeDiv :: (Eq a, Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeDiv (Just n) (Just 0) = Nothing
maybeDiv (Just n) (Just d) = Just (n / d)
maybeDiv Nothing _ = Nothing
maybeDiv _ Nothing = Nothing

maybeDivEx :: (Eq a, Fractional a) => a -> a -> a -> Maybe a
maybeDivEx a b c = Just a `maybeDiv` Just b `maybeDiv` Just c

maybeEx = Just (-10.1) `maybeMul` Just 1 `maybeAdd` Just 9.0 `maybeSub` Just 0 `maybeDiv` Just 1

maybeIntEx = Just (-10) `maybeMul` Just 1 `maybeAdd` Just 9 `maybeSub` Just 0 `maybeDiv` Just 1

maybeRatEx = Just (10 :: Rational) `maybeDiv` Just (3 :: Rational) `maybeMul` Just (3 :: Rational)

main :: IO ()
main = do
  print "Hello World"

test :: IO ()
test = do
  sdx <- safeDivEx 16 2 1
  putStrLn $ "safeDivEx: " <> show sdx

  putStrLn $ "fmapEx: " <> show (fmapEx 16 2 1)

  putStrLn $ "bindEx: " <> show (bindEx 16 2 1)

  putStrLn $ "doEx: " <> show (doEx 16 2 1)

  putStrLn $ "simpleEx: " <> show (simpleEx 16 2 1)

  putStrLn $ "maybeEx: " <> show maybeEx

  putStrLn $ "maybeIntEx: " <> show maybeIntEx

  putStrLn $ "maybeRatEx: " <> show maybeRatEx

  putStrLn $ "nonZeroEx: " <> show (nonZeroEx 16 2 1)

  putStrLn $ "simpleDivEx: " <> show (simpleDivEx 16 2 0)
