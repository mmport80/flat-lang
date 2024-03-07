import Control.Exception (SomeException, catch)
import Control.Monad (join)
import NonZero (NonZero (..), getNonZero, mkNonZero)

-- import NumberType

-- Attempt division and catch any exceptions, returning Nothing in case of an exception
safeDivEx :: Int -> Int -> IO (Maybe Int)
safeDivEx _ 0 = return Nothing -- Immediate return of Nothing if dividing by 0
safeDivEx numerator denominator =
  catch
    ( do
        let result = numerator `div` denominator
        return $ Just result
    )
    handler
  where
    handler :: SomeException -> IO (Maybe Int)
    handler _ = return Nothing

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

nonZeroDiv :: (Fractional a, Eq a) => a -> NonZero a -> a
nonZeroDiv numerator (NonZero denominator) = numerator / denominator

maybeDiv :: (Eq a, Fractional a) => Maybe a -> Maybe a -> Maybe a
maybeDiv (Just n) (Just 0) = Nothing
maybeDiv (Just n) (Just d) = Just (n / d)
maybeDiv Nothing _ = Nothing
maybeDiv _ Nothing = Nothing

-- not idomatic, but v nice
maybeDivEx :: Maybe Double
maybeDivEx = Just 16 `maybeDiv` Just 2 `maybeDiv` Just 0

-- likeable apart from the join
fmapEx :: Maybe Double
fmapEx = join $ fmap (16 `safeDiv`) (2 `safeDiv` 1)

-- Clunky
doEx :: Maybe Double
doEx = do
  a <- 16 `safeDiv` 2
  a `safeDiv` 1

-- most idiomatic?
bindEx :: Maybe Double
bindEx = (16 `safeDiv`) =<< (2 `safeDiv` 1)

-- NaN is part of Float type
simpleEx :: Double -> Double -> Double -> Maybe Double
simpleEx a b c =
  let result = a / b / c
   in if isNaN result then Nothing else Just result

-- Can cause exception. Bad idea
simpleDivEx :: (Integral a) => a -> a
simpleDivEx a = a `div` a

-- Does this do much better than a Maybe?
nonZeroEx :: (Eq b, Fractional b) => b -> b -> Maybe b
nonZeroEx a b = do
  d <- mkNonZero b
  return $ a `nonZeroDiv` d

-- data Number a = Zero | NonZero' a deriving (Show)

-- divNumber :: (Fractional a) => Number a -> NonZero a -> Number a
-- divNumber Zero _ = Zero -- Dividing zero by any non-zero number is zero
-- divNumber (NonZero' n) (NonZero d) = NonZero' (n / d) -- Normal division case

main :: IO ()
main = do
  print "Hello World"

test :: IO ()
test = do
  putStrLn $ "fmapEx: " <> show fmapEx

  putStrLn $ "bindEx: " <> show bindEx

  putStrLn $ "doEx: " <> show doEx

  putStrLn $ "simpleEx: " <> show (simpleEx 16 2 1)

  -- result1 <- safeDivEx 16 2
  -- result2 <- safeDivEx result1 1
  -- putStrLn $ "safeDivEx: " <> show result2

  putStrLn $ "maybeDivEx: " <> show maybeDivEx

  putStrLn $ "nonZeroEx: " <> show (nonZeroEx 0 1)

  putStrLn $ "simpleDivEx: " <> show (simpleDivEx 0)
