module NumberType where

-- Assume the existence of a NonZero type and its smart constructor mkNonZero
newtype NonZero a = NonZero a deriving (Show)

mkNonZero :: (Eq a, Num a) => a -> Maybe (NonZero a)
mkNonZero 0 = Nothing
mkNonZero x = Just (NonZero x)

data Number a = Zero | NonZeroNumber (NonZero a) deriving (Show)

-- Utility function to create a Number from any value
makeNumber :: (Eq a, Num a) => a -> Number a
makeNumber x = case mkNonZero x of
  Just nz -> NonZeroNumber nz
  Nothing -> Zero
