module NonZero (NonZero (NonZero), mkNonZero, getNonZero) where

newtype NonZero a = NonZero a deriving (Show, Eq)

mkNonZero :: (Eq a, Num a) => a -> Maybe (NonZero a)
mkNonZero 0 = Nothing
mkNonZero x = Just (NonZero x)

getNonZero :: NonZero a -> a
getNonZero (NonZero x) = x
