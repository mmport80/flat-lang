module TestUtils where

import Test.QuickCheck
  ( Args (chatty, maxSuccess),
    Testable,
    isSuccess,
    quickCheckWithResult,
    stdArgs,
  )

testQuiet :: (Testable prop) => [Char] -> prop -> IO ()
testQuiet name prop = do
  result <- quickCheckWithResult stdArgs {chatty = False, maxSuccess = 100} prop
  if isSuccess result
    then putStr "."
    else putStrLn $ "\n" ++ name ++ " FAILED"
