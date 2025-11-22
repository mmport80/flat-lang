module Tests where

import OpsTests qualified
import Parse qualified

test :: IO ()
test = do
  putStrLn "=== Parse Tests ==="
  Parse.test

  putStrLn "\n=== Ops Tests ==="
  OpsTests.test

main :: IO ()
main = test
