module Tests where

import OpsTests qualified
import ParseTests qualified

test :: IO ()
test = do
  putStrLn "=== Parse Tests ==="
  ParseTests.test

  putStrLn "\n=== Ops Tests ==="
  OpsTests.test

main :: IO ()
main = test
