module Tests where

import Ops qualified
import Parse qualified

test :: IO ()
test = do
  putStrLn "=== Parse Tests ==="
  Parse.test

  putStrLn "\n=== Ops Tests ==="
  Ops.test

main :: IO ()
main = test
