import System.Environment
import System.IO
import System.IO.Error

import Prime

main = do args <- getArgs
          let is_prime = millerRabin (read (args !! 0) :: Integer) (read (args !! 1) :: Int)

          print is_prime

          return ()
