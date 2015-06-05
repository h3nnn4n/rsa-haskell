import System.Environment
import System.IO
import System.IO.Error

import Prime

primGen n size k
    | n > 1     = [findPrime size k] ++ primGen (n-1) size k
    | otherwise = [findPrime size k]

show_numbers [n]    = print n
show_numbers (x:xs) =
    do print x
       show_numbers xs

main = do args <- getArgs
          let numbers = primGen (read (args !! 0) :: Int) (read (args !! 1) :: Integer) (read (args !! 2) :: Int)

          show_numbers numbers

          return ()
