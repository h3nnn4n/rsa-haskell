import Rsa
import Prime

import System.Environment
import System.IO
import System.IO.Error

primeSieve = [2] ++ [ x | x <- [3,5..], millerRabin x 32]

getFakekeys n = q * p
    where
        p = findPrime n 32
        q = findPrime n 32

notBombeBrain n = (p, q)
    where
    p = head $ dropWhile (\x -> n `mod` x /= 0) primeSieve
    q = n `div` p

main = do
    args <- getArgs
    let size = args !! 0
        keys = getFakekeys $ read size :: Integer

    print $ keys
    print $ notBombeBrain keys

    return ()
