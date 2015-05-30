import Rsa

import Data.Char
import Data.List
import Data.List.Split
import System.Environment
import System.IO
import System.IO.Error

padd :: [String]->[Int]
padd x = map (foldl (\a b -> 1000 * a + b) 0) enc_blocks
    where enc_blocks = map (map ord) x

main = do
    args  <- getArgs
    input <- readFile (head args)
    keys  <- readFile ((args !! 1) ++ "_pub.key")
    let blocks = splitEvery 6 input
        padded = padd blocks
        key = splitOn ";" keys
        enc_blocks = map (\m -> cypher m ((read(key!!0)::Integer),(read(key!!1)::Integer))) (map toInteger padded)
        enc_file   = foldl (\a b -> a ++ (show b) ++ ";") "" enc_blocks
    writeFile (args !! 2) enc_file
    return ()

