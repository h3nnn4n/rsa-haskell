import Rsa

import System.Environment
import System.IO
import System.IO.Error
import Data.Char
import Data.List
import Data.List.Split

decode :: Int->[Int]
decode x
    | x == 0     = []
    | otherwise  = (decode (x `quot` 1000)) ++ ((x `mod` 1000) : [])

main = do
    args  <- getArgs
    input <- readFile (head args)
    keys  <- readFile ((args !! 1) ++ "_priv.key")
    let key = splitOn ";" keys
        blocks = splitOn ";" input
        int_blocks = map (\x -> read x :: Integer) (init blocks)
        dec_blocks = map fromInteger $ map (\m -> decypher m ((read(key!!0)::Integer),(read(key!!1)::Integer))) int_blocks

        dec_txt = map (foldl (\a b->a++((chr b):[])) "") $ map decode dec_blocks
        dec_file = foldl (\a b-> a++b) "" dec_txt

    writeFile (args !! 2) dec_file
    return ()

