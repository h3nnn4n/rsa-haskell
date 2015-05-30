import Rsa

import Data.Char
import Data.List
import Data.List.Split
import System.Environment
import System.IO
import System.IO.Error

purify xs = filter (\x -> not (x == '(' || x == ')')) xs

padd :: [String]->[Int]
padd x = map (foldl (\a b -> 1000 * a + b) 0) enc_blocks
    where enc_blocks = map (map ord) x

decode :: Int->[Int]
decode x
    | x == 0     = []
    | otherwise  = (decode (x `quot` 1000)) ++ ((x `mod` 1000) : [])

main = do
    args  <- getArgs
    input <- readFile (head args)
    keys  <- readFile ((args !! 1) ++ "_pub.key")
    print keys
    print input
    let blocks = splitEvery 6 input
        padded = padd blocks
        key = splitOn ";" keys
        enc_blocks = map (\m -> cypher m ((read(key!!0)::Integer),(read(key!!1)::Integer))) (map toInteger padded)
        enc_file   = foldl (\a b -> a ++ (show b) ++ "/") "" enc_blocks
        --decoded = map decode padded
        --decoded_strings=map (foldl (\a b->a++((chr b):[])) "") decoded
        --decrypted_file=foldl (\a b-> a++b) "" decoded_strings
    --print $ show padded
    --print $ show enc_blocks
    --print $ show enc_file
    --print $ show decrypted_file
    --writeFile "huehue.dats.tads" decrypted_file
    writeFile (args !! 2) enc_file
    return ()

