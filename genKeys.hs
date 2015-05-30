import Rsa

import System.Environment
import System.IO
import System.IO.Error

wrapper k = return $ genKeys k

main = do
    -- Gets the arguments, first one is the name for keyfile and the second is the key size
    args <- getArgs
    keys <- wrapper (read (args !! 1) :: Integer)
    let name = args !! 0
        pubName = name ++ "_pub.key"
        privName = name ++ "_priv.key"
    print pubName
    print privName
    writeFile pubName  $ show ( head $ map (\(x,y) -> x) keys) ++ ";" ++ show ( head $ map (\(x,y) -> y) keys)
    writeFile privName $ show ( last $ map (\(x,y) -> x) keys) ++ ";" ++ show ( last $ map (\(x,y) -> y) keys)
    --writeFile pubName  $ show $ head keys
    --writeFile privName $ show $ last keys
    return ()

