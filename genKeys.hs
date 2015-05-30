import Rsa

import System.Environment
import System.IO
import System.IO.Error

wrapper k = return $ genKeys k

main = do
    args <- getArgs
    keys <- wrapper (read (args !! 1) :: Integer)
    --print $ head keys
    --print $ keys !! 1
    let name = args !! 0
        pubName = name ++ "_pub.key"
        privName = name ++ "_priv.key"
    print pubName
    print privName
    writeFile pubName $ show $ head keys
    writeFile privName $ show $ head keys
    return ()

