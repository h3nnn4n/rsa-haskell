import Rsa

import System.Environment
import System.IO
import System.IO.Error

wrapper k = return $ genKeys k

main = do
    args <- getArgs
    keys <- wrapper (read (args !! 1) :: Integer)
    let name = args !! 0
        pubName = name ++ "_pub.key"
        privName = name ++ "_priv.key"
    print pubName
    print privName
    writeFile pubName  $ show ( head $ map (\(x,y) -> x) keys) ++ ";" ++ show ( head $ map (\(x,y) -> y) keys)
    writeFile privName $ show ( last $ map (\(x,y) -> x) keys) ++ ";" ++ show ( last $ map (\(x,y) -> y) keys)
    return ()
