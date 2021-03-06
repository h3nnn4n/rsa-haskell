module Prime where

import System.Random
import Control.Monad
import System.IO.Unsafe -- ┌(° o °)┘ DANCE! └(° o °)┐

import Math.NumberTheory.Moduli

findPrime :: Integer -> Int -> Integer
findPrime b k = head (dropWhile (\x -> not (millerRabin x k)) (keyMaker 10000000000000000 b))

millerRabin :: Integer -> Int -> Bool
millerRabin _ 0 = False
millerRabin 2 _ = True
millerRabin n k
    | k == 0    = False
    | n == 2    = True
    | n < 2     = False
    | otherwise = and $ map (maybePrime n) (witnesses k n)

maybePrime :: Integer -> Integer -> Bool
maybePrime n a = satisfy (iter a (factor2 (n-1)) n) n

factor2 :: Integer -> (Integer, Integer)
factor2 n = f2 n 0
    where
    f2 n s | even n    = f2 (n `div` 2) (s + 1)
           | otherwise = (n,s)

iter :: Integer -> (Integer, Integer) -> Integer -> [Integer]
iter a (k, d) n = map (\x -> powerMod a (k*x) n) [ 2^x | x <- [0, 1 .. d]]

-- Given a list from the Miller-Rabin primality test steps
-- finds out if the term before 1 squared is congruent to +/- one
satisfy :: [Integer] -> Integer -> Bool
satisfy    [_] n = False
satisfy (x:xs) n
     | head xs == 1  = (((x^2) - (-1)) `mod` n == 0) || (((x^2) - 1) `mod` n == 0)
     | otherwise     = satisfy xs n

--      --
-- JUNK --
--      --
----------------------------------------------------------------------------------------

witnesses :: Int -> Integer -> [Integer]
witnesses k n = unsafePerformIO (witnessesNoob k n)

keyMaker :: Int -> Integer -> [Integer]
keyMaker k n = unsafePerformIO (keyMakerNoob k (n-1))

witnessesNoob :: Int -> Integer -> IO [Integer]
witnessesNoob k n = do g <- newStdGen
                       return $ take k (randomRs (2,n-1) g)

keyMakerNoob :: Int -> Integer -> IO [Integer]
keyMakerNoob k n = do g <- newStdGen
                      return $ take k (randomRs (2^(n-1), 2^(n)-1) g)
