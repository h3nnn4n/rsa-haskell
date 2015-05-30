module Prime where

import System.Random
import Control.Monad
import System.IO.Unsafe -- Whoops

keyMaker :: Int -> Integer -> [Integer]
keyMaker k n = unsafePerformIO (keyMakerNoob k (n-1))

keyMakerNoob :: Int -> Integer -> IO [Integer]
keyMakerNoob k n = do g <- newStdGen
                      return $ take k (randomRs (10^(n), 10^(n+1)-1) g)

findPrime :: Integer -> Int -> Integer
findPrime b k = head (dropWhile (\x -> not (millerRabin x k)) (dropWhile (\x -> x < 10^(b-1)) (keyMaker 10000000000000000 b)))

millerRabin :: Integer -> Int -> Bool
millerRabin n k = and $ map (maybePrime n) (witnesses k n)

maybePrime :: Integer -> Integer -> Bool
maybePrime n a = nope2 (iter a (factor2 (n-1)) n) n

teste :: Integer ->Integer -> [Integer]
teste n a = iter a (factor2 (n-1)) n

factor2 :: Integer -> (Integer, Integer)
factor2 n = f2 n 0
    where
    f2 n s | even n    = f2 (n `div` 2) (s + 1)
           | otherwise = (n,s)

congruente :: Integer -> Integer -> Integer -> Bool
congruente p q n
    | (p - q) `mod` n == 0    = True
    | otherwise             = False

iter :: Integer -> (Integer, Integer) -> Integer -> [Integer]
iter a (k, d) n = map (\x -> power a (k*x, 1) n) [ 2^x | x <- [0, 1 .. d]]

is_one :: Integer -> Bool
is_one 1 = True
is_one _ = False

power :: Integer -> (Integer, Integer) -> Integer -> Integer
--power a (d, k) n = (a^d) `mod` n
power a (d, k) n = powm a d n 1

-- Got from rosetta code modular exponentiation page
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

has_one :: [Integer] -> Bool
has_one [] = False
has_one ys = h2 ys where
    h2 (x:xs)
        | is_one x      = True
        | otherwise     = has_one xs

nope :: [Integer] -> Integer -> Bool
nope (x:xs) n
     -- | head xs == 1  = (congruente (x^2) (-1) n)
     | head xs == 1  = (congruente (x^2) (-1) n) || (congruente (x^2) (1) n)
     | otherwise     = nope xs n

nope2 :: [Integer] -> Integer -> Bool
nope2 xs n
    | has_one xs    = nope xs n 
    | otherwise     = False

witnesses :: Int -> Integer -> [Integer]
witnesses k n = unsafePerformIO (witnessesNoob k n)

witnessesNoob :: Int -> Integer -> IO [Integer]
witnessesNoob k n = do g <- newStdGen
                       return $ take k (randomRs (2,n-1) g)

