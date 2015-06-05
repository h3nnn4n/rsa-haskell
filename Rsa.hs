module Rsa where

import ExtendedEuclides
import Prime

import Math.NumberTheory.Moduli

--genKeys k = [(p, q), (e,p * q), (d, p * q)]
genKeys k = [(e, p * q) , (d, p * q)]
    where
        e
         | k < 16    = findPrime (k-1) 32
         | k <= 4    = error "too small"
         | otherwise = findPrime 16 32
        p = findPrime k 32
        q = findPrime k 32
        d = extendedEuclid (getPhi p q) e
            where
                getPhi p q = (p - 1) * (q - 1)

cypher m (e, n) = powerMod m e n

decypher m (d, n) = powerMod m d n
