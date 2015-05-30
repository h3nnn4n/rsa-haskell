module Rsa where

import ExtendedEuclides
import Prime

genKeys k = [(p,q),(e ,p*q),(d,p*q)]
    where
        e = findPrime 1 32
        p = findPrime k 32
        q = findPrime k 32
        d = extendedEuclid (getPhi p q) e
        --d = dropWhile (\x -> (extendedEuclid (getPhi p q) e) == 0) [1..]
            where
                getPhi p q = (p-1)*(q-1)

cypher m (e, n) = powm m e n 1

decypher m (d, n) = powm m d n 1

--getPhi p q = (p-1)*(q-1)

