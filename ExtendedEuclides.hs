module ExtendedEuclides where

extendedEuclid t d = ee t t t d 1
    where
        ee _ _ _ 0 _        = 0
        ee t a b 1 d
            | d >=0         = d
            | otherwise     = (d `mod` t)
        ee t a b c d
            | d < 0         = ee t a b c (d `mod` t)
            | otherwise     = ee t c d (a - (a `div` c) * c) (b - (a `div` c) * d)

extendedEuclid2 t d = ee t t t d 1
    where
        ee t _ _ 0 _        = [(0, 0, 0, 0)]
        ee t a b 1 d
            | d < 0         = [(a, b, 1, d `mod` t)]
            | otherwise     = [(a, b, 1, d)]
        ee t a b c d
            | d < 0         = ee t a b c (d `mod` t)
            | otherwise     = [(a,b,c,d)] ++ ee t c d (a - (a `div` c) * c) (b - (a `div` c) * d)
