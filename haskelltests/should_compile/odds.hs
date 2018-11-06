odds :: (Integral a) => [a] -> [a]
odds [] = []
odds (x:xs) = if (x `mod` 2) /= 0 
                then (x):(odds xs)
                else odds xs