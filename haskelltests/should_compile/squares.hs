squares :: (Num a) => [a] -> [a]
squares [] = []
squares (x:xs) = (x^2):(squares xs)