length' :: (Integral b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs