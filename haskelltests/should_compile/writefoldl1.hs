foldl1' f [x] = x
foldl1' f (x:y:xs) = let firstfold = f x y
                     in  foldl1' f (firstfold:xs)