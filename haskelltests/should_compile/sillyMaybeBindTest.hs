result = 
    Just 0        >>= \x ->
    Just (x + 1)  >>= \x ->
    Just (x * 3)  >>= \x ->
    Just (show x) >>= \x ->
    Just ("And the answer is " ++ x)