result = do 
    x <- pure 0
    x <- pure (x + 1)
    x <- pure (x * 3)
    x <- pure (show x)
    Just ("And the answer is " ++ x)