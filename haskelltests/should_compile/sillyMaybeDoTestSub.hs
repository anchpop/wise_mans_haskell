result = do 
    x₁ <- Just 0
    x₂ <- Just (x₁ + 1)
    x₃ <- Just (x₂ * 3)
    x₄ <- Just (show x₃)
    Just ("And the answer is " ++ x₄)