result = do 
    x₁ <- pure 0
    x₂ <- pure (x₁ + 1)
    x₃ <- pure (x₂ * 3)
    x₄ <- pure (show x₃)
    y₁ <- Nothing         -- This will cause the whole result function to evaluate to Nothing.
    Just ("And the answer is " ++ x₄)
