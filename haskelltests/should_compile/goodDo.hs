willWork = do
    putStrLn "hello"
    two <- pure ((1 + 1) :: Int)
    putStrLn . show $ two