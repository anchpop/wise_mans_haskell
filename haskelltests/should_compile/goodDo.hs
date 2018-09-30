willWork = do
    putStrLn "hello"
    two <- return ((1 + 1) :: Int) -- error! "Couldn't match expected type `IO a0' with actual type `Int'""
    putStrLn . show $ two