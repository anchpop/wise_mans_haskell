wontWork = do
    putStrLn "hello"
    two <- (1 + 1) :: Int -- error! "Couldn't match expected type `IO a0' with actual type `Int'""
    putStrLn . show $ two