spongebob = do
    putStrLn "Are you ready, kids?"
    first <- getLine
    putStrLn "I can't hear you!"
    second <- getLine
    putStrLn "Ohhhhh!"
    putStrLn ("Results: You said '" ++ first ++ "' the first time, and '" ++ second ++ "' the second time")
