spongebob =
    putStrLn "Are you ready, kids?" >>= \_ -> (
    getLine                         >>= \_ -> (
    putStrLn "I can't hear you!"    >>= \_ -> (
    getLine                         >>= \_ -> (
    putStrLn "Ohhhhh!"))))