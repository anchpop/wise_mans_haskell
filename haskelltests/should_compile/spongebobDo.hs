spongebob =
    putStrLn "Are you ready, kids?" >>= \_ ->
    getLine                         >>= \first ->
    putStrLn "I can't hear you!"    >>= \_ ->
    getLine                         >>= \second ->
    putStrLn "Ohhhhh!"              >>= \_ ->
    putStrLn ("You said '" ++ first ++ "' the first time, and '" ++ second ++ "' the second time")
