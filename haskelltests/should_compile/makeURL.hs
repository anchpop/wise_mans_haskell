makeURL :: String -> String -> String
makeURL domain path = 
    let full = protocol ++ "://" ++ address
        protocol = "http"
        address = domain ++ "/" ++ path
    in full