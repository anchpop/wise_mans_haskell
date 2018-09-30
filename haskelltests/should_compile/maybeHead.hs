maybeHead :: [a] -> Maybe a
maybeHead (x:xs) = Just x
maybeHead _ = Nothing