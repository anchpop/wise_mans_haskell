headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just (head xs)