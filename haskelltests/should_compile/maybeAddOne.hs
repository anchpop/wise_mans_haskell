maybeAddOne :: Maybe Int -> Maybe Int
maybeAddOne (Just x) = Just (x + 1)
maybeAddOne Nothing = Nothing

maybeHead :: [a] -> Maybe a
maybeHead (x:xs) = Just x
maybeHead _ = Nothing