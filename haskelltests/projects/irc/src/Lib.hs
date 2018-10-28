module Lib
    ( someFunc,
      isValidChannelName -- We also want to export isValidChannelName, so we add it here.
    ) where



isValidChannelName :: String -> Bool
isValidChannelName [] = False
isValidChannelName s
   | (head s) /= '#' = False
   | (elem ' ' s)    = False
   | otherwise       = True

someFunc :: IO ()
someFunc = putStrLn "someFunc"