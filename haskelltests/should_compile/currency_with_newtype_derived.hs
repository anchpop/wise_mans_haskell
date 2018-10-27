{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Dollar = Dollar Double  deriving (Read, Show, Num)
newtype Euro = Euro Double      deriving (Read, Show, Num)
newtype Yen = Yen Double        deriving (Read, Show, Num)