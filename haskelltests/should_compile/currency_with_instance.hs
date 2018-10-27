data Dollar = Dollar Double  deriving (Read, Show)
data Euro = Euro Double      deriving (Read, Show)
data Yen = Yen Double        deriving (Read, Show)

-- This, but for every currency:
instance Num Dollar where
    (Dollar a) + (Dollar b) = Dollar (a + b) 
    (Dollar a) - (Dollar b) = Dollar (a - b) 
    (Dollar a) * (Dollar b) = Dollar (a * b) 
    negate (Dollar a) = Dollar (-a) 
    abs (Dollar a) = Dollar (abs a) 