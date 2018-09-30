eitherDivide :: Double -> Double -> Either String Double
eitherDivide _ 0 = Left "Sorry, can't divide by zero"
eitherDivide x y = Right (x / y)