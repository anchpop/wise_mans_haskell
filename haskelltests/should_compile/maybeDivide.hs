maybeDivide :: Double -> Double -> Maybe Double -- Maybe Double is a type. 
                                                -- It's automatically made from the definition of Maybe a. 
                                                -- It can be Nothing or Just Double.
maybeDivide _ 0 = Nothing
maybeDivide x y = Just (x / y)