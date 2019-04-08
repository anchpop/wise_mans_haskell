data Currency = Dollars Double | Yen Double | Euros Double     deriving (Show)

convertToDollars :: Currency -> Currency
convertToDollars (Dollars d) = Dollars d
convertToDollars (Yen y)     = Dollars (y * 0.0089)
convertToDollars (Euros e)   = Dollars (e * 1.14)
