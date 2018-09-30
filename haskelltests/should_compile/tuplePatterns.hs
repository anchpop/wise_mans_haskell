unost :: (a, b, c) -> a
unost (a, _, _) = a

dost :: (a, b, c) -> b
dost (_, b, _) = b

trest :: (a, b, c) -> c
trest (_, _, c) = c