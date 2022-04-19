result :: (Ord a, Num a) => a -> a -> a -> Bool
result a b c = (a < b + c) && (b < a + c) && (c < a + b)
