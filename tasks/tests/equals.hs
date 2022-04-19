result :: (Num a, Eq a) => a -> [a] -> [a] -> a
result _ _ [] = -1
result n header (x : xs)
    | sum header == sum xs = n
    | otherwise = result (n + 1) (x : header) xs

-- result 0 [] somelist
