result :: (Ord a, Read a) => [String] -> (a, a)
result list = (minimum newList, maximum newList)
    where newList = read <$> list
