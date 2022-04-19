result :: Eq a => [a] -> [a]
result list = [x | x <- list, (length $ filter (== x) list) > 1]