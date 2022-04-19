result :: String -> [String]
result [x] = [x : "_"]
result [] = []
result (x1 : x2 : xs) = [x1, x2] : result xs