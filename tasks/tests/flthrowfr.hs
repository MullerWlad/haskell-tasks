-- define foldl throw foldr
foldlR :: (a -> a -> a) -> a -> [a] -> a
foldlR _ x [] = x
foldlR f n list = foldr (flip f) n $ reverse list
