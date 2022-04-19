summ :: Num a => a -> (a -> a) -> a
summ x square = x + square x

summ2 :: Num a => a -> (a -> a)
summ2 x y = x + y

summ3 :: Num a => a -> (a -> (a -> a))
summ3 a b c = a + b + c

summ3' :: Num a => a -> a -> a
summ3' = summ3 1 -- 1 + ...

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['А'..'Я'])

-- rev :: (a -> b -> c) -> (b -> a -> c)
-- rev :: (a -> b -> c) -> (b -> (a -> c))
rev :: (a -> b -> c) -> b -> a -> c
-- rev f = g
    -- where g x y = f y x
rev f y x = f x y