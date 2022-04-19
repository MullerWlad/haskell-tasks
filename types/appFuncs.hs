{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
lift2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b


lift1 :: (Applicative f) => (a -> b) -> f a -> f b
lift1 f a = f <$> a


-- let's get Just [3, 4] from Just 3, Just [4]
list :: Maybe [Integer]
list = (:) <$> Just 3 <*> Just [4]
-- because (:) :: a -> [a] -> [a]
    -- <$> :: (a -> b) -> f a -> f b
        -- (:) <$> :: f a -> f( [a] -> [a] )
        -- (:) <$> Just 3 :: f ( [a] -> [a] )
        -- <*> :: f( a -> b ) -> f a -> f b
            -- (:) <$> Just 3 <*> :: f [a] -> f [a]
                -- (:) <$> Just 3 <*> Just [4] :: f [a]


-- let's get [Just x] -> Just [x]
sequence' :: Applicative f => [f a] -> f [a]
sequence' [] = pure []
sequence' (x : xs) = (:) <$> x <*> sequence' xs
-- sequence' == sequenceA
-- sequenceA [(+3),(+2),(+1)] 3 == [6, 5, 4]
-- sequenceA [Just 3, Nothing, Just 1] == Nothing
