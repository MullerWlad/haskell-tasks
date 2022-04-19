--instance Functor Maybe where  
    --fmap func (Just val) = Just (func val)
    --fmap func Nothing = Nothing

--instance Functor ((->) r) where --> ((->) r) is constructor of functions
    --fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
        --in other words fmap :: (a -> b) -> (r -> a) -> (r -> b)
    --fmap f g = f . g
        --in other words fmap = (.)
--fmap (f . g) = fmap f . fmap g
    -- in other words fmap (f . g) x = fmap f (fmap g x)

-- The synonymus of fmap is <$> 
    -- fmap f g == f . g == f <$> g
    -- fmap (+3) [1, 2, 3] == (+3) <$> [1, 2, 3] == [4, 5, 6]

--now about applicative

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
-- for example f is [] --> fmap :: (a -> b) -> [a] -> [b]
    -- fmap g [] = []
    -- fmap g (x : xs) = (g x) : fmap xs

class (Functor' f) => Applicative' f where
    pure' :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b    -- is equal  f (a -> b) -> (f a -> f b)
                                          -- <**> is <*> in Aplicative

--have an operator <*>
--fmap (*) Just 3 == Just (*3) == (*) <*> Just 3
a :: Maybe Integer
a = Just (+3) <*> Just 4

-- [(*2), (+3)] <*> [1, 2, 3] == [2, 4, 6, 4, 5, 6]
-- Just (+) <*> Just 3 <*> Just 5 == Just 8 == (Just (+) <*> Just 3) <*> Just 5
    -- :t (+) == Num a => a -> a -> a
    -- :t (+3) == Num a => a -> a
    -- :t (5 + 3) == Num a => a

--have a <$>
--(+) <$> Just 4 == Just (+4)
--b :: Maybe (Integer -> Integer)
b :: [Integer -> Integer]
b = (+) <$> [4]

b' :: Maybe (Integer -> Integer)
b' = (*) <$> Just 4

-- f <$> x <*> y <*> z == ((f <$> x) <*> y) <*> z
-- (++) <$> Just "джонтра" <*> Just "волта" == Just "джонтраволта"
-- (+) <$> Just 3 == Just (+) <*> Just 3 == Just (+3)


-- <$> for functions
-- ((*) <$> (+3)) 10 3 = (10 + 3) * 3
-- ((-) <$> (+3)) 10 3 = (10 + 3) - 3


-- functions in applicative
-- instance Applicative ((->) r) where
        -- pure x = (\_ -> x)
        -- f <*> g = \x -> f x (g x) 
-- from applicative f, g :: ((->) r)
-- <*> :: f (a -> b) -> f a -> f b
    -- <*> :: (r -> a -> b) -> (r -> a) -> (r -> b)
        -- f <*> g :: { x :: r, f x :: a -> b, g x :: a => f x (g x) :: b } :: r -> b
        -- (f <*> g) = \x -> f x (g x)

-- for example
-- (\x y z -> [x, y, z]) <$> (+3) <*> (*2) <*> (/2) $ 5 == [8.0, 10.0, 2.5]

-- applicative rules:
-- pure f <*> x = (pure f) <*> x = fmap f x
    -- because f :: a -> b, pure :: app (a -> b)
        -- x :: app a, 
-- pure id <*> x = x
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- pure (.) :: f( (b -> c) -> (a -> b) -> (a -> c) )
-- pure f <*> pure x = pure f x
    -- pure f :: app f 
    -- pure x :: app x
    -- <*> :: app (a -> b) -> app a -> app b
        -- x :: a
        -- f x :: b
-- u <*> pure y = pure ($ y) <*> u
    -- <*> :: f (a -> b) -> f a -> f b 
    -- pure y :: f a --> y :: a
        -- u :: f (a -> b)
            -- pure ($ y) :: f ( (a -> b) -> b )
                -- u <*> pure y :: f b
                -- pure ($ y) <*> u :: f b

-- max <$> Just 2 <*> Just 3 == Just (max) <*> Just 2 <*> Just 3