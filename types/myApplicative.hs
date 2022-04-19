
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

class Functor' f => Applicative' f where
    pure' :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b
    (<$$>) :: (a -> b) -> f a -> f b

data Atom content = Core content | Empty deriving(Show)

instance Functor' Atom where
    fmap' f Empty = Empty
    fmap' f (Core a) = Core $ f a

instance Applicative' Atom where
    pure' = Core
    Empty <**> _ = Empty
    Core f <**> something = fmap' f something
    f <$$> something = fmap' f something

-- Core (+) <**> Core 3 <**> Core 4 == 7
-- (+) <$$> Core 3 <**> Core 4 == 7


-- give applicative for []
instance Functor' [] where
    fmap' f [] = []
    fmap' f (x : xs) = f x : fmap' f xs

instance Applicative' [] where
    pure' a = [a]
    [] <**> _ = []
    (f : fs) <**> something = fmap' f something ++ (fs <**> something) 
    f <$$> something = fmap' f something

-- [(+), (*)] <**> [2, 3] <**> [5, 6] == [7, 8, 8, 9, 10, 12, 15, 18]
-- (+) <$$> [1, 2, 3] <**> [2, 4] == [3,5,4,6,5,7]

-- Lets find all multiplications more than 50
-- filter (>50) $ (*) <$$> [2, 5, 10] <**> [8, 10, 11]
