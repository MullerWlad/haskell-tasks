{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
newtype Typ a = Typ a
-- is equal data Typ a = Typ a
-- new type is a special word to create type with only one constructor
-- it is quicker sollution to make own type


-- type used to create a synonim of type
-- newtype used to create quicker lazy own type with only one constructor
-- data used to create a multiple constructor type


-- definition of monoid like a usual monoid in algebra
class Monoid' m where
    mempty' :: m
    mappend' :: m -> m -> m
    mconcat' :: [m] -> m
    mconcat' = foldr mappend' mempty'

-- mempty' is e in monoid M = (M # e)
-- mappend' is # in moniod M = (M # e)
-- mconcat' need to (a, b, c) = abc, like in algebra of words

-- let's find some examples of Monoid
instance Monoid' Int where
    mempty' = 1
    mappend' = (*)

data Group = A | B | E deriving(Show)
instance Monoid' Group where
    mempty' = E
    mappend' A B = mempty'
    mappend' B A = mempty'
    mappend' a mempty' = a
    mappend' mempty' a = a
    mappend' A A = B
    mappend' B B = A

data Tree a = Empty | Node a (Tree a) (Tree a) deriving(Show)

-- let's slice two Tree's into one
instance Num a => Monoid' (Tree a) where 
    mempty' = Empty
    mappend' mempty' t = t
    mappend' t mempty' = t
    mappend' (Node x1 l1 r1) (Node x2 l2 r2) = Node (x1 + x2) (mappend' l1 l2) (mappend' r1 r2)
    