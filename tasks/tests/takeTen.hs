{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Go a = N a | 
            S a | 
            E a | 
            W a | 
            NE a a | 
            NW a a | 
            SE a a | 
            SW a a | 
            Stop deriving (Show)

-- Int is Z group => Go Int is Abel group, because Go Int is vector space => 
    -- Go Int is Monoid => Go Int is Semigroup

repr :: (Num a, Eq a, Ord a) => Go a -> Go a
repr (N a)
    | a < 0 = S (-a)
    | a == 0 = Stop
    | a > 0 = N a
repr (S a)
    | a < 0 = N (-a)
    | a == 0 = Stop
    | a > 0 = S a
repr (E a)
    | a < 0 = W (-a)
    | a == 0 = Stop
    | a > 0 = E a
repr (W a)
    | a < 0 = E (-a)
    | a == 0 = Stop
    | a > 0 = W a
repr (NW a b)
    | a < 0 && b < 0 = SE (-a) (-b)
    | a < 0 && b == 0 = S (-a)
    | a < 0 && b > 0 = SW (-a) b
    | a == 0 && b < 0 = E (-b)
    | a == 0 && b == 0 = Stop
    | a == 0 && b > 0 = W b
    | a > 0 && b < 0 = NE a (-b)
    | a > 0 && b == 0 = N a
    | a > 0 && b > 0 = NW a b
repr (NE a b)
    | a < 0 && b < 0 = SW (-a) (-b)
    | a < 0 && b == 0 = S (-a)
    | a < 0 && b > 0 = SE (-a) b
    | a == 0 && b < 0 = W (-b)
    | a == 0 && b == 0 = Stop
    | a == 0 && b > 0 = E b
    | a > 0 && b < 0 = NW a (-b)
    | a > 0 && b == 0 = N a
    | a > 0 && b > 0 = NE a b
repr (SW a b)
    | a < 0 && b < 0 = NE (-a) (-b)
    | a < 0 && b == 0 = N (-a)
    | a < 0 && b > 0 = NW (-a) b
    | a == 0 && b < 0 = E (-b)
    | a == 0 && b == 0 = Stop
    | a == 0 && b > 0 = W b
    | a > 0 && b < 0 = SE a (-b)
    | a > 0 && b == 0 = S a
    | a > 0 && b > 0 = SW a b
repr (SE a b)
    | a < 0 && b < 0 = NW (-a) (-b)
    | a < 0 && b == 0 = N (-a)
    | a < 0 && b > 0 = NE (-a) b
    | a == 0 && b < 0 = W (-b)
    | a == 0 && b == 0 = Stop
    | a == 0 && b > 0 = E b
    | a > 0 && b < 0 = SW a (-b)
    | a > 0 && b == 0 = S a
    | a > 0 && b > 0 = SE a b

instance (Num a, Eq a, Ord a) => Semigroup (Go a) where
    -- here to Go 

instance (Num a, Eq a, Ord a) => Monoid (Go a) where
    -- here to Go

-- result :: (Num a, Eq a, Ord a) => [Go a] -> Go a
-- result = (== Stop) . mconcat

-- too lazy to do it
