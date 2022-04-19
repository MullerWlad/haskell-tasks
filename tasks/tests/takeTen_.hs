data Go = N | S | W | E | Null deriving (Show, Eq)
type Vector a = (a, a)

(<++>) :: (Num a, Ord a, Eq a) => Vector a -> Vector a -> Vector a
(a, b) <++> (x, y) = (a + x, b + y)

fromDirection :: (Num a, Eq a, Ord a) => Go -> Vector a
fromDirection N = (1, 0)
fromDirection S = (-1, 0)
fromDirection E = (0, 1)
fromDirection W = (0, -1)
fromDirection Null = (0, 0)

fromVector :: (Num a, Eq a, Ord a) => Vector a -> Maybe Go
fromVector (0, 0) = Just Null
fromVector (x, 0)
    | x > 0 = Just N
    | x < 0 = Just S 
fromVector (0, x)
    | x > 0 = Just E
    | x < 0 = Just W
fromVector (x, y) = Nothing 

result :: [Go] -> Bool
result = (== Just Null ) . fromVector . foldl1 (<++>) . map fromDirection
