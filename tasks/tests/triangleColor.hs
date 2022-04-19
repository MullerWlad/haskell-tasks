data Color = R | G | B deriving (Show, Eq)
(<++>) :: Color -> Color -> Color
R <++> G = B
R <++> B = G
G <++> B = R
x <++> y
    | x == y = x
    | otherwise = y <++> x

type Colors = [Color]

triangle :: Colors -> [Colors]
triangle [] = []
triangle colors = colors : triangle (newColors colors)
    where newColors [] = []
          newColors [c] = newColors []
          newColors (c1 : c2 : cs) = (c1 <++> c2) : newColors (c2 : cs)
