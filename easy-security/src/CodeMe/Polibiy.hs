module CodeMe.Polibiy (
    polibiy ,
    hasLetter
    ) where
import CodeMe.Playfair (nexttick)


matrix :: [String]
matrix = [
    "pibjt",
    "lrcku",
    "aednv",
    "yxgow",
    "fmhsz"
    ]

hasLetter :: Char -> String -> Bool 
hasLetter _ [] = False
hasLetter y (x : xs)
    | y == x = True
    | y /= x = hasLetter y xs
    | otherwise = False

polibiy :: String -> String
polibiy [] = []
polibiy str@(x : xs) = nexttick column x : polibiy xs
    where column = head [y | y <- matrix, hasLetter x y] 
