module CodeMe.Playfair (
    playfair,
    usePlayfair, 
    nexttick
    ) where
        
import Data.List (intersect)


-- multiplex point
data Point = Point {
    point :: Char ,
    column :: String ,
    line :: String
} deriving (Show)

-- multitable
type Multitable = [ Point ]

-- matrix
matrix :: Multitable
matrix = [
    Point {point = 'p', line = "playf", column = "pibjt"} ,
    Point {point = 'l', line = "playf", column = "lrcku"} ,
    Point {point = 'a', line = "playf", column = "aednv"} ,
    Point {point = 'y', line = "playf", column = "yxgow"} ,
    Point {point = 'f', line = "playf", column = "fmhsz"} ,
    Point {point = 'i', line = "irexm", column = "pibjt"} ,
    Point {point = 'r', line = "irexm", column = "lrcku"} ,
    Point {point = 'e', line = "irexm", column = "aednv"} ,
    Point {point = 'x', line = "irexm", column = "yxgow"} ,
    Point {point = 'm', line = "irexm", column = "fmhsz"} ,
    Point {point = 'b', line = "bcdgh", column = "pibjt"} ,
    Point {point = 'c', line = "bcdgh", column = "lrcku"} ,
    Point {point = 'd', line = "bcdgh", column = "aednv"} ,
    Point {point = 'g', line = "bcdgh", column = "yxgow"} ,
    Point {point = 'h', line = "bcdgh", column = "fmhsz"} ,
    Point {point = 'j', line = "jknos", column = "pibjt"} ,
    Point {point = 'k', line = "jknos", column = "lrcku"} ,
    Point {point = 'n', line = "jknos", column = "aednv"} ,
    Point {point = 'o', line = "jknos", column = "yxgow"} ,
    Point {point = 's', line = "jknos", column = "fmhsz"} ,
    Point {point = 't', line = "tuvwz", column = "pibjt"} ,
    Point {point = 'u', line = "tuvwz", column = "lrcku"} ,
    Point {point = 'v', line = "tuvwz", column = "aednv"} ,
    Point {point = 'w', line = "tuvwz", column = "yxgow"} ,
    Point {point = 'z', line = "tuvwz", column = "fmhsz"}
    ]


-- solver in diag
solverDia :: Point -> Point -> Multitable
solverDia p1 p2 = [
    Point { point = head $ intersect (line p1) (column p2) , 
            line = line p1, 
            column = column p2 },
    Point { point = head $ intersect (column p1) (line p2), 
            line = line p2, 
            column = column p1 }
    ]

-- findnext
nexttick :: String -> Char -> Char
nexttick str chr
    | reverse str == chr : (tail $ reverse str) = head str
    | otherwise = help str
        where help [] = '_'
              help str
                | chr == head str = head $ tail str
                | otherwise = help $ tail str

-- solver
solver :: Point -> Point -> Multitable
solver p1 p2
    | line p1 == line p2 = [
            Point {
                point = nexttick (line p1) (point p1),
                line = line p1,
                column = column p1
                },
            Point {
                point = nexttick (line p2) (point p2),
                line = line p2,
                column = column p2
                }
        ]
    | column p1 == column p2 = [
            Point {
                point = nexttick (column p1) (point p1),
                line = line p1,
                column = column p1
                },
            Point {
                point = nexttick (column p2) (point p2),
                line = line p2,
                column = column p2
                }
        ]
    | otherwise = solverDia p1 p2

-- translater
translater :: Multitable -> String
translater xs = [ point ] <*> xs

playfair :: Char -> Char -> String
playfair l1 l2 = translater $ solver (head [x | x <- matrix, point x == l1]) 
                                     (head [x | x <- matrix, point x == l2])

-- getString
usePlayfair :: String -> String
usePlayfair [] = []
usePlayfair [x] = [x]
usePlayfair (x : y : xs) = head t : usePlayfair (head (tail t) : xs)
    where t = playfair x y
