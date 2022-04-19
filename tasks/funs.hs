-- sum of all natural < 1000 , :3, :5
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


import Data.Map (Map)
import Data.Char


a :: Int
a = sum [x | x <- [1..], x < 1000, rem x 3 == 0, rem x 5 == 0]


-- NOK [1..20]
b :: [Int]
b = [foldr1 lcm] <*> [[1..20]]


-- count of sollution x + y + z = n
c :: (Num a, Enum a, Eq a) => a -> Int
c n = length [(x, y, z) | x <- list, y <- list, z <- list, x + y + z == n]
    where list = [1..100]


-- analog break
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ [] = ([], [])
break' f list@( x : xs )
    | not $ f x =  ( [x], [] ) <#> break' f xs
    | otherwise = ([], list)
    where (a, b) <#> (a', b') = (a ++ a', b ++ b')


-- analog concatMap throw monad
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f list = list >>= f


-- analog curry
curry' :: ((a, b) -> c) -> a -> b -> c -- equal ((a, b) -> c) -> (a -> b -> c)
curry' f a b = f (a, b)


-- analog cycle
cycle' :: [a] -> [a]
cycle' list = list ++ cycle' list


-- analog drop
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' a ( x : xs ) = drop' ( a - 1 ) xs


-- analog elem
elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n ( x : xs )
    | n == x = True
    | otherwise = elem' n xs


-- analog even
even' :: Integral a => a -> Bool
even' n
    | mod n 2 == 0 = True
    | otherwise = False


-- analog flip
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f a b = f b a


-- lcm (NOK), gcd (NOD) analogs 
--lcm' :: (Fractional a, Integral a) => a -> a -> a
--lcm' x y = (x * y) / gcd x y

--gcd' :: (Fractional a, Integral a) => a -> a -> a
--gcd' x y = (x * y) / lcm x y


-- init analog
init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' ( x : xs ) = x : init' xs


-- analog iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f a = a : iterate' f (f a)


-- analog last
last' :: [a] -> a
last' [x] = x
last' ( x : xs ) = let help = last' in help xs


-- analog lookup
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ( (a, b) : ts )
    | x == a = Just b
    | otherwise = lookup' x ts


-- analog negate
negate' :: Num a => a -> a
negate' = \x -> -x


-- analog notElem, elem == elem'
notElem' :: Eq a => a -> [a] -> Bool
notElem' a = not . elem' a


-- analog null
null' :: [a] -> Bool
null' [] = True
null' _ = False


-- product analog
product' :: Num a => [a] -> a
product' = foldr1 (*)


-- zip, zipWith, zip3, zipWith3, unzip, unzip3
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' list = ([fst x | x <- list], [snd x | x <- list])


-- foldr, foldl, foldr1, foldl1 analog
foldr'' :: (a -> b -> b) -> b -> [a] -> b -- f a (f a (f a (f a b))) 
foldr'' f b [a] = f a b
foldr'' f b (a : as) = foldr'' f (f a b) as
-- (-) 3 [1, 2, 3, 4] -> 1 - (2 - (3 - (4 - 3)))
 
foldl'' :: (a -> b -> a) -> a -> [b] -> a -- f (f (f (f a b) b) b) b
foldl'' f a [b] = f a b
foldl'' f a (b : bs) = foldl'' f (f a b) bs
-- (-) 3 [1, 2, 3, 4] -> (((3 - 1) - 2) - 3) - 4

foldr1'' :: (a -> a -> a) -> [a] -> a
foldr1'' f (a : as) = foldr'' f a as

foldl1'' :: (a -> a -> a) -> [a] -> a
foldl1'' f (a : as) = foldl'' f a as


-- is palindrom
isPalindrom :: Eq a => [a] -> Bool
isPalindrom str = str == reverse str


-- how to make a process of negan counting in TWD?
type Person = String
type WordCount = String
neganCount :: [(Person, WordCount)]
neganCount = zip personStack wordStack
    where wordStack = ["eeny",
                               "meeny",
                               "miny",
                               "mo",
                               "catch",
                               "a tiger",
                               "by the",
                               "toe",
                               "if he",
                               "hollers",
                               "let him",
                               "go",
                               "eeny",
                               "meeny",
                               "miny",
                               "moe"]
          personStack = cycle $ reverse ["Glen",
                       "Rositta",
                       "Deryl",
                       "Mesean",
                       "Abraham",
                       "Maggie",
                       "Rick",
                       "Sasha",
                       "Faggot",
                       "Carl"]


-- length analog
myLength :: [a] -> Int
myLength = foldr (\ x -> (+) 1) 0

myLength' :: [a] -> Int
myLength' [] = 0
myLength' (x : xs) = 1 + myLength' xs


akkerman :: Integral a => a -> a -> a
akkerman 0 n = n + 1
akkerman m 0 = akkerman (m - 1) 1
akkerman m n = akkerman (m - 1) (akkerman m (n - 1))


collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n : collatz (div n 2)
    | otherwise = n : collatz (n * 3 + 1)


pills :: Int -> Int
pills n = foldr (+) 0 [1..n] + foldr (+) 0 [1..n - 1]


fib :: Integer -> [Integer]
fib cons = help 1 1
    where help n k
            | k < cons = n : help k (n + k)
            | otherwise = []

fib' :: (Eq a, Num a, Num p) => a -> p
fib' 0 = 0
fib' 1 = 1
fib' n = fib' (n - 1) + fib' (n - 2)

fib'' :: Integer -> Integer
fib'' n = help 1 1 n
    where help one two 1 = one
          help one two n = help two (one + two) (n - 1)


-- analog elem 2
myElem :: Eq a => a -> [a] -> Bool
myElem n = not . null . filter (\x -> x == n)


-- decart product
decart :: [a] -> [b] -> [(a, b)]
decart list1 list2 = [(a, b) | a <- list1, b <- list2]


-- Graph
data Oriented a b = Oriented {
    from :: a,
    to :: b
} deriving(Show)

newtype Simple a b = Simple {
    line :: (a, b)
} deriving(Show)

newtype Core a = Core a deriving(Show)

class Graph a where
    (<->) :: a -> a -> Simple a a
    (<->>) :: a -> a -> Oriented a a

instance Num a => Graph (Core a) where
    (Core m) <-> (Core n) = Simple {line = (Core m, Core n)}
    (Core m) <->> (Core n) = Oriented {from = Core m, to = Core n}


-- for fun
data List a = Empty | List a (List a) deriving(Show)
-- [2, 3, 5] == Cons 2 $ Cons 3 $ Cons 5 $ Empty

class Struct s where
    next :: s -> s
    current :: s -> s

instance Num a => Struct (List a) where
    next (List l list) = list
    next Empty = Empty
    current (List l list) = List l Empty
    current Empty = Empty

class Concated a where
    (<|>) :: a -> a -> a

instance Num a => Concated (List a) where 
    (List x Empty) <|> xs = List x xs
    Empty <|> Empty = Empty
    
instance Num a => Concated (Core a) where 
    Core m <|> Core n = Core (m * 10 + n)


-- look at infix
(<#->) :: Int -> Int -> Int
(<#->) = (-)

(<#*>) :: Int -> Int -> Int
(<#*>) = (*)

(<#+>) :: Int -> Int -> Int 
(<#+>) = (+)

infixr 6 <#-> -- try to change values
infixl 5 <#*>
infix 4 <#+>


dist :: (Double, Double) -> (Double, Double) -> Double 
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2


doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact ( n - 2 )


fibonacci :: Integer -> Integer
fibonacci 1 = 1
fibonacci 0 = 0
fibonacci n
    | n >= 0 = result
    | otherwise = ((-1) ^ (n + 1)) * result
    where result = fibonacci (n - 1) + fibonacci (n - 2)


fibonacci' :: Integer -> Integer
fibonacci' n
    | even n && n < 0 = - help 1 1 (-n)
    | n < 0 = help 1 1 (-n)
    | n == 0 = 0
    | otherwise = help 1 1 n
    where help one two 1 = one
          help one two n = help two (one + two) (n - 1)


seqA :: Integer -> Integer
seqA n = let
    help one _ _ 0 = one
    help _ two _ 1 = two
    help _ _ three 2 = three
    help one two three arg = help two three (three + two - 2 * one) (arg - 1)
    in help 1 2 3 n


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count number = let
    count _ 0 a = a - 1
    count number division a = count number (number `div` (10 ^ a)) (a + 1)
    counter = count (abs number) 1 1

    summer number 0 = number
    summer number counter = int + summer (number - onlyInt) (counter - 1)
        where int = number `div` (10 ^ counter)
              onlyInt = int * 10 ^ counter

    in (summer (abs number) counter, counter)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = sumTrap points
    where points = [a + step * line | step <- [0..999]]
          line = (b - a) / 1000
          sumTrap = sum . map (\x -> line * (f x + f (x + line)) / 2)


getSecondFrom :: a -> b -> c -> b
getSecondFrom _ b _ = b


-- nice function
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y
-- sumSq = (+) `on` (^2)
-- sumSq 2 3 = 12



class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"
    
instance Printable () where
    toString _ = "unit type"
    
instance (Printable a, Printable b) => Printable (a, b) where          -- 1
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

-- instance Printable a => Printable (a, a) where                       -- 2
-- does not work


avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger (toInteger a + toInteger b + toInteger c) / 3


-- redex, lazy !!!
-- strong analisys, strong functions, not strong functions
foo :: p -> p -- not strong, because undefined returns exeption
foo a = a

bar :: b -> p -> p -- not strong, because undefined returns exeption
bar = const foo

baz :: p -> b -> Bool -- strong, because undefined returns const True
baz x = const True

quux :: t -- not strong, because undefined returns exeption
quux = let x = x in x

corge :: String -- strong, because undefined returns error, not exeption
corge = "Sorry, my value was changed"

grault :: (Eq a, Num a) => p -> a -> p -- not strong, because undefined returns exeption
grault x 0 = x
grault x y = x

garply :: Integer -> Char -- not strong, because undefined returns exeption
garply = grault 'q'

waldo :: p -> p -- not strong, because undefined returns exeption
waldo = foo


-- NF, not NF, WHNF!!!
-- _|_ is for equation, that will never finished well or lazy finished, read more
-- seq leads first argument to WHNF

-- seq works with WHNF
-- seq 3 2 == 2, because 3 is NF
-- seq (\x -> undefined) 2 == 2, because (\x -> undefined) is WHNF
-- seq undefined 2 == exeption, because undefined is not NF

-- ($!) :: (a -> b) -> a -> b
-- f $! x = x seq f x

-- const 42 undefined == const 42 $ undefined == 42
-- const 42 $! undefined == exeption


oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = xs >>= \x -> case (odd x) of
    True -> [x]
    False -> []


readDigits :: String -> (String, String)
readDigits = span (`elem` "0123456789")


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f g = filter (\x -> f x || g x)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = [y | y <- xs, y <= x] ++ [x] ++ [y | y <- xs, y > x]


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes list = list >>= \x -> [x ^ 2, x ^ 3]


three :: Maybe [Char]
three = Just 3 >>= \x ->
        Just "!" >>= \y ->
        Just (show x ++ y)


-- all C
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = let 
    interleave [] = [[x]]
    interleave (y:ys) = (x:y:ys) : map (y:) (interleave ys)
    in [y | p <- perms xs, y <- interleave p]


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 a b c = foldl1 (zipWith max) [a, b, c]


-- infinite fibonacci
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream  (tail fibStream)


coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]
change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change amount = [ c : cs | c <- coins, amount >= c, cs <- change (amount - c) ]


lengthList :: [a] -> Int
lengthList = foldr (+) 0 . map (const 1)


sumOdd :: [Integer] -> Integer
sumOdd = (foldr (+) 0) . filter odd


evenOnly :: [a] -> [a]
evenOnly [] = []
evenOnly [x] = []
evenOnly (x : y : ls) = y : evenOnly ls


data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord ((toDouble x) + halfSize) ((toDouble y) + halfSize)
    where halfSize = size / 2
          toDouble = (* size) . fromInteger . toInteger

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (pos x - 1) (pos y - 1)
    where pos a = ceiling (a / size) -- round to convert Double to Int

