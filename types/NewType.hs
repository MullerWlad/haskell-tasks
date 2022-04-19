{-# OPTIONS_GHC -Wno-missing-fields #-}
module NewType
    (Point(..), Shape(..), area)
where

-- data Bool = False | True
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- someFunc :: Circle -> Float - doesnt working





-- data Person = Person String String Int Float String String deriving (Show)
-- firstName :: Person –> String
-- firstName (Person firstname _ _ _ _ _) = firstname
-- lastName :: Person –> String
-- lastName (Person _ lastname _ _ _ _) = lastname
-- age :: Person –> Int
-- age (Person _ _ age _ _ _) = age
-- height :: Person –> Float
-- height (Person _ _ _ height _ _) = height
-- phoneNumber :: Person –> String
-- phoneNumber (Person _ _ _ _ number _) = number
-- flavor :: Person –> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

-- works like

data Person = Person { 
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String 
} deriving (Show)

data Car = Car {
    mark :: String,
    number :: String
} deriving (Show, Eq)

data Vector a = Vector a a a deriving(Show)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector a b c) = Vector(i + a) (j + b) (k + c)


data Anime = Syonen {
    mainCh :: String,
    mainEnemy :: String
} | Etti {
    mainCh :: String,
    girls :: [String]
} deriving (Show, Eq)


-- demonsSlayer :: Anime
-- demonsSlayer = Syonen {mainCh = "Tanjiro", mainEnemy = "Mudzan"}


-- to change lastname of second person
-- using part of type example
data Person' = Person' { firstName' :: String, lastName' :: String, age' :: Int }
updateLastName :: Person' -> Person' -> Person'
updateLastName (Person' {lastName' = ln}) p2 = p2 {lastName' = ln}

-- or positional 
updateLastName' :: Person' -> Person' -> Person'
updateLastName' (Person' fn ln age) p2 = p2 {lastName' = ln}



--strict Types
data CoordLazy a = CoordL a a deriving Show
data CoordStrict a = CoordS !a !a deriving Show

class StrictExample a where 
    strictEx :: a -> a
instance StrictExample (CoordLazy a) where
    strictEx (CoordL x y) = CoordL x x
    -- strictEx (CoordL 1 undefined) == CoordL 1 1, does not calculate undefined
instance StrictExample (CoordStrict a) where
    strictEx (CoordS x y) = CoordS x x
    -- strictEx (CoordL 1 undefined) == error, calculate's undefined and returns error, because of !



-- infix constructors
data ConsExample a = Constructor a a
-- or 
--data ConsExample a = a `Constructor` a
-- or usyng operator, that should starting with (:)
--data ConsExample a = a :+ a

-- for example --> data [] a = Nil | a : ([] a) is ectually List





-- to make instance for (a, a)
newtype Shell a = Shell (a, a)
instance Num a => Num (Shell a) where
    (Shell (x1, y1)) + (Shell (x2, y2)) = Shell (x1 + x2, y1 + y2)
-- because (a, a) is hard to descript by pattern matching
-- or
instance (Num a, Num b) => Num (a, b) where
    (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
