import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Char (isDigit)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)
    
instance MapLike ListMap where
    empty = ListMap []
    lookup k' (ListMap list)
        | null newlist = Nothing 
        | otherwise = Just $ snd $ head newlist
        where newlist = [pare | pare <- list, fst pare == k']
    insert k v (ListMap list) = ListMap (help list)
        where help [] = [(k, v)]
              help (x : xs)
                | fst x == k = (k, v) : xs
                | otherwise = x : help xs
    delete k (ListMap list) = ListMap (help list)
        where help [] = []
              help (x : xs)
                | fst x == k = xs
                | otherwise = x : help xs

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)





data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf (Just value)) = Leaf $ Just $ f value
    fmap f (Leaf Nothing) = Leaf Nothing
    fmap f (Branch l (Just value) r) = Branch (fmap f l) (Just $ f value) (fmap f r)
    fmap f (Branch l Nothing r) = Branch (fmap f l) Nothing (fmap f r)




data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
     deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken x = case (all isDigit x) of
    True -> Just $ Number (read x :: Int)
    False -> Nothing

tokenize :: String -> Maybe [Token]
tokenize input = return input >>= -- Maybe String
                 return . words >>= -- Maybe [String]
                 return . (\words -> words >>= \word -> [asToken word]) >>= -- Maybe [Maybe Token]
                 noNothing -- Maybe [Token]
    where noNothing tokens = case Nothing `elem` tokens of
                                True -> Nothing
                                False -> Just (tokens >>= (\(Just token) -> [token]))



-- using monad 
-- pythagoreanTriple x = [(a, b, c) | a <- [1..], b <- [1..], c <- [1..], c <= x, a < b]
-- memory limit
