
-- functor => applicative => monad

-- in functor f
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- in applicative f
-- pure :: Applicative f => x -> f x
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
-- <$> :: Applicative f => (a -> b) -> f a -> f b

-- in monad m
-- >>= :: Monad m => m a -> (a -> m b) -> m b
-- >>= called "binding"

-- >>= is equal >>>=
class Applicative m => Monad' m where -- because functor => applicative => monad
    return' :: a -> m a
    (>>>=) :: m a -> (a -> m b) -> m b
    (>>>) :: m a -> m b -> m b
    x >>> y = x >>>= (\_ -> y) -- or const y
    fail :: String -> m a
    fail = error

instance Monad' Maybe where
    return' = Just
    Nothing  >>>= f = Nothing 
    Just a >>>= f = f a
    fail _ = Nothing

-- let's play with Maybe
-- Just 9 >>>= (\x -> return' $ x * 10) == Just 90

-- Bird's
type Bird = Int
type Pole = (Bird, Bird)

landR :: Bird -> Pole -> Maybe Pole
landR bird (l, r)
    | r + bird < 0 = Just (l, 0)
    | abs (l - r - bird) > 5 = Nothing 
    | otherwise = Just (l, r + bird)

landL :: Bird -> Pole -> Maybe Pole
landL bird (l, r)
    | l + bird < 0 = Just (0, r)
    | abs (l + bird - r) > 5 = Nothing 
    | otherwise = Just (l + bird, r)

-- landR 4 $ landL 3 $ landL 5 $ landR (-4) (0, 0) == (8, 0)
    -- or landR 4 (0, 0) >>= landL 3 >>= landL 5 >>= landR (-4) == Just (8, 0)
    -- or Just (0, 0) >>= landR 4 >>= landL 3 >>= landL 5 >>= landR (-4) == Just (8, 0)
    -- or return (0, 0) >>= landR 4 >>= landL 3 >>= landL 5 >>= landR (-4) == Just (8, 0)
-- monads let us adding a lot functions in current context

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- return (0, 0) >>= landR 4 >>= banana >>= landL 3 >>= landL 5 >>= landR (-4) == Nothing
    -- or return (0, 0) >>= landR 4 >> Nothing >>= landL 3 >>= landL 5 >>= landR (-4) == Nothing


-- do notation
-- Just 3 >>= (\x -> Just((show x) ++ "!")) == Just "3!"
    -- or Just 3 >>= (\x -> Just "!" >>= (\y -> Just(show x ++ y))) == Just "3!"
    -- let's write it instring
    -- foo = Just 3 >>= (\x ->
          -- Just "!" >>= (\y ->
          -- Just (show x ++ y)
    -- let's delete lambda's and use do notation
    -- foo = do
        -- x <- Just 3
        -- y <- Just "!"
        -- Just (show x ++ y)
conc :: Show a => a -> Maybe String
conc x = do
    a <- Just x -- or let a = Just x
    b <- Just "!"
    Just (show a ++ b)

-- Just 9 >>= (\x -> Just (x > 8)) == True
-- or simple = do
    --  x <- Just 9
    --  Just (x > 8)

-- return (0, 0) >>= landR 4 >>= landL 3 >>= landL 5 >>= landR 3 == Just (8, 7)
-- or return (0, 0) >>= (\x -> landR 4 x >>= (\y -> landL 3 y >>= (\z -> landL 5 z >>= (\t -> landR 3 t)))) == Just (8, 7)
result :: Maybe Pole -- Just (8, 7)
result = do
    start <- return (0, 0) -- or let start = return (0, 0)
    x <- landR 4 start
    y <- landL 3 x
    z <- landL 5 y
    landR 3 z

-- if we use <- ==> we use >>=, or if we doesn't use <- ==> we use >>
result' :: Maybe Pole -- Nothing
result' = do
    start <- return (0, 0) -- or let start = return (0, 0)
    x <- landR 4 start
    y <- landL 3 x
    Nothing 
    z <- landL 5 y
    landR 3 z

-- in the end
-- Just 5 >>= (\x -> Just (x + 3) >>= (\y -> Just (y * 2)))
summing :: Maybe Integer
summing = do
    start <- Just 5 -- or let start = Just 5
    x <- Just (start + 3)
    Just (x * 2)


justFirst :: Maybe Char -- Just 'h'
justFirst = do
    (x : xs) <- Just "hello"
    return x


-- lists !
instance Monad' [] where
    return' x = [x]
    xs >>>= f = concat $ return' f <*> xs
    -- or [] >>>= _ = []
        -- (x : xs) >>>= f = f x ++ (xs >>>= f)
    fail _ = []

-- [1, 2, 3] >>= (\x -> [[x, -x]]) >>= (\y -> return $ product y)
    -- or [1, 2, 3] >>= (\x -> [[x, -x]] >>= (\y -> return $ product y))
    -- product is equal foldr1 (*)
dolist :: [Integer]
dolist = do
    start <- [1, 2, 3] -- or let start = [1, 2, 3]
    x <- (\x -> [[x, -x]]) start
    return $ product x


-- monad's in generator's of list's (<-)
-- [x | x <- [1, 2, 3]] == [1, 2, 3]


-- monad's rules
-- return x >>= f == f x, because return x :: Monad m => m a, f :: Monad m => a -> m a
-- m >>= return == m
-- (m >>= f) >>= g == m >>= (\x -> f x >>= g)
    -- but >>= is left-associative ==> (m >>= f) >>= g == m >>= f >>= g
    -- but lambda is infinite right ==> m >>= \x -> f x >>= g
-- m >>= f >>= g == m >>= \x -> f x >>= g

-- do rules
-- do {e1 ; e2} == e1 >> e2
-- do {p <- e1 ; e2} == e1 >>= \p -> e2
-- do {let v = e1 ; e2} == let v = e1 in do e2

-- goWrap =
--  let i = 3 in
--  wrap'n'succ i >>= \x ->
--  wrap'n'succ x >>= \y ->
--  wrap'n'succ y >>
--  return (i, x + y)

-- or (imperative style)
-- goWrap = do
--  let i = 3
--  x <- wrap'n'succ i
--  y <- wrap'n'succ x
--  wrap'n'succ y
--  return (i, x + y)
