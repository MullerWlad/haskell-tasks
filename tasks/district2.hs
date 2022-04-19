data Point a = Client a | Domain a

instance Functor Point where
    fmap f (Client a) = Domain $ f a
    fmap f (Domain a) = Client $ f a

-- Client f <*> Client a is equal Client of a number sends f to Domain $ f a
instance Applicative Point where
    pure = Domain
    Client f <*> Client a = fmap f (Client a)
    Domain f <*> Domain a = fmap f (Domain a)
    Domain f <*> Client a = fmap f (Domain a)
    Client f <*> Domain a = fmap f (Client a)

instance Monad Point where
    return = Domain
    Domain a >>= f = f a
    Client a >>= f = f a
    Client a >> Client b = Client b
    Client a >> Domain b = Domain b
    Domain a >> Domain b = Domain b
    Domain a >> Client b = Client b
