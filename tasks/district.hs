type District = Int
type Path = (District, District)

-- have a districts (a)
-- (a, b) <- P => (a, b) is path from a to b
-- P != reflecsive
-- P = P ^ (-1)
-- P * P != P
-- (a, b), (b, c) <- P, (a, c) doesn't <- P => b - custom

transit :: Path -> Path -> Maybe Path
transit (b', a') (a, b)
    | a == a' = Nothing 
    | b == b' = Just (a, a')
    | otherwise = Nothing 

joiner :: Path -> Path -> Maybe District
joiner (a, b) (a', b')
    | (a == b) || (a' == b') = Nothing
    | ((a == a') && (b /= b')) || ((a == b') && (a' /= b)) = Just a
    | ((b == b') && (a /= a')) || ((a' == b) && (a /= b')) = Just b
    | otherwise = Nothing

-- a = Just (1, 2) >>= (\n -> (transit (2, 3) n >>= (\k -> transit (3, 5) k)))
-- is equal
-- a = Just (1, 2) >>= (\n -> transit (2, 3) n >>= (\k -> transit (3, 5) k))
