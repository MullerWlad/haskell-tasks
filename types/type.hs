{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

setOneHead :: (Num a) => [a] -> [a]
setOneHead (x:xs) = xs