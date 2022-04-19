injection :: [[a]] -> [a]
injection [] = []
injection [[]] = injection []
injection (l : ls) = l ++ help ls
    where help ls = [last l | l <- ls]

inside :: [[a]] -> [[a]]
inside [] = []
inside (m : ms) = [help x | x <- ms]
    where help [] = []
          help [x] = help []
          help (x : xs) = x : help xs

strongReverse :: [[a]] -> [[a]]
strongReverse matrix = reverse [reverse m | m <- matrix]

snail :: [[a]] -> [a]
snail [] = []
snail matrix = (injection matrix) ++ (snail $ strongReverse $ inside matrix)

-- works with matrix m x n | m, n <- N
