-- takes words of message
wordsOf' :: String -> [String]
wordsOf' [] = []
wordsOf' message = word : wordsOf' other
    where word = fst division -- is equal takeWhile help message
          other = specialTail $ snd division -- is equal specialTail $ dropWhile help message
          specialTail [] = []
          specialTail xs = tail xs
          division = span help message
          help = and . (\x -> (/=) <$> [' '] <*> [x]) -- add more exceptions
wordsOf :: String -> [String]
wordsOf = filter (/= "") . wordsOf'
-- using monad -> [message] >>= wordsOf

-- returns message of words
messageOf :: [String] -> String
messageOf [] = ""
messageOf words = foldl1 (\w1 w2 -> w1 ++ " " ++ w2) words

uppList :: [(Char, Char)]
uppList = zip ['a'..'z'] ['A'..'Z']

toUpp :: String -> String -> String
toUpp message template = messageOf $ solve msgList templateList
    where msgList = wordsOf message
          templateList = wordsOf template
          solve m t = map (help t) m
          help _ [] = []
          help temp word@(l : ls)
            | word `elem` temp = word
            | l `notElem` ['a'..'z'] = word
            | otherwise = (snd $ head $ [x | x <- uppList, fst x == l]) : ls

--reading :: IO ()
--reading = readFile "test.txt" >>= print . (`toUpp` "a in of the")

count :: IO ()
count = readFile "test.txt" >>= print . length . wordsOf
