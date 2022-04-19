module Auth (
    auth
    ) where
import Data.Char (toLower)


reader :: FilePath -> IO [PassLine]
reader path = readFile path >>= 
              return . lines >>= 
              return . parser

data PassLine = PassLine {
    variant :: String,
    login :: String,
    password :: String
} deriving (Show, Eq)

parser :: [String] -> [PassLine]
parser = map ((\[a, b, c] -> PassLine {
    variant = a,
    login = b,
    password = c
    }) . words)

findVar :: String -> [PassLine] -> [PassLine]
findVar var list = [pl | pl <- list, variant pl == var]

findLast :: [PassLine] -> PassLine
findLast = last

appender :: FilePath -> String -> String -> String -> IO ()
appender path variant login password = appendFile path "\n" >> 
                                       appendFile path (variant ++ " " ++ login ++ " " ++ password)

actualPass :: String -> IO PassLine
actualPass variant = reader "app/pass.txt" >>= 
                     return . findVar variant >>=
                     return . findLast

doSomething :: Bool -> String -> IO ()
doSomething False variant = putStrLn "Failed" >> auth variant
doSomething True variant = putStrLn "Done" >> wellPassed variant

wellPassed :: String -> IO ()
wellPassed variant = putStrLn "Change or Relog" >>
                     getLine >>= 
                    (\x -> case x of
                        "change" -> changer variant
                        "relog" -> auth variant
                        x -> putStrLn "Wrong choise!") >>
                    wellPassed variant

(<%>) :: Eq a => [a] -> [a] -> [a]
[] <%> _ = []
(x : xs) <%> list
    | not $ null [a | a <- list, a == x] = x : (xs <%> list)
    | otherwise  = x : (xs <%> list)

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : (uniq (filter (\z -> z /= x) xs))

changer :: String -> IO ()
changer "15" = getLine >>=
               (\x -> case ( (last $ words x) == uniq (x <%> (['A'..'Z'] ++ ['a'..'z'])) && (length $ last $ words x) == 6 ) of
                   True -> appender "app/pass.txt" "15" (head $ words x) (last $ words x) >> wellPassed "15"
                   False -> putStrLn "Wrong pass!" >> changer "15"
                   )
changer "9" = getLine >>=
               (\x -> case ( (last $ words x) == (x <%> "0123456789") && (length $ last $ words x) == 7 ) of
                   True -> appender "app/pass.txt" "9" ('1' : (tail $ head $ words x)) (last $ words x) >> wellPassed "9"
                   False -> putStrLn "Wrong pass!" >> changer "9"
                   )
changer x = putStrLn "No variant" >> auth x


auth :: String -> IO ()
auth variant = do 
    putStrLn "Login Password"
    lp <- getLine
    alp <- actualPass variant
    doSomething (parser [variant ++  " " ++ lp] == [alp]) variant
