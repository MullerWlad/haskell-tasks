-- import Data.Char (toUpper)
import Control.Monad ( when )
main :: IO ()
{-main = putStrLn "Hi" >> 
    getLine >>= 
    (\name -> putStrLn ("Hi," ++ name) >> 
    getLine >>= 
    (\surname -> putStrLn $ "Of course, " ++ name ++ " " ++ surname))-}
-- or
{-main = do
    putStrLn "Hi"
    name <- getLine
    putStrLn $ "Hi, " ++ name
    surname <- getLine
    putStrLn $ "Of course, " ++ name ++ " " ++ surname-}

-- look at Hello_world


-- let's use let in do notation
{-main = do
    word <- getLine 
    let bigWord = toUpper <$> word
    putStrLn bigWord-}


-- recursive
{-main = do
    str <- getLine
    putStrLn $ "String: " ++ str
    main-}
-- or
{-main = forever $ do
    str <- getLine
    putStrLn $ "String: " ++ str-}


-- solver
{-main = do
    str <- getLine
    let result = let solve str
                         | length str > 5 = "more five"
                         | otherwise = "less five"
                 in solve str
    putStrLn result
    main-}


-- print == putStrLn . show


-- when
{-main = do
    input <- getLine
    when ( input == "hello" ) $ do
        putStrLn input-}


-- sequcence
{-main = do
    a <- getLine 
    b <- getLine
    c <- getLine
    print [a, b ,c]-}
-- or
main = do
    xs <- sequence [getLine, getLine, getLine]
    print xs
