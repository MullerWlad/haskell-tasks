main :: IO ()
{-main = do
    line <- getLine
    putStrLn line
    main-}
-- or better
main = do
    content <- getContents
    putStrLn content
-- use ..io2 < data.txt
-- on windows => cmd /c '.\io2 < data.txt'
