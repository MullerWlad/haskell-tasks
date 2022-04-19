appender :: FilePath -> IO ()
appender path = read path >>= append >> appender path
    where read path = readFile path >>= 
                putStrLn >> 
                return path -- returning for append
          append path = msg2 >> 
                appendFile path "\n" >>
                getLine >>= 
                appendFile path
          msg2 = putStrLn "Writing new line..."

main :: IO ()
main = msg1 >> getLine >>= appender
    where msg1 = putStrLn "Reading a path..."
          

{-type FilePath = String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()-}