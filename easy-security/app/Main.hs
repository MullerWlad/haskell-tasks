module Main where

import CodeMe.Playfair ( playfair, usePlayfair )
import CodeMe.Polibiy ( polibiy )
import Auth

codePlayfair :: IO ()
codePlayfair = getLine >>= (putStrLn . usePlayfair)

codePolibiy :: IO ()
codePolibiy = getLine >>= (putStrLn . polibiy)

coder :: String -> IO ()
coder line
    | line == "Playfair" = codePlayfair
    | line == "Polibiy" = codePolibiy
    | otherwise = putStrLn "Nothing to do"

variant :: String -> IO ()
variant line
    | line == "9" = auth "9"
    | line == "15" = auth "15"
    | otherwise = putStrLn "Nothing to do"

taskChooser :: String -> IO ()
taskChooser line
    | line == "3" = putStrLn "You choose 3: Playfair | Polibiy" >> 
                    getLine >>=
                    coder
    | line == "4" = putStrLn "You choose 4: 9 | 15" >>
                    getLine >>=
                    variant
    | otherwise = putStrLn "Nothing to do"

main :: IO ()
main = putStrLn "Hi, choose 3 or 4" >>
       getLine >>= 
       taskChooser 
