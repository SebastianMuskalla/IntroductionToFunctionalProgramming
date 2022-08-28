module Main where

import Data.Char

main = echo

echo =
    do
        s <- getLine
        putStrLn $ map toUpper s
