--
-- Introduction to functional programming in Haskell
--
-- Examples for 11: do notation
--

import Data.Char

f =
    do
        putStrLn "evaluating f"
        return 1

g =
    do
        putStrLn "evaluating g"
        return 'a'

h =
    do
        putStrLn "evaluating h"
        return 2.0

test1 =
    do
        x <- f
        y <- g
        z <- h
        return (x,y,z)


test2 =
    f >>= \ x ->
        do
            y <- g
            z <- h
            return (x,y,z)


test3 =
    f >>= \ x -> g >>= \ y ->
        do
            z <- h
            return (x,y,z)

test4 =
    f >>= \ x -> g >>= \ y -> h >>= \ z ->
        do
            return (x,y,z)

test5 =
    f >>= \ x -> g >>= \ y -> h >>= \ z -> return (x,y,z)


f1 = do
    input <- getLine
    putStrLn $ map toUpper input

f2 =
    do
        input <- getLine
        putStrLn $ concatMap (\ x -> x : " ") input
