--
-- Introduction to functional programming in Haskell
--
-- 4: Higher-order functions
--

-- even basic functions that seemingly take two parameters are higher order functions
plus :: Integer -> (Integer -> Integer)
plus a b = a + b
-- this can be seen a function that takes ONE parameter and returns a function

-- function composition, called (.) in the standard library
dot' :: (b -> c) -> (a -> b) -> (a -> c)
dot' f g = \ x -> f (g (x))
-- point-free notation, equivalent to:
-- dot' f g x = f (g x)

-- example
f = (+1) . (\ x -> x*x)


-- MAP family
-- take a list, return a list

-- map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (h:t) = (f h) : (map' f t)

-- example
l1 = map' f [1..10]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate (h:t) =
    if predicate h then h : rest else rest
        where
            rest = filter' predicate t

-- example
l2 = filter' even [1..50]

zip' :: (a -> b -> c) -> [a] -> [b] -> [c]
zip' _ []     _      = []
zip' _ _      []     = []
zip' z (x:xs) (y:ys) = (z x y) : zip' z xs ys

l3 = [0..10]
l4 = reverse l3
l5 = zip' (\ x y -> (x,y)) l3 l4


-- REDUCE family
-- take a list, return a value

-- foldin' from the right...
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' operator init []     = init
foldr' operator init (x:xs) = x `operator` foldr operator init xs

sumr = foldr' (+) 0

-- foldin' from the left
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' operator init []     = init
foldl' operator init (x:xs) = foldl' operator (init `operator` x) xs

-- what are the type signatures for these values?
-- what do they do?
suml = foldl' (+) 0
allTrue = foldl' (&&) True


-- other examples

-- trivial higer order function, called ($) in the standard library
trivial :: (a -> b) -> (a -> b)
trivial f = f
-- equivalent to
-- trivial f x = f x

val = (+1) `trivial` (+1) 5
-- avoids brackets: (+1) (+1) 5 is not valid
val' = (+1) $ (+1) 5 -- nice!

-- flippin' the arguments
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- useful for currying
mapOverSmallNumbers = flip map [1..100]
someSquareNumbers = mapOverSmallNumbers (^2)
