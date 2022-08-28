{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Introduction to functional programming in Haskell
--
-- Exercise sheet 1
--
-- Code snippets
--

--
-- Exercise 1
--

e1 = [True]
e2 = []
e3 = \ x -> x
e4 = \ x y -> (y, x)
e5 = \ x y -> [y, x]
e6 = \ x -> (if x == [] then x else x)
e7 = head
e8 = \ xs -> tail xs
e9 = \ xs -> tail (tail xs)
f x y z = if x then y else z
g x y z = if x == y then y else z

data Tree a = Leaf | Node [a] (Tree a) (Tree a)
h Leaf = "c"
h (Node "x" l r) = "x"
h (Node x l r) = x
i (Node x l r) = id
j (Node [] l r) = l
k Nothing = Nothing
l (Node a b c) = a
m (Node x l r) = Node [length x] (m l) (m r)


--
-- Exercise 2
--

permutations :: [Integer] -> [[Integer]]
permutations = undefined

idioticSort :: [Integer] -> [Integer]
idioticSort = undefined


--
-- Exercise 3
--

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap = undefined

partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' = undefined

length' :: [a] -> Int
length' = undefined

minimumWrt :: (a -> Integer) -> [a] -> a
minimumWrt = undefined

sortWrt :: (a -> a -> Bool) -> [a] -> [a]
sortWrt = undefined


--
-- Exercise 4
--

-- assign :: Formula -> Literal -> Formula

-- unitLiteral :: Formula -> Maybe Literal

-- pureLiteral :: Formula -> Maybe Literal

-- sat :: Formula -> Bool

-- sat' :: Formula -> Maybe [Literal]
