--
-- Introduction to functional programming in Haskell
--
-- 7: Lazy evalation
--

-- language pragma that allows us to use bang patterns (see below) to enforce strictness
{-# LANGUAGE BangPatterns #-}


-- lazy evaluation of lists
mylist = [1,2,3,undefined]

-- try these two - what is happening
string = show mylist

-- works!
string2 = show $ take 3 mylist


-- non-strict semantics
f a b = b

valf = f undefined 5

-- enforce strict semantics with a bang pattern
g !a b = b

valg = g undefined 5


-- lazy evaluation allows us to work with infinite objects

-- infinite lists

-- notation
inflist1 = [1..]

-- can be implemented as follows
from :: Integer -> [Integer]
from n = n : (from (n+1)) -- recursive function without a base case

inflist2 = from 1

-- another example
repeatAdInfinitum x = x : (repeatAdInfinitum x)

-- works if we only access a finite prefix
finlist1 = take 10 [1..]

-- does not terminate...
finlist2 = filter (\ x -> x < 10) [1..]


-- we can do infinite objects for our own data types
data Tree = Node Integer Tree Tree | Leaf
    deriving Show

infiniteTreeFrom n = Node n (infiniteTreeFrom (n+1)) (infiniteTreeFrom (n+1))

inftree = infiniteTreeFrom 1

-- function for cutting of beyond a certain level
truncateAt :: Integer -> Tree -> Tree
truncateAt 0 t = Leaf
truncateAt _ (Leaf) = Leaf
truncateAt n (Node i left right) = Node i (truncateAt (n-1) left) (truncateAt (n-1) right)

fintree = truncateAt 3 inftree


-- we can define recursive data types with no base cases
-- this enforces that all objects of this type are necessarily infinite

data InfiniteList a = Head a (InfiniteList a)
    deriving Show

infiniteListFrom n = Head n (infiniteListFrom (n+1))

inflist = infiniteListFrom 1


-- memoization

-- naive definition of the Fibonacci function
fibSlow :: Int -> Integer
fibSlow 0 = 0
fibSlow 1 = 1
fibSlow n = fibSlow (n-2) + fibSlow (n-1)

-- try fib 40
-- does not work in reasonable time because haskell does not store function values
-- e.g. in order to compute fib 40, we compute fib 38 twice, fib 37 four times, ...

-- haskell can store values
-- by defining a list of all Fibonacci numbers (which is populated with the needed values using lazy evaluation)
-- we can circumvent the problem as follows

fibonacci = map fibHelper [0..] -- list of all Fibonacci numbers that is stored
fibFast n = fibonacci !! n
fibHelper 0 = 0
fibHelper 1 = 1
fibHelper n = fibFast (n-2) + fibFast (n-1) -- note that we call fibFast, not fibHelper here

-- try fib_fast 40

-- Notes
-- 1. memoization can be fickle
-- we need to make sure that the value is stored and retained

-- the following version also works
fibFast' :: Int -> Integer
fibFast' = (map fibHelper' [0..] !!)
-- this is point-free notation for
-- fib'' n = map fib_helper [0..] !! n
-- (but if we actually wrote it that we, it wouldn't be fast, see blelow)
    where
        fibHelper' 0 = 0
        fibHelper' 1 = 1
        fibHelper' n = fibFast' (n-2) + fibFast' (n-1) -- note that we call fib'', not fib_helper here

-- this version is slow -- the list is not retained
fibFail n = fibonacci' !! n
    where
        fibonacci' = map fibHelper' [0..]
        fibHelper' 0 = 0
        fibHelper' 1 = 1
        fibHelper' n = fibFail (n-2) + fibFail (n-1)

-- 2. memoization can be generalized into a pattern that works for all kinds of functions
-- memoization can be implementing using a fixed-point operator
-- see e.g. https://wiki.haskell.org/Memoization

-- 3. our memoized versions of fib (fib'' and fib''') still uses space linear in n
-- (if we assume that Integers need constant space)
-- because in order to compute fib n, we store fib 1, fib 2, ..., fib (n-1)
-- in imperative programming languages, fib can be implemented using constant space (by just storing the two last values of fib)
-- a constant-space variant can be defined in Haskell using the ST monad (later)
