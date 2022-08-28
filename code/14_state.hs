--
-- Introduction to functional programming in Haskell
--
-- 12: The monads State and ST
--


-- language pragma that allows us to specify the signature for functions
-- in the definitions of instances of type classes
-- for the sake of clarity
{-# LANGUAGE  InstanceSigs #-}


import Prelude hiding (gcd)
import Control.Monad.State.Lazy
import Control.Monad.ST
import Data.STRef
import Data.Array.ST


-- State monad:
-- State s r = a computation that returns an object of type r and maintains an internal state of type s
-- to be precise (State s) is a monad for every type s

newtype State' s r = State' (s -> (s,r))

instance Functor (State' s) where
    -- apply the function to the result of the computation
    fmap :: (r1 -> r2) -> State' s r1 -> State' s r2
    fmap f (State' transformation) = State'
        (
            \ state ->
            let
                (newstate, result) = transformation state
            in
                (newstate, f result)
        )

instance Applicative (State' s) where
    -- do not touch the internal state
    pure result = State' (\ x -> (x, result))
    -- execute the command yielding the function,
    -- then execute the command yielding a value
    -- then apply the function to the value and return the result
    (<*>) :: State' s (r1 -> r2) -> State' s r1 -> State' s r2
    (State' transformationf) <*> (State' transformationx)
        = State'
        (
            \ state ->
            let
                (newstate, f)   = transformationf state
                (newerstate, x) = transformationx newstate
            in
                (newerstate, f x)
        )

instance Monad (State' s) where
    (>>=) :: (State' s r) -> (r -> State' s r2) -> (State' s r2)
    (State' transformation) >>= function = State'
        (
            \ state ->
            let
                (newstate, result) = transformation state
                (State' transformation2) = function result
            in
                transformation2 newstate
        )


-- we also need the following helper functions

-- get the current internal state and exhibit it as the result
-- (while not changing the internal state)
get' :: State' s s
get' = (State' (\ state -> (state,state)))


-- set the internal state to the specified value
-- (and output () as a dummy result)
put' :: s -> State' s ()
put' x = State' ( \ _ -> (x, ()))

-- function that takes a transformation and an initial state, applies the transformation and returns the result
-- (note that we discard the final state)
runState' :: State' s r -> s -> r
runState' (State' transformation) initialstate = snd $ transformation initialstate

-- Example: Euclidean algorithm

-- Imperative pseudo-code (from https://en.wikipedia.org/wiki/Euclidean_algorithm#Implementations)
-- function gcd(a, b)
--     while b ≠ 0
--        t := b;
--        b := a mod b;
--        a := t;
--     return a;


gcd a b =
    let
        ( () ,(a',b')) = runState gcdLoop (a,b)
    in
        a'

-- we implement the loop
-- we implement it using the monad State (Int, Int)
-- i.e. our internal state is a tuple of integers (a,b)
gcdLoop :: State (Int,Int) ()
gcdLoop =
    do
        (a,b) <- get           -- take the internal state
        if b /= 0              -- while condition
        then do
            put (b, a `mod` b) -- this replaces 'a' by 'b' and 'b' by the old value of 'a mod b'
            gcdLoop           -- recursion in order to repeat the loop
        else
            return ()          -- the loop is finished, the values we want are stored in the state

-- note that gcdLoop looks like it is imperative (or even impure) code,
-- but it is still purely functional code, just written in an imperative way

-- the State monad resp. its bind (>>=) operator allows us to makes the handling of the internal state implicit


-- the ST (state-thread monad)
-- the ST monad can be seen as a variant of the State monad, where the state is the state of a "thread"

-- However, there are a few fundamental differences
--   unlike the State monad, the ST monad cannot simply be reimplemented by the user
--   conceptually, the ST monad is defined using so-called higher-kinded types
--   we cannot expose the internal state, i.e. there is no equivalent of the "get" function that returns the state
--   the actual implementation of the ST monad is impure

-- Also note that the IO monad is just a version of the ST monad


-- Example: Constant-space fibonacci using the ST Monad
-- (taken from R. Bird's book)

-- recall that using memoization, we were able to define a variant of the Fibonacci function that uses linear time and space
-- (in contrast to exponential time for a naive implementation)

-- using the ST monad, we can define a variant that uses linear time and constant space

fibST :: Int -> ST s Integer
fibST n =
    do
        a <- newSTRef 0 -- reference to some fresh address a with value 0
        b <- newSTRef 1 -- reference to some fresh address a with value 1
        repeatFor n -- do n times, see below
            (
                do
                    x <- readSTRef a -- read current value of address a
                    y <- readSTRef b -- read current value of address b
                    writeSTRef a y -- set a to y (old value of b)
                    writeSTRef b $! (x+y) -- set b to (x + y) (old value of a + b)
            )
        readSTRef a

-- note taht we use the strict function $! to enforce the evaluation of (x+y) because lazy evaluation might gives us the wrong values

-- helper functions
done :: Monad m => m ()
done = return ()

repeatFor :: Monad m => Int -> m a -> m ()
repeatFor n = ( foldr (>>) done ) . (replicate n)

-- execute the code
fib n = runST $ fibST n

-- understanding the details of what happens here is beyond the scope of this lecture
-- if you really want to understand what happens here, you might want to start with understanding what the "repeatFor" function does


-- Example 3: In-place quicksort using mutable arrays in the ST monad
-- (also taken from R. Bird's book)

-- pure variant that uses list comprehension (which implicitly uses the list monad)
quickSort [] = []
quickSort (x:xs) =
    quickSort [ y | y <- xs, y < x ]
        ++ [x]
        ++ quickSort [ y | y <- xs, y >= x ]

quickSort' xs =
    runST $
    do
        xa <- newListArray (0,n-1) xs
        quickSortST xa (0,n)
        getElems xa
    where
        n = length xs

-- newListArray :: Ix i => (i,i) -> [e] -> ST s (STArray s i e)
-- takes a tuple of indices and a list and creates an array containing that list

-- sort the given interval (a,b) in the given array xa, i.e.
-- sort xa[a, ..., b-1]
quickSortST :: Ord a => STArray s Int a -> (Int,Int) -> ST s ()
quickSortST xa (a,b)
    |   a == b    = return () -- the interval we should sort is empty
    |   otherwise =
        do
            m <- partition xa (a,b) -- rearrange the array (see below)
            quickSortST xa (a,m)    -- recursive call for the first half
            quickSortST xa (m+1,b)  -- recursive call for the second half

-- rearrange the interval (a,b) of array xa, i.e. xa[a, ..., b-1]
-- such that
--     new xa[a ... m-1]     == values of xa[a,...,b-1] smaller than xa[a]
--     new xa[m]             == xa[a]
--     new xa[m+1, ..., b-1] == values of xa[a, ..., b+1] greater than xa[a]
-- and return the index m
partition :: Ord a => STArray s Int a -> (Int,Int) -> ST s Int
partition xa (a,b) =
    do
    {
        pivot <- readArray xa a;
        let
            loop (j,k) =
                if j == k
                then do
                    swap xa a (k-1)
                    return (k-1)
                else do
                    y <- readArray xa j
                    if y < pivot then loop (j+1,k)
                    else do
                        swap xa j (k-1)
                        loop (j,k-1)
        in
            loop (a+1,b)
    }


-- swap the values at the given indices
swap :: STArray s Int a -> Int -> Int -> ST s ()
swap xa i j =
    do
        x <- readArray xa i
        y <- readArray xa j
        writeArray xa i y
        writeArray xa j x
