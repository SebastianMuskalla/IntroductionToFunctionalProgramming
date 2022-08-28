--
-- Introduction to functional programming in Haskell
--
-- 12: Monads
--

-- language pragma that allows us to specify the signature for functions
-- in the definitions of instances of type classes
-- for the sake of clarity
{-# LANGUAGE  InstanceSigs #-}


import Data.Char

-- we first need to understand Functors


-- class Functor m  where
--     fmap :: (a -> b) -> m a -> m b

-- Functor laws:
-- fmap should satisfy the equalities
--   fmap id       ==  id
--   fmap (f . g)  ==  fmap f . fmap g

-- trivial functor: Wrap something in a burrito
newtype Burrito a = Burrito a
    deriving (Eq,Show,Ord)

-- fmap: apply the function to the inside of the burrito
instance Functor Burrito where
    fmap f (Burrito x) = Burrito (f x)


-- custom version of Maybe
data Maybe' a = Nothing' | Just' a
    deriving (Eq,Ord,Show)

-- fmap: apply the function to the value in the Just-case
instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap _ Nothing' = Nothing'
    fmap f (Just' x) = Just' (f x)


-- custom version of [a]
data List a = Empty | Head a (List a) deriving (Eq,Ord,Show)

-- fmap ... is essentially map
instance Functor List where
    fmap :: (a -> b) -> (List a) -> (List b)
    fmap _ Empty         = Empty
    fmap f (Head x tail) = Head (f x) (fmap f tail)


-- next, we should understand Applicative

-- class Applicative m where
--     pure :: a -> m a
--     (<*>) :: m (a -> b) -> m a -> m b

-- Applicative laws:
-- pure and <*> should satisfy the equalities
--   pure id <*> v              == v
--   pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
--   pure f <*> pure x          == pure (f x)
--   u <*> pure y               == pure ($ y) <*> u


-- trivial Applicative
instance Applicative Burrito where
    -- wrap something in a Burrito
    pure x = Burrito x
    -- equivalently:
    -- pure = Burrito
    -- apply a wrapped function to a wrapped value
    -- by unwrapping both,
    -- applying the function,
    -- then rewrapping the result
    (Burrito f) <*> (Burrito x) = Burrito (f x)


-- Maybe is an Applicative
instance Applicative Maybe' where
    pure :: a -> Maybe' a
    pure = Just'

    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    Nothing'  <*> _         = Nothing'
    _         <*> Nothing'  = Nothing'
    (Just' f) <*> (Just' x) = Just' (f x)

-- implementation of (++) for our custom list type
conc :: List a -> List a -> List a
conc Empty          list2 = list2
conc (Head x list1) list2 = Head x (conc list1 list2)

instance Applicative List where
    pure :: a -> List a
    -- pure x = [x]
    pure x = Head x Empty
    (<*>) :: List (a -> b) -> List a -> List b
    -- []     <*> _       = []
    -- (f:fs) <*> vallist = (map f vallist) ++ (fs <*> vallist)
    Empty            <*> _       = Empty
    (Head f funlist) <*> vallist = (fmap f vallist) `conc` (funlist <*> vallist)


-- now we can try to understand Monad

--  class Monad m where
--    (>>=)       :: m a -> (a -> m b) -> m b
--    return      :: a -> m a

-- >>= ("bind") is also present in the Haskell logo!
-- "return" is a different name for "pure" from Applicative
--   (difference exists for historic reasons)
-- we don't need to specify "return" since it equals "pure"

-- Monad laws:
-- return and >>= should satisfy the equalities
--   return a >>= k                  ==  k a
--   m        >>= return             ==  m
--   m        >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

-- let >=> be the Kleisli arrow
-- i.e.  the function defined by
--   (f >=> g) :: (a -> m a) -> (b -> m b) -> (a -> m b)
--   (f >=> g) x = f x >>= g
-- the laws can be rewritten as:
--   return    >=> g      == g               -- return is left-neutral
--   f         >=> return == f               -- return is right-neutral
--   (f >=> g) >=> h      == f >=> (g >=> h) -- law of associativity


-- trivial monad
instance Monad Burrito where
    (>>=) :: Burrito a -> (a -> Burrito b) -> Burrito b
    -- unwrapp the value, apply the function
    (Burrito x) >>= f = f x


-- Maybe monad
instance Monad Maybe' where
    (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
    -- if the value if is not Nothing, apply the function
    Nothing'  >>= _    = Nothing'
    (Just' x) >>= f    = f x

-- the Maybe monad is useful to propagate the value "Nothing"
-- this allows us to avoid nested case distinctions

-- Recall:
type Map k v = [(k,v)]

lookup' :: Eq k => Map k v  -> k -> Maybe v
lookup' [] _  = Nothing
lookup' ((k2,v):tail) k1  = if k1 == k2 then Just v else lookup' tail k1

type Name = String
mommyTable :: Map Name Name
mommyTable = [("Bob","Anna"),("Anna","Mathilda"),("John","Eve")]

-- old definition without bind
grandmother :: Name -> Maybe Name
grandmother p =
    case lookup' mommyTable p of
        Nothing     -> Nothing
        Just mother -> lookup' mommyTable mother

-- equivalent definition with bind
grandmother' :: Name -> Maybe Name
grandmother' p = (lookup' mommyTable p) >>= lookup' mommyTable

-- can also be written using do notation
grandmother'' p =
    do
        mother <- lookup' mommyTable p
        grandmother <- lookup' mommyTable mother
        return grandmother

-- or even simpler
grandmother''' p =
    do
        mother <- lookup' mommyTable p
        lookup' mommyTable mother


-- the list monad
instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    -- []     >>= _ = []
    -- (x:xs) >>= f = (f x) ++ (xs >>= f)
    -- list   >>= f = concatMap f list
    -- >>=          = flip . concatMap
    Empty     >>= _   = Empty
    Head x xs >>= f   = (f x) `conc` (xs >>= f)

-- used to model non-determinism / branching
allTuples :: [a] -> [b] -> [(a,b)]
allTuples xs ys =
    do
        y <- ys
        x <- xs
        return (x,y)

-- without do notation
allTuples' :: [a] -> [b] -> [(a,b)]
allTuples' xs ys = -- xs >>= (\x -> (ys >>= (\y -> [(x,y)])))
    xs >>=
    (
        \ x ->
        (
            ys >>=
            ( \ y -> [(x,y)] )
        )
    )

-- try e.g.
tuples = allTuples [1..10] ['a'..'z']



-- the IO monad

-- something like the actual IO monad cannot be implemented by the user,
-- because it features impure behavior that is hardwired in the compiler

-- recall that conceptually, a value of type "IO a" is a function
--   RealWorld -> (a, RealWorld)

-- we implement a greatly simplified version

type Input = String
type Remainder = String
type Output = String

data IO' a = IO' (Input -> (a,Remainder,Output))

-- intuitively:
-- get input String
-- process a part of it
-- return
-- * result of type a
-- * remainder (unprocessed part of the input String)
-- * console output


-- make IO' a Functor
instance Functor IO' where
    -- apply the function to the result of the command,
    -- leave console output and remainder unchanged
    fmap :: (a -> b) -> IO' a -> IO' b
    fmap f (IO' t) =
        IO'
        (
            \ input ->
                let
                    (x, remainder, output) = t input
                in
                    (f x, remainder, output)
        )


-- make IO' an Applicative
instance Applicative IO' where
    -- don't touch the input String
    pure :: a -> IO' a
    pure x = IO' (\ input -> (x,input,""))

    -- first execute the command producing the function,
    -- then execute the command producing the value
    -- then compose the two
    (<*>) :: IO' (a -> b) -> IO' a -> IO' b
    (IO' commandf) <*> (IO' commandx) =
        IO'
        (
            \ input ->
            let
                (f, remainderf, outputf) = commandf input
                (x, remainderx, outputx) = commandx remainderf
            in
                (f x, remainderx, outputf ++ outputx)
        )


-- make IO' a Monad
instance Monad IO' where
    -- first compute the result of the command,
    -- then apply the function, obtaining a new command
    -- then compute its result
    (>>=) ::  IO' a -> (a -> IO' b) -> IO' b
    (IO' commandx) >>= f =
        IO'
        (
            \ input ->
            let
                (x, remainderx, outputx) = commandx input
                (IO' commandfx) = f x
                (fx, remainderfx, outputfx) = commandfx remainderx
            in
                (fx, remainderfx, outputx ++ outputfx)
        )


-- let's write a few functions using our new IO' monad

-- get a single character
getCharacter :: IO' Char
getCharacter = IO' t
    where
        t "" = error "empty input"
        t (h:t) = (h,t,"")

-- get a whole String
get :: IO' String
get =
    do
        x <- getCharacter
        if x == '\n'
        then
            return ""
        else do
            rest <- get -- recursion
            return (x:rest)

-- print an output
put :: String -> IO' ()
put s = IO' (\ input -> ((), input, s ++ "\n"))

-- let's try to write an echo command
echo :: IO' ()
echo =
    do
        x <- get
        let echo_string = "Echo: " -- this notation only works inside do
        put $ echo_string ++ (map toUpper x)


-- to actually try this out, we need to couple our IO' with Haskell's IO

apply :: IO' a -> Input -> (a,Remainder,Output)
apply (IO' f) = f

convert :: IO' () -> IO ()
convert m = interact
    (\ input ->
        let
            (x, remainder, output) = apply m input
        in
            output
    )

echo' :: IO ()
echo' = convert echo


-- IO is strict
strictness :: IO ()
strictness = undefined >> (putStrLn "Nonstrict!")
--  (undefined >>= (\x -> putStrLn "Nonstrict!")
