--
-- Introduction to functional programming in Haskell
--
-- 6: Type classes
--

-- language pragma that allows us to specify the signature for functions
-- in the definitions of instances of type classes
-- for the sake of clarity
{-# LANGUAGE  InstanceSigs #-}

-- Peano definition of the Naturals
data Natural = Zero | Succ Natural


two = Succ (Succ Zero)
five = Succ (Succ (Succ two))

plusThree :: Natural -> Natural
plusThree n = Succ (Succ (Succ n))

convert :: Natural -> Integer
convert Zero     = 0
convert (Succ n) = 1 + convert n

-- type class Eq
-- make "==" available for Naturals
instance Eq Natural where
    (==) :: Natural -> Natural -> Bool
    (==) Zero     Zero     = True
    (==) Zero     _        = False
    (==) _        Zero     = False
    (==) (Succ n) (Succ m) = (n == m)
    --       (/=) :: a -> a -> Bool
    --       x == y = not (x /= y)
    --        x /= y = not (x == y)


-- type class Ord
-- class Eq a => Ord a
-- a type needs to be instance instance of Eq so that it can be an instance of Ord
instance Ord Natural where
    (<=) :: Natural -> Natural -> Bool
    Zero     <= _        = True
    _        <= Zero     = False
    (Succ n) <= (Succ m) = n <= m

-- we have to define <= (or, alternative, the function compare)
-- we then automatically get the functions
-- compare, max, min
-- and the operators
-- <=, >=, <, >
-- that behave as expected
-- see: https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#t:Ord

-- type class Show
-- for String conversion
-- implictly used by GHCi whenever you type in a value
instance Show Natural where
    show :: Natural -> String
    -- show n = show $ convert n
    show (Zero) = "Zero"
    show (Succ n) = "Succ (" ++ show n ++ ")"

-- equivalently:
-- let the compiler do the work for us
data Natural' = Zero' | Succ' Natural'
    deriving (Eq,Ord,Show)

-- type class Num
-- for number types
instance Num Natural where
    Zero + sth = sth
    (Succ n) + sth = Succ (n + sth)
    Zero * sth = Zero
    (Succ n) * sth = sth + (n * sth)
    abs = id
    signum Zero = 0
    signum _ = 1
    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n-1))
    negate _ = undefined

-- note that "let x = 10 :: Natural" now works in GHCi!


-- a custom type class
-- a type needs to be in class Eq in order to be able to to be in class Halfable
class Eq a => Halfable a where
    half :: a -> a

-- make our type natural an instance of Halfable
instance Halfable Natural where
    half Zero = Zero
    half (Succ Zero) = Zero
    half (Succ (Succ n)) = Succ (half n)

-- we can make an existing type an instance of Halfable
instance Halfable Integer where
    half n = n `div` 2

--instance Halfable String where
-- or more generally
instance Eq a => Halfable [a] where
    half s = take (length s `div` 2) s

-- we can make sure that whenever some type a is an instance of Halfable
-- Maybe a is also an instance of Halfable!
instance Halfable a => Halfable (Maybe a) where
    half :: Maybe a -> Maybe a
    half Nothing = Nothing
    half (Just x) = Just $ half x
