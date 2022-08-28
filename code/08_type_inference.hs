--
-- Introduction to functional programming in Haskell
--
-- 7: The pitfalls of type inference
--

-- language pragma for disabling the monomorphism restriction
{-# LANGUAGE NoMonomorphismRestriction  #-}

-- language pragma for enabling the monomorphism restriction
-- {-# LANGUAGE MonomorphismRestriction  #-}


import Data.List


-- type holes

-- in the following definition, replace the type by
-- inferPlease :: _
-- and try to compile

inferPlease :: Show a => a -> String
inferPlease x = show x


-- monomorphism restriction
-- function :: Show a => a -> String
function = show

-- try enabling the monomorphism restriction (by un-uncommenting line 11) and then compile
-- the compile will refuse to compile unless you provide a signature

-- with the monomorphism restriction disabled, the compiler will sometimes infer types that are less general than what you would expect
-- e.g.
-- old versions of the compiler infers the type
--   () -> String
-- instead of
--   Show a => a -> String
-- for function

-- the monomorphism restriction forces you to provide a type signature to prevent this from happening
-- currently, the monomorphism restriction is enabled by default in GHC
-- and disabled by default in GHCi (!)


-- mutually recursive binding groups

f :: a -> a
f x = const x g
g :: a -> Char
g y = f 'λ'

f' x = const x g'
g' y = f' 'λ'

-- check the type of f' inferred by the GHC


-- polymorphic recursion
data Crazy a = Empty | Head a (Crazy (a,a))

-- comment out the signature and try to compile
size :: Crazy a -> Int
size Empty = 0
size (Head _ rest) = 1 + 2 * size rest
