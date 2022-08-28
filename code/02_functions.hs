--
-- Introduction to functional programming in Haskell
--
-- 2: Functions
--

-- function names start with lower-case letters
-- (beside constructors, later)

square :: Integer -> Integer
square(n) = n*n

-- brackets can be omitted
square' :: Integer -> Integer
square' n = n*n

-- type signature just as for values
-- functions are values of their function type
-- functions are "first-class citizens"

-- anonymous function (lambda-abstraction)
-- \ represents lam(b)da (λ)
square'' :: Integer -> Integer
square'' = \ n -> n*n

-- if-then-else
absoluteValue :: Integer -> Integer
absoluteValue n =
    if n >= 0
    then n
    else negate n

-- guards
absoluteValue' :: Integer -> Integer
absoluteValue' n
    | n >= 0    = n
    | otherwise = -n

-- check: absolute_value -5
-- what is wrong?

-- Recursion
factorial :: Integer -> Integer
factorial n =
  if n == 0
  then 1
  else n * factorial (n-1)

-- guards
factorial' :: Integer -> Integer
factorial' n
    | n == 0    = 1
    | otherwise = n * factorial (n-1)

-- pattern matching
factorial'' :: Integer -> Integer
factorial'' 0 = 1
factorial'' n = n * factorial (n-1)

-- more pattern matching
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- check: fib 30 / 100

-- mutual recursion
even' :: Integer -> Bool
even' n = (n == 0) || odd' (n-1)
odd' :: Integer -> Bool
odd' n =  (n /= 0) && even' (n-1)
-- inefficient, but well defined and correct for n >= 0

-- where & let
-- ballAreaAndVolume :: Radius -> (Area,Volume)
ballAreaAndVolume :: Double -> (Double,Double)
ballAreaAndVolume r =
    let
        -- helper values
        rsquare :: Double
        rsquare = r * r
        -- rcube :: Double -- we don't need this signature
        rcube = r * r * r
        area = 4 * pi * rsquare
        volume = (4/3) * pi * rcube
    in
        (area,volume)

-- can instead define area and volume as helper functions
ballAreaAndVolume' :: Double -> (Double,Double)
ballAreaAndVolume' radius =
    let
        -- helper values (and functions)
        squared x = x*x
        cubed x = x*x*x
        area x = 4 * pi * squared x
        volume x = (4/3) * pi * cubed x
    in
        (area radius,volume radius)

-- instead of let ... in, we can use ...where
ballAreaAndVolume'' :: Double -> (Double,Double)
ballAreaAndVolume'' r = (area,volume)
    where
        area = 4 * pi * r * r
        volume = (4/3) * pi * r * r * r

-- tuples as parameter
choose :: (Integer,Integer) -> Integer
choose (n,k) = (factorial n) `div` ((factorial k) * (factorial (n - k)))

-- versus currying
choose' :: Integer -> Integer -> Integer
-- read as:
-- choose' :: Integer -> (Integer -> Integer)
choose' n k = div enum denom
    where
        enum = factorial n
        denom = (factorial k) * (factorial (n - k))

-- easy access to partially evaluated functions
-- compare the ugly
chooseOutOf10 k = choose (10,k)
-- to the nice
chooseOutOf10' = choose' 10

-- also works for operators (so-called sections)
add5 = (+5)
add5' = (5+)
smallerThan2 = (<2)

-- be careful!
subtract1 = (-1)
subtract1' = (+(-1))
