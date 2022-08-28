--
-- Introduction to functional programming in Haskell
--
-- 1: Basic data types and their values
--

-- Booleans

-- Bool = False | True
b1 :: Bool
b1 = False

-- single assignment
-- b1 = True
-- does not compile

b2 = True

b3 = otherwise

b4 = True == True

b5 = True /= False

b6 = True > False

b7 = not b6

b8 = b6 && b7

b9 = b6 || b7


-- Numbers

-- Int: Fixed precision integers
-- precision is machine dependent
-- at least 29 bit data (+1 bit sign)
i1 :: Int
i1 = 0

i2 = -5

i3 = 3 + 7

i4 = 4 * 5

i5 = 8 / 4 -- check the type (using :t)

-- div is a function (prefix notation)
-- use backticks (`) to use it as operator (with infix notation)
i6 = 8 `div` 4
i7 = div 8 4

-- +,*,/, ... are operators (infix notation)
-- use brackets to use it as function (with prefix notation)
i8 = (+) 3 7
i9 = 3 + 7

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

ib1 = 5 > 3

ib2 = 5 /= 5

ib3 = intMin - 1 == intMax


-- Integer: arbitrary precision integer numbers
integer0 :: Integer
integer0 = 0

-- int0 == integer0
-- type error

integerMax = toInteger(intMax)

integerMaxSquared = integerMax * integerMax

minus1 :: Integer
minus1 = negate 1

plus1 = abs minus1

sign = signum (-5)


-- Float: low precision floating point numbers
-- usually 32 bit
f1 :: Float
f1 = fromIntegral intMax

f2 :: Float
f2 = logBase 2 f1

f3 :: Float
f3 = sin pi

f4 :: Float
f4 = sin 2*pi -- ??? weird result ???

f5 :: Float
f5  = sin (2*pi)


-- Double: high precision floating point numbers
-- usually 64 bit

-- compare to float3 and float5
d1 :: Double
d1 = sin pi

d2 :: Double
d2 = sin (2*pi)

-- Rational: arbitrary precision rational numbers
r1 :: Rational
r1 = fromIntegral intMax

r2 = r1 / (r1  + 1)

-- compare with
f6 :: Float
f6 = f1 / (f1 + 1)


-- Char: Unicode characters
c1 = 'a'
c2 = succ c1

c3 = '♥'

cb1 = 'a' < 'A'

cnum1 = fromEnum 'a'
cnum2 = fromEnum 'A'

-- Tuples

--  if t1, ..., tn are types, then (t1, ..., tn) is a type!
integerTuple :: (Integer,Integer)
integerTuple = (5,6)

mixedTuple :: (Integer,Char)
mixedTuple = (300,'\n')

tupleInt = fst mixedTuple
tupleChar = snd mixedTuple

largeTuple :: (Int,Integer,Float,Double,Rational,Char)
largeTuple = (0,0,0,0,0,'0')

-- () is the only value of type ()
emptyTuple :: ()
emptyTuple = ()

-- ... well, it is almost the only value, see below


-- Undefined
undefinedBool :: Bool
undefinedBool = undefined

undefinedTuple :: ()
undefinedTuple = undefined

undefinedInteger :: Integer
undefinedInteger = undefined

-- check :t undefined
