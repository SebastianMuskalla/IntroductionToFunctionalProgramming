--
-- Introduction to functional programming in Haskell
--
-- Exercise sheet 2
--
-- Code snippets
--

--
-- Exercise 1
--

-- data Exception a = ...


--
-- Exercise 2
--

data Arith = Const Integer | Div Arith Arith deriving (Eq,Show)

eval :: Arith -> Integer
eval (Const n) = n
eval (Div l r) = (eval l) `div` (eval r)

good :: Arith
good = Div (Div (Const 1932) (Const 2)) (Const 23)

bad :: Arith
bad = Div (Const 1) (Const 0)


--
-- Exercise 3
--

data Expression =
        Constant Integer
    |   Add Expression Expression
    |   Subtract Expression Expression
    |   Multiply Expression Expression
    |   Divide Expression Expression

testexpr = Add
            (Constant 2)
            (Multiply
                (Constant 3)
                (Subtract (Constant 4) (Divide (Constant 2) (Constant 3))))


data BExpression = T Term | A BExpression Term | S BExpression Term -- recursion, addition, subtraction
data Term        = F Factor | M Term Factor | D Term Factor  -- recursion, multiplication, division
data Factor      = C Integer | B BExpression -- expression in brackets

testbexpr =
    A (A (T $ F $ C 1) (F $ C 2))
      (M (F $ C 3) (B $ S (T $ F $ C 4) (D (F $ C 2) (C 3))))


convert :: BExpression -> Expression
convert = undefined

-- bExpressionParser :: Parser BExpression

-- you can test your parser using e.g.
-- convert $ parse bExpressionParser "1 + 2 + 3 * (4 - 2 / 3)"

-- here are some test cases
-- testStrings = ["1 + 2 + 3", "1 + 2 * 3", "1 + 2 + 3 * (4 - 2 / 3)"]
-- expectedResults = ["((1 + 2) + 3)", "(1 + (2 * 3))", "((1 + 2) + (3 * (4 - (2 / 3))))"]
-- testCase0 = show $ convert $ parse bExpressionParser (testStrings !! 0)
-- test0 = testCase0 == (expectedResults !! 0)
-- testCase1 = show $ convert $ parse bExpressionParser (testStrings !! 1)
-- test1 = testCase1 == (expectedResults !! 1)
-- testCase2 = show $ convert $ parse bExpressionParser (testStrings !! 2)
-- test2 = testCase2 == (expectedResults !! 2)
-- tests = [test0, test1, test2]
