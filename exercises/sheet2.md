Introduction to functional programming in Haskell
=================================================

Exercise sheet 2
----------------

You can find the code snippets for this exercise in [sheet2.hs](sheet2.hs)

### Exercise 1: Exception handling with monads

Design a type constructor `Exception` such that a value of type `Exception a` is either an exception with an error message (of type `String`) or a result of type `a`.

Implement the functions necessary to make `Exception` a `Monad`.

### Exercise 2: From non-monadic to monadic code

Consider the following code that specifies a data type for simple arithmetic expressions involving constants and division, and an evaluation function.

```haskell
data Arith = Const Integer | Div Arith Arith deriving Show

eval :: Arith -> Integer
eval (Const n) = n
eval (Div l r) = (eval l) `div` (eval r)

good :: Arith
good = Div (Div (Const 1932) (Const 2)) (Const 23)

bad :: Arith
bad  = Div (Const 1) (Const 0)
```

1. Reimplement function `eval` such that it uses the trivial monad.

2. Extend the code from 1. such that dividing by zero gives an exception.\
    Use the `Exception` monad from Exercise 1.

3. Extend the code from 1. such that it counts the number of divisions.\
    Use the monad `State Int`

4. Extend the code from 1. such that it prints all intermediary results.\
    Use the `IO` monad.

How many modifications to the code do you need to make for 2. - 4.?

### Exercise 3: Parsing bracket-free expressions

1. Consider the following code that specifies a data type for arithmetic expressions.

```haskell
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
```

Implement the instance `Show Expression` with a function `show` that returns the fully bracketed, infix representation of an expression,\
    e.g. `show testexpr == "(2 + (3 * (4 - (2 / 3))))"`

2. We want to display arithmetic expressions without unnecessary brackets

The following data type helps us in doing so:

```haskell
data BExpression = T Term | A BExpression Term | S BExpression Term -- recursion, addition, subtraction
data Term        = F Factor | M Term Factor | D Term Factor  -- recursion, multiplication, division
data Factor      = C Integer | B BExpression -- expression in brackets

testbexpr =
    A (A (T $ F $ C 1) (F $ C 2))
      (M (F $ C 3) (B $ S (T $ F $ C 4) (D (F $ C 2) (C 3))))
```

Implement the instance `Show BExpression` with a function `show` that returns the infix representation of an expression with as few brackets as possible,\
    e.g. `show testbexpr == "1 + 2 + 3 * (4 - 2 / 3)"`

As usual, brackets bind stronger than `*` and `/`, which in turn bind stronger than `+` and `-`.
Operators of the same precedence are processed from left to right.

3. Implement a function `convert :: BExpression -> Expression` that converts a `BExpression` into an `Expression`

4. Implement a function `bExpressionParser :: Parser BExpression` that can parse a `String` representations of a `BExpression` back into a `BExpression`.

    Note:\
    The lecture material contains a Parser for `Expression` from 1. that may serve as a basis.

    Hint:\
    You can test your parser with\
    `convert $ parse bExpressionParser "1 + 2 + 3 * (4 - 2 / 3)"`\
    (using the `parse` function from the lecture material)

    Hint 2:\
    A naive implementation may run into an infinite loop because of the so-called left-recursion problem.
    To avoid this, you can use the following transformed grammar for `BExpression`:

```
bexpr  ::= term (addop term)*  -- 0 or more repetitions of addop term
term   ::= factor (multop factor)*  -- 0 or more repetitions of multop factor
factor ::= <some Integer>   |   '('  bexpr ')'
addop  ::= '+'   |   '-'
multop ::= '*'   |   '/'
```

### Solutions

The solutions can be found in [solutions/sheet2_solution.hs](solutions/sheet2_solution.hs). Try it yourself first though!
