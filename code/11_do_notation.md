Introduction to functional programming in Haskell
=================================================

11: Do notation
---------------

`do` notation is syntactic sugar that makes it easier to write code involving monads

When we prefix a block of code by `do`, we can effectively omit the bind (`>>=`) operator

### Example

Consider our earlier example for `IO`:

`echo = getLine >>= putStrLn`\
resp.
`echo = getLine >>= (\ x -> putStrLn x)`\

With do notation, this code can be written as
```
    echo =
        do
            x <- getLine
            putStrLn x
```

### Note

* `do` notation is not specific to the `IO` monad,\
  it can be used for all monads, even for ones defined by the user

* `do` notation makes it look like we are writing imperative non-pure code.\
  However, it is simply a different syntax for purely functional Haskell code

### Transformation rules

There are two transformation rules that can be used to transform code using `do` notation into regular code using `>>=`

Consider a monad `M` (i.e. for each type `a`, we get a type `M a`)

1. Rule
```haskell
do { x <- m1 ; m2 }
==
m1 >>= (\ x -> do { m2 })
```
where `m1`, `m2` are monadic expressions (i.e. of type `M a`, `M b` respectively, and `x` is of type `a`)

Intuitively, we bind the result of `m1` to the name `x` and plug it into `m2`

2. Rule
```haskell
do { m }
==
m
```
where `m` is a monadic expression without unbound names

### Syntax with brackets and semicolons

As in the above example, Haskell supports formatting code using brackets (`{`,`}`) and semicolons (`;`).

For example, we can write\
`do { x <- m1 ; m 2}`\
instead of
```haskell
do
    x <- m1
    m2
```

However, this syntax often leads to unexpected compile errors (because it is fickle with respect to indentation) and hence using it is **discouraged**.

Whether one puts `do` into a new line is a matter of personal preference.

```haskell
-- this is okay
f1 = do
    input <- getLine
    putStrLn $ map toUpper input

-- this is okay, too
f2 =
    do
        input <- getLine
        putStrLn $ concatMap (\ x -> x : " ") input
```

### The return function

In addition to the operator `>>=`, every monad `M` comes with a function\
`return :: a -> M a`\
that a transforms a regular value of type `a` into a value of type `M a`.

For example, for the `IO` monad\
`return :: a -> IO a`\
takes some value `x` and constructs the command that if we execute it, performs **no** side effects and simply returns the value `x`.

(Note: `return` in Haskell sometimes serves a similar purpose as `return` in imperative programming languages, but conceptually, it is something completely different)


Rule (2) in particular means that:
```haskell
do { return x }
==
return x
```

### Example

Consider the code
```haskell
do
    x <- f
    y <- g
    z <- h
    return (x,y,z)
```
where `f` is of type `M a`, `g` is of type `M b`, and `h` is of type `M c`.\
(What is type of the whole block of code?)

We apply Rule 1. and get
```haskell
f >>= \ x ->
    do
        y <- g
        z <- h
        return (x,y,z)
```

We apply Rule 1. again and get
```haskell
f >>= \ x -> g >>= \ y ->
    do
        z <- h
        return (x,y,z)
```

We apply Rule 1. again and get
```haskell
f >>= \ x -> g >>= \ y -> h >>= \ z ->
    do
        return (x,y,z)
```
Finally, we apply Rule 2. and get
```haskell
f >>= \ x -> g >>= \ y -> h >>= \ z -> return (x,y,z)
```

#### Test it yourself

[11_do_notation_examples.hs](11_do_notation_examples.hs)

``` haskell
f =
    do
        putStrLn "evaluating f"
        return 1

g =
    do
        putStrLn "evaluating g"
        return 'a'

h =
    do
        putStrLn "evaluating h"
        return 2.0

test1 =
    do
        x <- f
        y <- g
        z <- h
        return (x,y,z)


test2 =
    f >>= \ x ->
        do
            y <- g
            z <- h
            return (x,y,z)


test3 =
    f >>= \ x -> g >>= \ y ->
        do
            z <- h
            return (x,y,z)

test4 =
    f >>= \ x -> g >>= \ y -> h >>= \ z ->
        do
            return (x,y,z)

test5 =
    f >>= \ x -> g >>= \ y -> h >>= \ z -> return (x,y,z)

f1 = do
    input <- getLine
    putStrLn $ map toUpper input

f2 =
    do
        input <- getLine
        putStrLn $ foldl (\ x y -> x ++ " " ++ y) input
```
