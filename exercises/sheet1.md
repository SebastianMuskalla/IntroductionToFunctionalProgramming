Introduction to functional programming in Haskell
=================================================

Exercise sheet 1
----------------

You can find the code snippets for this exercise in [sheet1.hs](sheet1.hs)


### Exercise 1: Typing

1. For each of the following expressions, determine the most general type.

```haskell
[True]
[]
\ x -> x
\ x y -> (y, x)
\ x y -> [y, x]
\ x -> (if x == [] then x else x)
head
\ xs -> tail xs
f x y z = if x then y else z
\ xs -> tail (tail xs)
g x y z = if x == y then y else z
```

2. For each of the functions `h` to `m` determine the most general type.

```haskell
data Tree a =Leaf | Node [a] (Tree a) (Tree a)

h Leaf = "c"
h (Node "x" l r) = "x"
h (Node x l r) = x
i (Node x l r) = id
j (Node [] l r) = l
k Nothing = Nothing
l (Node a b c) = a
m (Node x l r) = Node [length x] (m l) (m r)
```

If you struggle with the exercise, you can use `:t` in the GHCi to show the type of an expression

### Exercise 2: Permute and sort

1. Write a function `permutations :: [Integer] -> [[Integer]]` that, given a list, computes a list containing all its permutations.

    What is the most general type of your function?

2. Write a function `idioticSort :: [Integer] -> [Integer]` that takes a list and sorts it by computing all permutations and returning the first one that is sorted.

    Note: `idioticSort` is a non-randomized version of Bogosort.

### Exercise 3: Higher-order functions

Write a function ...

1. `flatMap :: (a -> [b]) -> [a] -> [b]` that maps a function of type `a -> [b]` over a list and flattens the result.

2. `partition' :: (a -> Bool) -> [a] -> ([a],[a])` that partitions a list into the elements that satisfy a given predicate and the ones that do not satisfy the predicate.

3. `length' :: [a] -> Int` that computes the length of a list using `foldl`.

4. `minimumWrt :: (a -> Integer) -> [a] -> a` that computes the minimum of a given list with respect to a given evaluation function.

    What is the most general type of your function?

5. `sortWrt :: (a -> a -> Bool) -> [a] -> [a]` that sorts a list with respect to a given comparison operator.

    Which properties should the operator satisfy such that its usage makes sense?

### Exercise 4: Davis-Putnam

We want to implement the Davis Putnam algorithm for checking satisfiability in propositional logic.

1. * A *formula* (in conjunctive normal form) is a set of clauses, seen as their conjunction
    * A *clause* is a set of literals, seen as their disjunction
    * A *literal* is either a positive or a negated occurrence of a variable.
    * The *variables* are taken from a countable set.
    * The *negation* `¬L` of a literal `L` is the literal for the same variable with the opposite polarity.

    Design appropriate data types in Haskell.

    (How can the special formulas `true` and `false` be represented?)

2. If `F` is some formula and `L` is a literal, then the formula `F[L]` is obtained as follows:
    * Replace all occurrences of `L` by `true`
    * Replace all occurrences of `¬L` by `false`
    * Remove all occurrences of `false` from a clause
    * Remove all clauses containing an occurrence of `true`

    (What happens if a clause / the formula becomes empty?)

    Design a function `assign :: Formula -> Literal -> Formula` that implements this (i.e. `assign F L` should return `F[L]`)

3.  The Davis Putnam algorithm applies the following rules to a formula until one either finds a satisfying assignment or has proven the formula to be unsatisfiable.

    * Unit:\
        If a formula `F` contains a clause consisting of a single literal `L`, `F` is satisfiable if and only if `F[L]` is satisfiable.

        Design a function `unitLiteral :: Formula -> Maybe Literal` that returns a unit literal if one exists.

    * Pure:\
        If a formula `F` contain only occurrences of literals `L` with the same polarity for some variable (i.e. all literals for that variable are positive, or all literals are negated), then `F` is satisfiable if and only if `F[L]` is satisfiable.

        Design a function `pureLiteral :: Formula -> Maybe Literal` that returns a pure literal if one exists.

    * Split:\
        A formula `F` is satisfiable if `F[L]` or `F[¬L]` is satisfiable for some literal `L`.

    * Design a function `sat :: Formula -> Bool` that checks whether a formula is satisfiable by using the Davis Putnam rules recursively.

    * Advanced version:\
        Write a function `sat' :: Formula -> Maybe [Literal]` that checks whether a formula is satisfiable and if it is also returns a satisfying assignment.

### Solutions

The solutions can be found in [solutions/sheet1_solution.hs](solutions/sheet1_solution.hs). Try it yourself first though!
