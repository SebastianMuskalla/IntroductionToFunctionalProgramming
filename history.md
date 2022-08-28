Introduction to functional programming in Haskell
=================================================

Notes on the history of functional programming and Haskell
----------------------------------------------------------

### History of programming

* Early computers: Strict separation between program (e.g. electronic circuits) and data (e.g. punch cards)
* von Neumann architecture: program and data share the same memory (stored-program computers)
* von Neumann-style imperative programming: Commands/statements (representing state changes) vs. expressions (representing data)
* Imperative programs are sequences of commands (with jumps, e.g. realized via `goto`)
* Dijkstra 1968: "Go To statement considered harmful"
    * structured programming (`if`, `while`, ...) & procedural programming
* Procedural programs: Functionality realized using procedures
* Procedures have explicit parameters and return values, but may have side effects, like reading and writing the global state and using IO (input and output)
* John Backus (Fortran language designer) in his Turing Award lecture 1977 "Can programming be liberated from the von Neumann style?"
    *  Boost of research into functional programming

### What is functional programming?

* A (pure) function is a procedure without side effects
    * it can only read immutable parts of the global state
    * it cannot modify the global state
    * it cannot use IO
* Pure functions are functions in the sense of mathematics: If `x == y` then `f(x) = f(y)`, no matter where this expression occurs in the code (assuming `f` is a pure function)
* In functional programming, functionality is realized by composing (pure) functions
* In a purely functional language: Only pure functions; no commands/statements, only expressions

### History of functional programming

* Since the 1950s: LISP and its dialects
* After Backus' lecture:
    * Research into functional programming became popular...
    * but its practical applications were limited to special fields (e.g. Erlang since 1986 for communication systems)
* In the last ~15 years: Rise of functional programming and multi-paradigm languages

### Languages

* Non-pure functional languages: LISP (including dialects like Clojure, Scheme), ML (including dialects like (O)Caml), Erlang, ...
* Multi-paradigm languages: Python, Ruby, JavaScript, ...
* Rise and fall of Scala (around 2010)
* Since Java 8 (2014) and C++11 (2011): Some functional concepts in Java and C++
* Nowadays: Almost no new language without aspects of functional programming, e.g. Apples's Swift, Kotlin. (Counter-example: Google's Go)

### Haskell

* Since 1986
* Named after the logician Haskell Curry (1900 - 1982)
* It is the only popular pure functional language
* According to R. Bird: The most radical functional language
* Some features
    * Compiled (but also has an interpreter)
    * Non-strict semantics, lazy evaluation
    * Strong static typing (with type inference)
    * Concepts from category theory like monads
* Initially mostly a scientific project, but increasing use in practice in the last years
* Increasingly, concepts from Haskell/functional programming get implemented in other non-purely-functional languages\
    (e.g. "monadic interfaces" for Nullables/Optionals and Futures/Promises)


### Environment

* The Haskell platform including the GHC is the de-facto standard implementation
* Compiler GHC: (Glorious) Glasgow Haskell Compiler
* "Interpreter" GHCi: Glasgow Haskell Compiler interactive version
    * Warning: The default values of some settings (*language pragmas*) differ between GHCi und GHC
* No popular IDE specifically for Haskell but plugins for e.g. IntelliJ and VSCode

Get your dose of Haskell today: <https://www.haskell.org/downloads/>

### A tiny example

[history_code/main.hs](history_code/main.hs)

```
module Main where

import Data.Char

main = echo

echo =
    do
        s <- getLine
        putStrLn $ map toUpper s
```

* Either compile by `ghc Main.hs` and execute using `./Main` resp. `Main.exe`
* or load in GHCi by `ghci Main.hs` and execute the main function by typing `main`
* then type some text

## Why functional programming?

* Succinctness of the code(factor 5-10)
    * Hope that maintenance costs decrease
* Functional programming has an easier mathematical theory, supports equational reasoning
* The absence of side-effects simplifies code and enabled optimization and easier parallelization
    * Consider `x = f(a); y = g(b)`.\
      Changing the code to `y = g(b); x = f(a)` or even to `x = f(a) || y = g(b);` (i.e. evaluating in parallel) is not necessarily valid under the presence of side effects, but it is valid in a functional language.\
        (But: Automatic optimizations do not work very well in practice yet)
* Declarative rather than imperative programming style

### Imperative vs. declarative programming

* Declarative programming: Telling the computer *what* to do
* Imperative programming: Telling the computer *how* to do it

#### Example: Computing the first n square numbers

*In Haskell, declarative:*

[history_code/square_numbers.hs](history_code/square_numbers.hs)

```haskell
numbers = [1..]
square = \ x -> x*x
squareNumbers = map square numbers
firstSquares n = take n squareNumbers

main =
    do
        inputString <- getLine
        print $ firstSquares (read inputString :: Int)
```
* **Let** `numbers` be the positive numbers
* **Let** `square` be the function that squares
* **Let** `squareNumbers` be the list of all squares of positive numbers
* **Let** `firstSquares` be the function that takes the first n square numbers from the list
* (Plus `main` function that reads a number n and prints the first n square numbers)

(Open the program with `ghci`, then type `main`, then type a number)

*In Java, imperative:*

[history_code/SquareNumbersImperative.java](history_code/SquareNumbersImperative.java)

```java
import ...

public class SquareNumbersImperative
{
    public static void main (String[] args)
    {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        List<Integer> squares = firstSquares(n);
        System.out.println(squares.toString());
    }

    static int square(int n)
    {
        return n * n;
    }

    static List<Integer> firstSquares (int n)
    {
        List<Integer> numbers = new ArrayList<>();
        for (int i = 1; i <= n; i++)
        {
            numbers.add(i);
        }

        List<Integer> squares = new ArrayList<>();
        Iterator<Integer> itr = numbers.iterator();
        while (itr.hasNext())
        {
            Integer num = itr.next();
            Integer square = square(num);
            squares.add(square);
        }
        return squares;
    }
}
```

* **Create** a new list
* **Insert** the numbers from 1 to n into it in ascending order
* **Create** another list.
* **Iterate** over the first list from the beginning to the end
* In each step, take a number from the first let, **square** it and **append** it to the end of the second list
* **Return** the resulting list of squares
* (Plus `main` method that reads a number n and prints the first n square numbers)

(Compile the program with `javac`, run it with `java`, then type a number)


The code can of course be optimized. It is written like this to highlight the issue.\
Using features that were introduced with Java 8, we can write a declarative version

*In Java, declarative:*

[history_code/SquareNumbersDeclarative.java](history_code/SquareNumbersDeclarative.java)

```java
import ...

public class SquareNumbersDeclarative
{
    public static void main (String[] args)
    {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        List<Integer> squares = firstSquares(n);
        System.out.println(squares.toString());
    }

    static List<Integer> firstSquares (int n)
    {
        IntStream numbers = IntStream.iterate(1, i -> i + 1);
        IntStream squares = numbers.map((x) -> x * x);
        return squares.boxed().limit(n).collect(Collectors.toList());
    }
}
```
