Introduction to functional programming in Haskell
=================================================

10: Monads
----------

We have just seen that in order to do IO in haskell, we need the bind operator\
`>>= :: IO a -> (a -> IO b) -> IO b`.

Let us replace `IO` by an arbitrary type constructor `M`\
(being a type constructor means that `M` turns some type `a` into a new type `M a`).\
It turns out, that such the operator **bind**\
`>>= :: M a -> (a -> M b) -> M b`\
is useful not just for `IO`, but for many type constructors,\
including many type constructors we already know (Lists, `Maybe`).

We call a type constructor `M` that has such an operator a **monad**\
(assuming it satisfies some additional properties.

#### Monad tutorials

There are many monad tutorials,\
see e.g. <https://wiki.haskell.org/Monad_tutorials_timeline>\
The best one for programmers (Philip Wadler at YOW! 2013):\
<https://www.youtube.com/watch?v=yjmKMhJOJos>


#### A brief history:

* originally a concept in category theory
* 1989: Eugenio Moggi "Computational lambda-calculus and monads" (using monads for denotational semantics)
* then introduced to Haskell to be able to comfortably write a Haskell compiler in Haskell by Philip Wadler

#### Explanation:

* Some people compare Monads to contexts, containers, computations, burritos, ...
* Some people claim: "Analogies and metaphors will not lead to understanding."
* The truth is somewhere in the middle:
    * Metaphors will help in some applications and get you are more intuitively understanding of monads,
    * but no metaphor will work in all cases, so ...
    * to master monads, you will also need to be able to work with the formal algebraic definition

#### Monadic Myths (from <http://dev.stephendiehl.com/hask/>)

The following are wrong:
* Monads are burritos.
* Monads are impure.
* Monads are about effects.
* Monads are about state.
* Monads are about imperative sequencing.
* Monads are about IO.
* Monads are dependent on laziness.
* Monads are a "back-door" in the language to perform side-effects.
* Monads are an embedded imperative language inside Haskell.
    * Monads an be used for these purposes, or some monads satisfy these properties, but none of these properties is inherent to being a monad.
* Monads require knowing abstract mathematics.
    * (but it may help)
* Monads are unique to Haskell.
    * Monads are also implemented in OCaml, Scala, ...
    * Many libraries for imperative or multi-paradigm languages provide "monadic interfaces" that either implement a monad or at least behave similarly, e.g.
        * `Maybe` / `Nullable` / `Optional` / `...?` in many modern languages
        * Exception handling in some modern languages (see e.g. <https://github.com/RishabhRD/expected>)
        * Asynchronous concurrent programming using `Future` / `Promise` / `Deferred`

### Understanding monads

We would like to understand Monads

However: Definition in the standard library:\
`class Applicative m => Monad m where ...`\
Every `Monad` needs to be an `Applicative`,\
we need to understand `Applicative` first

However: Definition of `Applicative` in the standard library:\
`class Functor m => Applicative m where ...`\
Every `Applicative` needs to be a `Functor`,\
we need to understand `Functor` first

**See examples in [12_monads.hs](12_monads.hs)!**
