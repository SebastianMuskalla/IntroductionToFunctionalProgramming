Introduction to functional programming in Haskell
=================================================

Syllabus
--------

### History of functional programming and Haskell

### Basic Haskell

* **Basic data types and their values**: `Bool`, `Int`, `Integer`, ..., tuples, `()`, `undefined`

* **Functions**: `if-then-else`, guards, pattern matching, `let ... in`, `where`, recursion, currying, sections

* **Lists**: basic operations, pattern matching, sorting

### Advanced concepts:

* **Higher-order functions**: composition, `map`, `foldl`, `$`

* **Data types**: `type`, `newtype`, `data`, constructors, enums, recursive types, `Maybe`, records

* **Type classes**: Constraints, defining classes, implementing instances, `Eq`, `Ord`, `Show`, `Num`

* **Lazy evaluation**: strictness, infinite lists, memoization

* **The pitfalls of type inference**: type holes, monomorphism restriction, mutually recursive binding groups, polymorphic recursion

### Monads

* **IO in functional languages**: non-pure IO (SML), linear types (Clean), IO in Haskell
* **Basic definitions**: `Functor`, `Applicative`, `Monad`
* **Examples**: Trivial monad, `Maybe`, `List`, `IO`, parsing as monad, `Alternative`, `MonadPlus`, `State`, `ST`
