Introduction to functional programming in Haskell
=================================================

This is material for an introduction to functional programming on Haskell, with a focus on monads.
It was originally created for a series of 4 talks (~360 minutes) that were held in the context of a lecture on semantics by Roland Meyer and Jürgen Koslowski at Technische Universität Braunschweig in the summer of 2018.

[Semantics - Summer 2018 @ Technische Universität Braunschweig](https://www.tcs.cs.tu-bs.de/teaching/Semantics_SS_2018.html)

Lecture material
----------------

* [Syllabus](syllabus.md)

* [Literature](literature.md)

* [Notes on the history of functional programming and Haskell](history.md)

* [Exercise Sheet 1](exercises/sheet1.md)

    ([Code snippets](exercises/sheet1.hs), [Solution](exercises/solutions/sheet1_solution.hs))

* [Exercise Sheet 2](exercises/sheet2.md)

    ([Code snippets](exercises/sheet2.hs), [Solution](exercises/solutions/sheet2_solution.hs))

* Code examples / explanation (see below)

Code examples / Explanation
---------------------------

#### Basic

01. [Values and types](code/01_values_and_types.hs)

02. [Functions](code/02_functions.hs)

03. [Lists](code/03_lists.hs)

### Advanced concepts

04. [Higher-order functions](code/04_higher_order.hs)

05. [Types](code/05_types.hs)

06. [Type classes](code/06_type_classes.hs)

07. [Lazy evaluation](code/07_lazy_evaluation.hs)

08. [The pitfalls of type inference](code/08_type_inference.hs)

### Monads

09. [IO in functional languages](code/09_IO.md)

10. [Monads](code/10_monads.md)

11. [do notation](code/11_do_notation.md)
    * ([Code examples](code/11_do_notation_examples.hs))

12. [Examples of monads](code/12_monads.hs)

13. [Parsing using monads](code/13_parsing.hs)

14. [The monads State and ST](code/14_state.hs)

Note: The code has been tested using GHC version 8.10.7 on Windows 10 version 21H2.

License
-------

Copyright 2022 Sebastian Muskalla

Published under the MIT License, see [LICENSE](LICENSE)
