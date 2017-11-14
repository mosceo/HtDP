HtDP 2e, Part 2. Review.
========================

* The most interesting programs deal with *data of arbitrary size*.
  Often we can't predict the size of data. If the size of data is known,
  use structs, otherwise use lists.

* There are two ways to create a list: `(cons 1 (cons 2 (cons 3 '())))` or `(list 1 2 3)`.
  There are also predicates `cons?` and `list?`.

* The usual template for a function consuming lists:
  
   ```racket
   (define (func l)
     (cond
       [(empty? l) ...]
       [else (... (first l) ...
              ... (func (rest l)) ...)]))
   ```

* Sometimes you traverse a list and call an auxiliary function on each element.
  Othertimes you traverse a list and combine the elements into something else.

* Create and refer to a separate function template whenever you are developing
  a template for a data definition that refers to other data definitions.

* Self-references call for recursive functions. Cross-references call for auxiliary functions.

* Design one template per data definition. Formulate auxiliary function definitions
  when one data definition points to a second data definition.

* Sometimes it is easier to *solve a general problem* than a narrow one.
  In this case you should solve the general problem and define the main function
  as a specific use of the general function.

* The key insight is that just because a function has to deal with more inputs than another function
  does not mean that the former is more complex. Generalizations often simplify function definitions.

* Complex problems are solved by *decomposition-composition*. You decompose a complex problem into separate problems.
  You need two pieces: functions that solve the separate problems and data definitions
  that compose these separate solutions into a single one.

* Programs are collections of definitions: structure type definitions, data definitions,
  constant definitions, and function definitions. And don’t forget tests.

* Design one function per task. Formulate auxiliary function definitions
  for every dependency between quantities in the problem.

* *Be pragmatic!* Don't write a function if you already have one. All programming languages provide
  many built-in operations and many library functions. Explore them first.

* Iterative refinement is a crucial design principle.
  Sometimes it is impossible to design an application without it.

