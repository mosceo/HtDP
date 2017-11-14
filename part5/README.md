HtDP 2e, Part 5. Review.
========================

* Functions typically decompose their arguments into their immediate structural components
  and then process those components. If one of these immediate components belongs
  to the same class of data as the input, the function is structurally recursive.

* There is also *generative recursion*. The inputs of an algorithm represent a problem.
  An algorithm tends to re-arrange a problem into a set of several problems,
  solve those, and combine their solutions into one overall solution.
  Often some of these newly generated problems are the same kind of problem as the given one.
  In these cases, the algorithm is recursive but its recursion uses
  newly generated data not immediate parts of the input data.

* The choice of a data representation for a problem affects our thinking about the computational process.
  Be prepared to backtrack and to explore different data representations.

* Designing generative recursion functions we should think about the following four questions:
  - What is a trivially solvable problem?
  - How are trivial solutions solved?
  - How does the algorithm generate new problems that are more easily solvable than the original one?
    Is there one new problem that we generate or are there several?
  - Is the solution of the given problem the same as the solution of (one of) the new problems?
    Or, do we need to combine the solutions to create a solution for the original problem?
    And, if so, do we need anything from the original problem data?

* Generative recursion is not always superior to structural recursion. It is difficult to understand,
  requires some deep insight and sometimes slower (quick-sort vs. insertion-sort for small arrays).
  Experience shows that most functions employ structural design. When we encounter a situation
  that could use either structural or generative recursion, start with a structural version.

* The design of an algorithm starts with an informal description of a process of how to create a problem
  that is more easily solvable than the given one and whose solution
  contributes to the solution of the given problem.

* When creating a function that applies generative recursion,
  in the purpose statement we should articulate not only WHAT the function does,
  but also HOW it does it (just a few words). Example:
  
  ```racket
  ; Number -> Image
  ; generative creates Sierpinski Δ of size side by generating
  ; one for (/ side 2) and placing one copy above two copies
  ```

* Every function designed according to the old recipe either produces an answer or raises an error.
  Generative recursion adds an entirely new aspect to computations: non-termination.
  Algorithms may not terminate for some inputs. You should present some argument that
  the program will terminate and warn the readers of your code of "bad" input.

* *Parsing* is so complex and so central to the creation of full-fledged software applications,
  that most undergraduate curricula come with at least one course on parsing.

* ISL+ treats numbers like 2.9999 as exact rational numbers and computation
  with them can take a lot of time. Use #i2.9999 if needed.

* While we have dubbed it generative recursion,
  most computer scientists refer to these functions as *algorithms*.

* For truly clever algorithms, software companies employ highly paid specialists, domain experts,
  and mathematicians. Programmers in general implement only simple or well-known algorithms.

* When programs grow large, data abstraction becomes a critical tool for the construction of a program’s components.

* The best preparation is practice.

