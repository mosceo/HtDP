HtDP 2e, Part 4. Review.
========================

* Common generalizations involve many self-references in one data definition or a bunch
  of data definitions that refer to each other. These forms of data have become ubiquitous.

* For intertwined data definitions, create one template per data definition.
  Create them in parallel. Make sure they refer to each other in the same way the data definitions do.

* You must design as many functions in parallel as there are data definitions.
  Each function specializes for one of the data definitions; all remaining arguments remain the same.

* Check for all self references and cross references. Look at the arrows in data definitions.
  For each arrow in the data definitions, include an arrow in the templates.
  Now replace the arrows with actual function calls.

* Each arrow leads to a function call. It is called symmetry.

* *Iterative refinement* comes form science. A scientist creates a model to represent some part of the world.
  Then he tests it and refines until the discrepancies with reality are small enough.

* Complicated situations call for a refinement process to get to a sufficient data representation
  combined with the proper functions. The process starts with the essential pieces of information
  and adds others as needed.

* XML is really a family of languages. People define dialects for specific channels of communication.
  For example, XHTML is the language for sending web content in XML format.

* You should design a separate function whenever you encounter a complex form of data.

* You can visualize an S-expression as a multi-branch tree where each leaf
  is an Atom (Symbol, Number, Stirng) and each node is a list containing
  its children in order. Example:
  
  ```racket
  '(10 (20 'apple ("one" 'two)) "apple")
  ```

  An X-expression is a subset of S-expressions. It is a list, where the first element is always a symbol,
  then goes an optional list of attributes, then goes zero ore more S-expressions. Examples:
  
  ```racket
  '(machine)
  '(machine ((cores "2") (os "Linux")))
  '(machine ((cores "2") (os "Linux")) (core) (core))
  ```

  XEnum, XItem and XWord form a subset of X-epressions and represent unordered lists.
  Lists can be nested. Example:

  ```racket
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))
    (li (ul
         (li (word ((text "1")))))))
  ```

* Programmers devise simple, special-purpose languages for administrators to configure the system.
  These special languages are also known as *domain-specific languages* (DSL).

* A DSL can be a sublanguage of XML. Here is an example. A possible configuration of a finite state machine:

  ```xml
   <machine initial="white">
     <action state="white" next="black" />
     <action state="black" next="white" />
   </machine>
  ```

* Some functions can be simplified. It is better to design a function in a systematic manner and then simplify it,
  as opposed to creating a simplified version in the first place. The code will be less buggy.

* If a data definition A refers to a data definition B, then the template function-for-A refers to function-for-B
  in the exact same place and manner. Otherwise the design recipes work as before, function for function.

* When a function has to process two types of complex data, you need to distinguish three cases.
  First, the function may deal with one of the arguments as if it were atomic. Second, the two arguments
  are expected to have the same structure, and the function traverses them in a parallel manner.
  Third, the function may have to deal with all possible combinations separately. In this case,
  you make a two-dimensional table that along one dimension enumerates all kinds of data
  from one data definition and along the other one deals with the second kind of data.

* When a function consumes two complex data types, create a table that matches all cases for the first type
  with all cases for the second. For every cell in the table create a functional example and a cond clause.
  The key idea is to translate the data definitions into a table that shows all feasible and interesting combinations.

* If a function consumes three complex data types, you will have to imagine a three-dimensional table.

* You have now seen all forms of structural data that you are likely to encounter
  over the course of your career, though the details will differ.

* The teaching languages support both *exact* and *inexact numbers*.
  Arithmetic operations preserve exactness when possible;
  they produce an inexact result when necessary.

* Plain Racket interprets all decimal numbers as inexact numbers; it also renders all real numbers as decimals,
  regardless of whether they are exact or inexact. A programmer can force Racket to interpret numbers
  with a dot as exact by prefixing numerical constants with `#e` (`#e3.14`).

