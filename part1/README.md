HtDP 2e, Part 1. Review.
========================

* In general, when a problem refers to distinct tasks of computation,
  a program should consist of one function per task and a main function that puts it all together.
  Here is a simple slogan: *Define one function per task*.

* For every constant mentioned in a problem statement, introduce one constant definition.

* A *batch program* consumes all of its inputs at once (maybe from files) and computes its result.
  An interactive program consumes some of its inputs,
  computes, produces some output, consumes more input, and so on.

* A good program comes with a short write-up that explains what it does,
  what inputs it expects, and what it produces. Ideally, it also comes with
  some assurance that it actually works.

* Sometimes to write a program you might need some knowledge from other fields
  (e.g., physics or biology). It is called *domain knowledge*.

* A program consumes information and turns it into some form of data.
  Then it processes the data. When it is finished, it turns the resulting data into information again.

* The first question we have to answer is how to represent information as data in our program.
  We write data definitions that describe classes of data.
  
  Examples:

  ```racket
  ; A PositiveNumber is a Number greater or equal to 0. 
  ; A TrafficLight is one of the following Strings:
  ; – "red"
  ; – "green"
  ; – "yellow"
  ; interpretation the three strings represent the three 
  ; possible states that a traffic light may assume
  ```

* There are six steps in the design recipe for functions:
  
  1. **From Problem Analysis to Data Definitions**<br>
     Identify the information that must be represented and how it is represented
     in the chosen programming language.
     Formulate data definitions and illustrate them with examples.
  
  2. **Signature, Purpose Statement, Header**<br>
     State which data the desired function consumes and produces.
     Articulate what the function computes as a concise one-line statement.
     Define a stub that lives up to the signature.
  
  3. **Functional Examples**<br>
     Work through examples that illustrate the function’s purpose.
  
  4. **Function Template**<br>
     Translate the data definitions into an outline of the function.
  
  5. **Function Definition**<br>
     Fill in the gaps in the function template. Exploit the purpose statement and the examples.
  
  6. **Testing**<br>
     Articulate the examples as tests and ensure that the function passes all.
     Doing so discovers mistakes and also helps others read and understand the definition
     when the need arises—and it will arise for any serious program.

* Here are the components that a fuction consists of.

  ```racket
  ; Number -> Number                                signature
  ; computes the area of a square with side len     purpose statement
  ; given: 2, expect: 4                             examples
  ; given: 7, expect: 49
  (define (area-of-square len)                      function definition (body)
    (sqr len))
  ```

* Top-down approach to design says that we first design top-level functions, than lower-level functions,
  than more-lower-level primitives and so on. We can implement a top-level function in terms of lower-level
  functions that don't yet exist. We pretend that they exist and put them on our *wish-list* to implement them later.

* MVC makes it easier to develop the application. We can separete working on data (model)
  from working on its representation (view). When we decide how data changes,
   we don't have to think how its rendering on the screen changes.


* Designing World Programs.
  
  1. Introduce constants for those properties of the world that remain the same.
  2. Develop a data representation for all possible states of the world.
  3. Design a number of functions so that you can form a valid big-bang expression.
  4. You need a main function.

* Imagine you have a finite collection of elements of the same type.
  If you can assign only these elements to your data type, this data type is an enumeration.
  Here is an example:
 
  ```racket
  ; A TrafficLight is one of the following Strings:
  ; – "red"
  ; – "green"
  ; – "yellow"
  ; interpretation the three strings represent the three 
  ; possible states that a traffic light may assume 
  ```

  In a function template you create as many cond clauses as there are elements in the enumeration.

  ```racket
  ; WorldState KeyEvent -> ...
  (define (next-light tl)
    (cond
      [(string=? "red" tl) ...]
      [(string=? "green" tl) ...]
      [(string=? "blue" tl) ...]
      [else ...]))
  ```

* *An interval* is a description of a class of numbers via boundaries.
    
  ```racket
  ; A WorldState falls into one of three intervals: 
  ; – between 0 and CLOSE
  ; – between CLOSE and HEIGHT
  ; – below HEIGHT
  ```

  In a function template the number of cond clauses is equal to the number of intervals.

  ```racket
  ; WorldState -> WorldState
  (define (f y)
    (cond
      [(<= 0 y CLOSE) ...]
      [(<= CLOSE y HEIGHT) ...]
      [(>= y HEIGHT) ...]))
  ```

* An *itemizations* is a mix of all other data types.

  ```racket 
  ; A LRCD (for launching rocket count down) is one of:
  ; – "resting"
  ; – a Number between -3 and -1
  ; – a NonnegativeNumber 
  ; interpretation a grounded rocket, in count-down mode,
  ; a number denotes the number of pixels between the
  ; top of the canvas and the rocket (its height)
  ```

* Design of a function depends on a data type. For functional examples you should pick
  at least one example from each sub-class in the data definition. 

* When you need to combine several related pieces of data into one entity, use a structure.
    
  *A structure* data type is defined like this:

  ```racket
  (define-struct person [name email age])
  ```

  Other functions are created automatically.

  ```racket
  (make-person "Dave" "dave@gmail.com" 32)          ; constructor
  (person-name p) (person-email p) (person-age p)   ; selectors
  (person? p)                                       ; predicate
  ```

* Here is a proper structure type definition:
    
  ```racket
  (define-struct person [name email salary])
  ; An Person is a structure: 
  ;   (make-person String String Number)
  ; interpretation a persons's name, email and salary 

  (define (process-person p)
    (... (person-name p) ... (person-email p) ... (person-salary p) ...))
  ```

* One thing to keep in mind is that data definitions may refer to other data definitions.
  If a function deals with nested structures, develop one function per level of nesting. 

* It is better if your program has *a few pivotal constants* and everything else follow from them.
  For example, you define the size of a ball and the sizes of other objects are computed from it. 

* Make examples for data definitions like you do for functions. Examples help to understand.

* When we design an application, we make design choices. In future they may turn out to be bad design choices.
  We might realize that If we had chosen other data structures the program would be simpler.
  We can't learn how to make good choices, it only comes with practice.

* *Don't reinvent the wheel*. Your language probably comes with a rich library and you have to know it well.
  Functions from the library are well-written and well-tested.

