HtDP 2e, Part 3. Review.
========================

* Good programmers try to *eliminate similarities* as much as the programming language allows.
  Programmers write down their first drafts of programs, spot similarities (and other problems),
  and get rid of them.

* You must *abstract* to avoid similarities. Abstracting different versions of functions
  (functional abstraction) is one way to eliminate similarities from programs.
  That's how you get filter, map, reduce, etc.

* We make abstract functions and define specific functions in terms of the abstract ones.

* Functions can consume functions. Functions are values as well.
  That's the world of functional languages, baby.

* We also can abstract data definitions. They begin to have parameters.

  ```racket
  ; A [List-of ITEM] is one of: 
  ; – '() 
  ; – (cons ITEM [List-of ITEM])

  ; A [CP H V] is a structure: 
  ;   (make-point H V)

  ; A [List X Y] is a structure: 
  ;   (cons X (cons Y '()))
   
  ; A [Maybe X] is one of: 
  ; – #false 
  ; – X

  ; [Number -> Boolean] -> [List-of Number]
  ```

* Signatures can also be abstracted. Here is a signature with parameters:

  ```racket
  ; [X Y] [List-of X] -> [List-of Y]
  ; [X Y] [List-of X] Y -> [Maybe Y]
  ```

* A signature describes a class of functions. For example, the signature `[Number -> Boolean]`
  describes a class of all functions that consume a Number and return a Boolean.

* To abstract is to turn something concrete into a parameter.

* The single most important advantage of creating abstractions is the *single points of control*
  for some common functionality. You fix a mistake in one place, you edit code in one place.

* To find a matching abstraction means to pick an abstraction whose purpose
  is more general than the one for the function to be designed.

* A guideline: form an abstraction instead of copying and modifying any code.

* The use of an abstraction helps readers of your code to understand your intentions.

* The chosen programming language affects a programmer’s ability to express solutions,
  and a future reader’s ability to recognize the design insight of the original creator.

* Here is an example of a local definition.
  
  ```racket
  (local ((define v1 1)
          (define v2 2)
          ; Number Number -> Number
          (define (sum a b)
            (+ a b)))
         ; -IN-
    (sum v1 v2))
  ```

* Repeated code patterns call for abstraction. To abstract means to factor out the repeated pieces
  of code and to parameterize over the differences.

* All programming languages come with the means to build abstractions though some means are better than others.
  All programmers must get to know the means of abstractions and the abstractions that a language provides.

* Here is design recipe:
   
  1. Follow the design recipe for functions.
  2. Exploit the signature and purpose statement to find a matching abstraction (filter, map, foldr).
  3. Write down a template.
  4. Finally, it is time to define the auxiliary function inside local.
  5. The last step is to test the definition in the usual manner.

* Lambda functions come out:

  ```racket
  ((lambda (a b) (+ a b))
    1 2)

  (define sum (lambda (a b) (+ a b)))

  (map (lambda (x) (add x 1))
    '(1 2 3))
  ```

* Here is an interesting data definition:
  
  ```racket
  ; A Shape is a function: 
  ;   [Posn -> Boolean]
  ; interpretation if s is a shape and p a Posn, (s p) 
  ; produces #true if p is in of s, #false otherwise
  ```

* Reuse existing abstractions, avoid creating your own.

* Here are examples of for loops:

  ```racket
  (for/list ([i 4])
    i)                  ;= (list 1 2 3)

  (for/list ([i '(1 2 3)] [j '(10 20 30)])         ; two lists are traversed concurently
    (list i j))         ;= (list (list 1 10) (list 2 20) (list 3 30))

  (for*/list ([i '(1 2)] [j '(10 20)])             ; like nested loops
    (list i j))         ;= (list (list 1 10) (list 1 20) (list 2 10) (list 2 20))
  ```

  The meaning of these loops is clear from examples:

  ```racket
  ; check two lists for equality
  (for/and ([a '(1 2 3)] [b '(1 2 3)])
    (= a b))

  ; does the list contain a negative element?
  (for/or ([a '(1 -2 3)])
    (< a 0))

  (for/sum ([c "APPLE"])          ; traverses the string letter by letter
    (string->int c))
  ```

