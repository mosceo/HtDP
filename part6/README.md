HtDP 2e, Part 6. Review.
========================

* If a function needs additional "knowledge" we introduce an additional argument called an *accumulator*.
  An accumulator provides some context to the call. For example, when a function that traverses a graph
  is called on a node, it would like to "know" which nodes have already been visited.

* Accumlators may improve performance. Sometimes you will not be able to solve a problem without them.

* The goal of a programmer is to make sure that others who follow understand the code easily.

* Here is a recipe for designing functions with accumulators:
  1. Create an accumulator template:

     ```racket
     ; Domain -> Range 
     (define (function d0)
       (local (; Domain AccuDomain -> Range
               ; accumulator ...
               (define (function/a d a)
                 ...))
         (function/a d0 a0)))
     ```
  2. Determine the kind of data that the accumulator tracks.
  3. Determine the initial value a0.
  4. Determine how to compute the accumulator for the recursive function calls within the definition of function/a.

* The first step is to recognize the need for introducing an accumulator and
  determine what knowledge the accumulator represents.

