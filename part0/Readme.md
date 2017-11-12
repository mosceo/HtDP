HtDP 2e, Preface & Prologue. Review.
====================================

* A good programmer designs programs. A bad programmer tinkers until the program
seems to work (garage programming).

* Coding is just a part of programming. Before we even begin writing code
we should do a lot of work to design the application. We should develop programs
in a *systematic* manner. Which means we have a strict plan
what to do and design comes before coding.

* Functions are the basic building blocks. The key is to discover which functions
are needed, how to connect them, and how to build them from basic ingredients.

* This book deals with just two recipes for complete programs: one for programs
with GUI and one for batch programs. In contrast, design recipes for functions come
in a wide variety of flavors (depends on the data consumed by the function).

* Getting everything right at once is nearly impossible. Instead, computer scientists
apply *iterative refinement*. In essence, iterative refinement recommends stripping away
all inessential details at first and finding a solution for the remaining core problem.
A refinement step adds in one of these omitted details and re-solves the expanded problem,
using the existing solution as much as possible.

* It is not enough to know a programming language to be a good programmer.
There is something more fundamental. If you know how to program well in one language,
you will program well in any other language.

* Being a good programmer is like being a good essay writer. Knowing English alone
does not make you a good essay writer. You have to know English to write essays,
but there is something else.

* Good design principles can be applied in any programming language.
We can apply them in other professions, not only in computer science.

* The book suggests top-down planning phase followed by a bottom-up construction phase.

* In the ideal program, a small request, such as changing the sizes of the canvas,
should require an equally small change.

* Every time you re-organize your program to prepare yourself for likely future
change requests, you refactor your program.

* Do you see repeated expressions in the code? Do you see similar blocks of logic?
Eliminate them all by creating new fnctions and constant definitions.

* Do you see a number in an expression? After some time you may have no idea what it stands for.
These numbers are called *magic numbers*. Get rid of them by creating constant definitions.
`(* 20 t)` becomes `(* SPEED t)`.

* In Star Wars master Yoda says a very deep thing: "You must unlearn what you have learned".
When we were learning programming, we were concentrating on the mechanics of the language
and picked up some bad practices. And now we have to "unlearn" them to learn good program design.
You see, a way from bad programmer to good programmer can be *longer* than from no programmer
to good programmer. Because we have to unlearn all of this.

