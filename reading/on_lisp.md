# [On Lisp][homepage], by Paul Graham, Prentice Hall (1993)

[errata][errata]

[homepage] : http://www.paulgraham.com/onlisp.html
[errata]: http://www.paulgraham.com/onlisperrata.html

> Lisp is a programmable programming language. -- John Foderaro

There is more to Lisp than this, but the ability to bend Lisp to one' will is a
large part of what distinguishes a Lisp expert from a novice. As well as
writing their programs down toward the language, experienced Lisp programmers
build the language up toward their programs.<br>
Instead of just writing your program in Lisp, you can write your own language
*on Lisp*, and write your program in that.<br>
Mastering macros is one of the most important steps in moving from writing
correct Lisp programs to writing beautiful ones.<br>
One of the purposes of this book is to explain what makes Lisp different from
other languages. *On Lisp* deals mostly with the kinds of programs you could
only write in Lisp. I also wanted to explain *why* Lisp is different.

## 1. The Extensible Language

In fact, the association between Lisp and AI is just an accident of history.
Lisp was invented by John McCarthy, who also invented the term "artificial
intelligence."<br>
In Lisp, you can do much of your planning as you write the program.<br>
In Lisp, you don't just write your program down toward the language, you also
build the language up toward your program. Language and program evolve
together. Like the border between two warring states, the boundary between
language and program is drawn and redrawn, ... In the end your program will
look as if the language had been designed for it.<br>
By making the language do more of the work, bottom-up design yields programs
which are smaller and more agile.<br>
Lisp was invented as a language for expressing algorithms. Fortran makes life
easy for the compiler wrier; Lisp makes life easy for the programmer. Fortran
and Lisp have themselves moved closer to the center.

## 2. Functions

It is therefore said that Common Lisp has distinct *name-spaces* for variables
and functions.<br>
One of the big difference between Common Lisp and the dialects which preceded
it are the large number of built-in functions that take functional
arguments.<br>
Common Lisp is a lexically scoped Lisp. Scheme is the oldest dialect with
lexical scope; before Scheme, dynamic scope was considered one of the defining
features of Lisp. In a lexically scoped Lisp, instead of looking back through
the chain of calling functions, we look back through the containing
environments at the time the function was *defined*. Though you can still get
dynamic scope by declaring a variable to be `special`, lexical scope is the
default in Common Lisp.<br>
Because Common Lisp is lexically scoped, when we define a function containing
free variables, the system must save copies of the bindings of those variables
at the time the function was defined. Such a combination of a function and a
set of variable bindings is called a *closure*. Closures are functions with
local state.<br>
However, there is an important difference between `let` and `labels`. In a
`let` expression, the value of one variable can't depend on another variable
made by the same `let` ...<br>
`(proclaim '(optimize speed))`<br>
Later chapters will depend on another effect of compilation: when one function
occurs within another function, and the containing function is compiled, the
inner function will also get compiled. CLTL2 does not seem to say explicitly
that this will happen, but in a decent implementation you can count on it. As
later chapters will show, this fact is of great importance in the
implementation of embedded languages. If a new language is implemented by
transformation, and the transformation code is compiled, then it yields
compiled output---and so becomes in effect a compiler for the new language.<br>
`(proclaim '(inline 50th))` The drawback is that if we redefine `50th`, we also
have to recompile `foo`, or it will still reflect the old definition. The
restrictions on inline functions are basically the same as those on macros.<br>
It cannot be overemphasized how important it is that Lisp programs can write
Lisp programs, especially since this fact is so often overlooked.

## 3. Functional Programming

*Functional programming* means writing programs which work by returning values
instead of by performing side-effects. Side-effects include destructive changes
to objects (e.g. by `rplaca`) and assignments to variables (e.g. by `setq`).
Lisp programs have not always been written in this style, but over time Lisp
and functional programming have gradually become inseparable.<br>
Suppose for some reason we want the elements of a list in the reverse order.
Instead of writing a function to reverse lists, we write a function which takes
a list, and returns a list with the same elements in the reverse order.<br>
One way to start is to treat the following operators as if there were a tax on
their use:
    `set` `setq` `setf` `psetf` `psetq` `incf` `decf` `push` `pop` `pushnew`
    `rplaca` `rplacd` `rotatef` `shiftf` `remf` `remprop` `remhash`
and also `let*`, in which imperative programs often lie concealed. Treating
these operators as taxable is only proposed as a help toward, not a criterion
for, good Lisp style. However, this alone can get you surprisingly far.<br>
A functional program tells you what it wants; an imperative program tells you
what to do.<br>
Instead of treating all side-effects as equally bad, it would be helpful if we
had some way of distinguishing between such cases. Informally, we could say
that it's harmless for a function to modify something that no one else owns.

## 4. Utility Functions

## 5. Returning Functions

## 6. Functions as Representation

## 7. Macros

## 8. When to Use Macros

## 9. Variable Capture

## 10. Other Macro Pitfalls

## 11. Classic Macros

## 12. Generalized Variables

## 13. Computation at Compile-Time

## 14. Anaphoric Macros

## 15. Macros Returning Functions

## 16. Macro-Defining Macros

## 17. Read-Macros

## 18. Destructuring

## 19. A Query Compiler

## 20. Continuations

## 21. Multiple Processes

## 22. Nondeterminism

## 23. Parsing with ATNs

## 24. Prolog

## 25. Object-Oriented Lisp

