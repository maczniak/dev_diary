# [Compiling with Continuations][homepage] by Andrew W. Appel, Cambridge University Press (1992)

[homepage]: http://www.cambridge.org/us/academic/subjects/computer-science/programming-languages-and-applied-logic/compiling-continuations

## 1. Overview

The intermediate representation that SML/NJ uses for optimization and code
 generation--*continuation-passing style*--is applicable to the compilation of
 many modern programming languages, not just ML.<br>
Continuation-passing style (CPS) is a program notation that makes every aspect
 of control flow and data flow explicit. It also has the advantage that it's
 closely related to Church’s λ-calculus, which has a well-defined and
 well-understood meaning.<br>
A continuation is a function that expresses "what to do next;" for example, we
 can say that `prodprimes` is given a continuation *c* as one of its arguments,
 and when `prodprimes` has computed its result *a* it will continue by applying
 *c* to *a*. Thus, returning from a function looks just like a function call!

optimizations | CPS | λ-calculus<br>(without<br>explicit<br>continuations) | QUADruples<br>(register<br>transfers) | Program-<br>Dependence<br>Graphs | Static<br>Single-<br>Assignment<br>form
------------------------|----|----|----|----|---
in-line expansion       | ++ |    |    |    |
closure representations | ++ | ++ |    |    |
dataflow analysis       | +  |    | +  | +  | ++
register allocation     | ++ |    | ++ | +  | ++
vectorizing             | +  |    | +  | ++ | +
instruction scheduling  | +  |    | +  | +  | +

The intermediate representations described here have many similarities. Static
 single-assignment form is just a restricted form of quadruples.
 Continuation-passing style is a restricted form of λ-calculus. And in fact,
 there are many similarities between SSA and CPS, since CPS variables have a
 single-binding property. With continuations, we get both the clean substitution
 operations of λ-calculus and the dataflow and register analyses appropriate for
 von Neumann machines.

## 2. Continuation-passing style

An important property of well-formed CPS expressions is that the arguments to a
 function (or a primitive operator such as +) are always atomic (variables or
 constants); a function application can never be an argument of another
 application. This is because the CPS language is meant to model the program
 executed by a von Neumann machine, which likes to do just one thing at a time,
 with all the argumets to an operation ready-at-hand in registers.

```ml
datatype cexp =
        RECORD of (value * accesspath) list * var * cexp
      | SELECT of int * value * var * cexp
      | OFFSET of int * value * var * cexp
      | APP of value * value list
      | FIX of (var * var list * cexp) list * cexp
      | SWITCH of value * cexp list
      | PRIMOP of primop * value list * var list * cexp list
```

All function calls in continuation-passing style are "tail" calls; that is, they
 do not return to the calling function.<br>
In a continuation-passing-style compiler, all function calls must be translated
 into such "tail" calls. This is accomplished by introducing *continuation
 functions*: A continuation expresses the "rest" of the computation after the
 called function was to have returned.

```
let fun f(x) = 2*x+1
 in f(a+b)*f(c+d)
end

let fun f(x,k) = k(2*x+1)
    fun k1(i) = let fun k2(j) = r(i*j)
                 in f(c+d, k2)
                end
 in f(a+b, k1)
end
```

The function *f* is used only locally, but the function *g* is both used locally
 and exported to the outside; we say that *g escapes* but *f* does not. Since
 *g* might be hled in a data structure and extracted at arbitary points in the
 program; or worse, might be exported and used in some other "compilation unit"
 about which we know nothing at all, its representation (as a function that
 takes a triple, not a pair) must not be changed. Since the straightforward
 conversion of ML into CPS establishes these invariants (rules about escaping
 functions), we might want the optimizer to make use of them in reasoning about
 the behavior of escaping functions.<br>
The reason to use CPS as a compiler intermediate representations is that it is
 quite close to the instruction set of a von Neumann computer.<br>
Now, a "von Neumann" machine represents a function just by an address in the
 machine-code. Such an address cannot describe the current values of the free
 variables of the function. The usual way to represent functions with free
 variables is by a *closure*: a pair of *machine-code pointer* and
 *free-variable information*.<br>
The translation of a program in CPS, into another CPS program in which none of
 the functions have free variables, is called "closure conversion." Every
 function will be given an extra argument, which will be a closure record. The
 first field of the closure record will be a pointer to the function itself;
 the other fields will be the values of free variables of the function.<br>
The primary reason for using `LABEL` is to make the computation of
 free-variables sets for register allocation easier; from the code generator's
 point of view, a `VAR` occupies a register and a `LABEL` does not.<br>
The use of variables in the CPS language resembles in many ways the use of
 registers on a von Neumann machine. However, von Neumann machines have only a
 fixed number of registers, and CPS expressions can have an arbitary number of
 variables. To correct this mismatch, we will map many CPS variables onto the
 same register. After the closure phase, the *spill* phase rewrites the CPS to
 satisfy this rule.

## 3. Semantics of the CPS

## 4. ML-specific optimizations

## 5. Conversion into CPS

## 6. Optimization of the CPS

## 7. Beta expansion

## 8. Hoisting

## 9. Common subexpressions

## 10. Closure conversion

## 11. Register spilling

## 12. Space complexity

## 13. The abstract machine

## 14. Machine-code generation

## 15. Performance evaluation

## 16. The runtime system

## 17. Parallel programming

## 18. Future directions

## A. Introduction to ML

## B. Semantics of the CPS

## C. Obtaining Standard ML of New Jersey

## D. Readings

