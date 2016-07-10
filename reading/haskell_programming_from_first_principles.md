# [Haskell Programming from first principles][homepage], by Christopher Allen and Julie Moronuki

[homepage]: http://haskellbook.com/

## 1. Lambda Calculus

alpha equivalence and beta reduction<br>
A combinator is a lambda term with no free variables.<br>
the semantics of the core language are the same as the lambda calculus. That is, the meaning of Haskell programs is centered around evaluating expressions rather than executing instructions.<br>
Normal order isn't how Haskell code is evaluated -- it's call-by-need instead.

* [A Tutorial Introduction to the Lambda Calculus][tut_lambda_calculus] by Raul Rojas
* [Introduction to Lambda Calculus][intro_lambda_calculus] by Henk Barendregt and Erik Barendsen
* [Proofs and Types][proofs_and_types] by Jean-Yves Girard, P. Taylor and Yves Lafon

[tut_lambda_calculus]: http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf
[intro_lambda_calculus]: http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf
[proofs_and_types]: http://www.paultaylor.eu/stable/prot.pdf

## 2. Hello Haskell

* [How to desugar Haskell code][desugar_haskell]

[desugar_haskell]: http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

## 3. Strings

Global or top level bindings in Haskell mean bindings visible to all code within a module and, if made available, can be imported by other modules or programs. Global bindings in the sense that a variable is unconditionally visible throughout an entire program do not exist in Haskell.

## 4. Basic Datatypes

Num - Int, (Data.Int) Int8, Int16, Integer, Float, Double, Rational, (Data.Scientific) Scientific<br>
"Julie" == 8 -- error<br>
length :: [a] -> Int<br>
there is no singleton tuple, but there is a zero tuple also called unit or ().

## 5. Types

funcIgnoresArgs :: a -> a -> a -> String<br>
funcIgnoresArgs x y z = "Blah"<br>
funcIgnoresArgs undefined undefined :: a -> String<br>
curry f a b = f (a, b)<br>
uncurry f (a, b) = f a b<br>
Prelude> let f :: a -> a -> a -> a; f = undefined<br>
[How to make *ad-hoc* polymorphism less *ad hoc*][ad_hoc_polymorphism]<br>
if a variable could be anything, then there's little that can be done to it because it has no methods. If it is a concrete type, you lose the type flexibility but, due to the additive nature of typeclass inheritance, gain more potential methods.<br>
Haskell's [type inference][type_inference] is built on an extended version of the [Damas-Hindley-Milner type system][damas_hindley_milner_type_system].<br>
With respect to Haskell, the *principal type* is the most generic type which still typechecks.<br>
[@parametricity][twitter_parametricity]

* [Principal type-schemes for functional programs][principal_type_schemes]
* [Fundamental Concepts in Programming Languages][fundamental_concepts_in_programming_languages]

[ad_hoc_polymorphism]: http://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf
[type_inference]: https://en.wikipedia.org/wiki/Type_inference
[damas_hindley_milner_type_system]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
[twitter_parametricity]: https://twitter.com/parametricity
[principal_type_schemes]: http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf
[fundamental_concepts_in_programming_languages]: http://www.cs.cmu.edu/~crary/819-f09/Strachey67.pdf

## 6. Typeclasses

...

partial function vs total function

...

## 7. Functions

...

*Bottom* (⊥) is a non-value used to denote that the program cannot return a value or result. The two main varieties of bottom are computations that failed with an error or those that failed to terminate. (for example, `error`, failed pattern matches of partial functions, `undefined` and infinite loops)<br>
*Higher-order* functions are functions which themselves take functions as arguments or return functions as results.<br>
`enumFrom`<br>
*[Pointfree][pointfree]* refers to a style of composing functions without specifying their arguments. The "point" in "pointfree" refers to the arguments.

* [Case Expressions and Pattern Matching chapter of A Gentle Introduction to Haskell, Version 98][gentle_intro] by Paul Hudak, John Peterson and Joseph Fasel
* [The Implementation of Functional Programming Languages][implementation_fp_lang] by Simon Peyton Jones
* [An introduction to pointfree programming][pointfree_intro] by J.N. Oliveira
* [Point-free Program Calculation][pointfree_calc] by Manuel Alcino Pereira da Cunha

[pointfree]: https://wiki.haskell.org/Pointfree
[gentle_intro]: https://www.haskell.org/tutorial/patterns.html
[implementation_fp_lang]: http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm
[pointfree_intro]: http://www.di.uminho.pt/~jno/ps/iscalc_1.ps.gz
[pointfree_calc]: http://www4.di.uminho.pt/~mac/Publications/phd.pdf

## 8. Recursion

Being able to write recursive functions, though, is essential to Turing completeness. We use a combinator `.` known as the [Y combinator][y_combinator] or fixed-point combinator `.` to write recursive functions in the lambda calculus. Haskell has native recursion ability based on the same principle as the Y combinator.

[y_combinator]: http://mvanier.livejournal.com/2897.html

## 9. Lists

Ranges use `enumFromTo` and `enumFromThenTo` of `Enum` type class.<br>
`:sprint` - see what has been evaluated, caution: opportunistic optimizations and polymorphism<br>
NF (normal form) implies WHNF (weak head normal form). An expression cannot be in normal form or weak head normal form if the outermost part of the expression isn't a data constructor. It can't be in normal form if any part of the expression is unevaluated.<br>
`length` is strict in the spine but not the values.<br>
A common mantra for performance sensitive code in Haskell is, "lazy in the spine, strict in the leaves."

* [Data.List][data_list]
* [Ninety-nine Haskell problems][ninety_nine_problems]

`bool` function in Data.Bool module?

[data_list]: http://hackage.haskell.org/package/base/docs/Data-List.html 
[ninety_nine_problems]: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems 

## 10. Folds

Catamorphisms ("cata-" means "down" or "against") are a means of deconstructing data.<br>
`foldr f acc (x:xs) = f **x** (foldr f acc xs)` can be used with infinite lists. Everything is find unless the first piece (cons cell) of the spine is bottom.<br>
`[1, 2, 3, 4, undefined] -- undefined value`<br>
`[1, 2, 3, 4] ++ undefined -- undefined spine`<br>
`length $ take 2 $ take 4 ([1, 2]++undefined) -- no error`<br>

...

## 11. Algebraic datatypes

## 12. Signaling Adversity

## 13. Building projects in Haskell

## 14. Testing

## 15. Monoid, Semigroup

## 16. Functors

## 17. Applicative

## 18. Monad (SPOOKY? No.)

## 19. Abstract structure applied

## 20. Foldable

## 21. Traversable

## 22. Reader

## 23. State

## 24. Parsers

## 25. Composing Types

## 26. Monad Transformers

## 27. Non-strictness

## 28. Commonly used data structures

## 29. Demystifying IO

## 30. Exceptions

## 31. Final Project

