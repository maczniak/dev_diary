# [Haskell Programming from first principles][homepage] by [Christopher Allen][christopher_allen] and Julie Moronuki

[various answers][answers]

[homepage]: http://haskellbook.com/
[christopher_allen]: http://bitemyapp.com/
[answers]: https://github.com/larrybotha/haskell-book

We use tools like Haskell because they help us.<br>
Haskell is a language in a progression of languages dating back to 1973, when ML
 was invented by Robin Milner and others at the University of Edinburgh. ML was
 itself influenced by ISWIM, which was in turn influenced by ALGOL 60 and Lisp.
 In 1968, the ALGOL68 dialect had the following features built into the
 language:
1. User-defined record types.
1. User-defined sum types (unions not limited to simple enumerations).
1. Switch/case expressions supporting the sum types
1. Compile-time enforced constant values, declared with `=` rather than `:=`.
1. Unified syntax for using value and reference types - no manual pointer
   dereferencing.
1. Closures with lexical scoping (without this, many functional patterns fall
   apart).
1. Implementation-agnostic parallelized execution of procedures.
1. Multi-pass compilation - you can declare stuff after you use it.

Sometimes we hear Haskell being dismissed as "academic" because it is relatively
 up-to-date with the current state of mathematics and computer science
 research.<br>
The REPL we'll be using is called GHCi---"i" for "interactive."

## 1. Lambda Calculus

The lambda calculus has three basic components, or *lambda terms*: expressions,
 variables, and abstractions. The word *expression* refers to a superset of all
 those things: an expression can be a variable name, an abstraction, or a
 combination of those things. Variables here has no meaning or value; they are
 only names for potential inputs to functions.<br>
An *abstraction* is a *function*.. It is a lambda term that has a head (a
 lambda) and a body and is applied to an argument. An *argument* is an input
 value. The head of the function is a λ followed by a variable name. The body of
 the function is another expression. The variable named in the head is the
 *parameter* and *binds* all instances of that same variable in the body of the
 function. The dot (.) separates the parameters of the lambda from the function
 body. The act of "applying a lambda function to an argument", (λ...), is called
 *application* and application is the lynchpin of the lambda calculus.<br>
A named function can be called by name by another function; an anonymous
 function cannot.<br>
alpha equivalence and beta reduction<br>
Application in the lambda calculus are *left associative*.<br>
*free variables* are variables in the body that are not bound by the head. Note
 that alpha equivalence does not apply to free variables.<br>
Functions that require multiple arguments have multiple, nested heads. This
 formulation was originally discovered by Moses Schönfinkel in the 1920s but was
 later rediscovered and named after Haskell Curry and is commonly called
 *currying*.<br>
The lambda calculus is a process or method, like a game with a few simple rules
 for transforming lambdas, but no specific meaning.<br>
These are multiple normal forms in lambda calculus, but here when we refer to
 normal form we mean *beta normal form*. Beta normal form is when you cannot
 beta reduce the trerms any further. This corresponds to a fully evaluated
 expression, or, in programming, a fully executed program. Don't be intimidated
 by calling the reduced form of an expression its normal form.<br>
A combinator is a lambda term with no free variables. The point is to call out a
 special class of lambda expressions that can *only* combine the arguments it is
 giving, without injecting any new values or random data.<br>
Not all reducible lambda terms reduce to a normal form because they *diverge*.
 Reducing terms should ordinarily *converge* to beta normal form. Here's an
 example of a lambda term called *omega* that diverges: (λ*x.xx*)(λ*x.xx*) This
 matters in programming because terms that diverge are terms that don't produce
 an *answer* or meaningful result.<br>
Haskell is a *typed* lambda calculus with a lot of surface-level decoration
 sprinkled on top, to make it easier for humans to write, but the semantics of
 the core language are the same as the lambda calculus. That is, the meaning of
 Haskell programs is centered around evaluating expressions rather than
 executing instructions, although Haskell has a way to execute instructions,
 too.<br>
*Normal order* is a common evaluation strategy in lambda calculi. Normal order
 means evaluating the leftmost outermost lambdas first. Normal order isn't how
 Haskell code is evaluated -- it's call-by-need instead.

1. [A Tutorial Introduction to the Lambda Calculus][tut_lambda_calculus] by Raul
   Rojas
1. [Introduction to Lambda Calculus][intro_lambda_calculus] by Henk Barendregt
   and Erik Barendsen
1. [Proofs and Types][proofs_and_types] by Jean-Yves Girard, P. Taylor and Yves
   Lafon

[tut_lambda_calculus]: http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf
[intro_lambda_calculus]: http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf
[proofs_and_types]: http://www.paultaylor.eu/stable/prot.pdf

## 2. Hello Haskell

[How to learn Haskell][how_to_learn_haskell]<br>
`stack ghci`

```yaml
# stack.yaml
resolver: lts-12.10
packages: []
```

To return to the `Prelude>` prompt, use the command `:m`, which is short for
 `:module`. This will unload the file from GHCi, so the code in that file will
 no longer be in scope in your REPL. `:load` will have unloaded the first one
 before loading the new one. As we build larger projects that require having
 multiple modules in scope, we will use a project manager called Stack rather
 than GHCi itself.<br>
Regarding *declarations*, it suffices to say for now that they are top-level
 bindings which allows us to name expressions.<br>
Expressions are in *normal form* when they've reached an irreducible form.
 Reducible expressions are also called *redexes*.<br>
This chaned as of the release of GHC 8.0.1: using `let` in declarations in GHCi
 is no longer necessary.<br>
`:r` (recompile and load)<br>
Values are expressions, but cannot be reduced further.<br>
Haskell only evaluates to weak head normal form (WHNF) by default. What this
 means is that not everything will get reduced to its irreducible form
 immediately.<br>
Operators are functions which can be used in infix style.<br>
Use spaces, *not* tabs, to indent your source code. The basic rule is that code
 that is part of an expression should be indented under the beginning of that
 expression. Furthermore, parts of the expression that are grouped should be
 indented to the same level.<br>
(quot x y)*y + (rem x y) == x, (div x y)*y + (mod x y) == x, the results of
 `mod` will have the same sign as the divisor, while the result of `rem` will
 have the same sign as the dividend.<br>
In the specific case of `-`, the syntactic sugar means the operator now have two
 possible interpretations: an alias for `negate` or the subtraction function.
 Fortunately, syntactic overloading like this isn't common in Haskell. Instead
 of `(- x)`, you can write `(subtract x)`.<br>
In the case of `($)` you might see some cruft like:
 `forall (r :: GHC.Type.RuntimeRep) a (b :: TYPE r).`<br>
If you use sectioning with a function that is not commutative, the order
 matters.<br>

* [How to desugar Haskell code][desugar_haskell]

[how_to_learn_haskell]: https://github.com/bitemyapp/learnhaskell
[desugar_haskell]: http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

## 3. Strings

`:set prompt "λ> "` You can set it permanently if you prefer by setting the
 configuration in your ~/.ghci file.<br>
Global or top level bindings in Haskell mean bindings visible to all code within
 a module and, if made available, can be imported by other modules or programs.
 Global bindings in the sense that a variable is unconditionally visible
 throughout an entire program do not exist in Haskell.

## 4. Basic Datatypes

`Num` - `Int` ((Data.Int, GHC.Int) `Int8`, `Int16`, `Int32`, `Int64`),
 `Integer`, `Word` (fixed-precision non-negative integer), `Fractional`
 (`Float`, `Double`, `Rational`, `Fixed E2`, (Data.Scientific) `Scientific`)<br>
You almost never want a `Float` unless you're doing graphics programming such as
 with OpenGL.<br>
`(/) :: Fractional a => a -> a -> a`<br>
Values of `Fractional a => a` default to the floating point type `Double`.<br>
"Julie" == 8 -- error<br>
length :: [a] -> Int<br>
there is no singleton tuple, but there is a zero tuple also called unit or
 `()`.<br>
In Haskell, type classes are "unique" pairings of class and concrete instance.

## 5. Types

Developments in logic, mathematics, and computer science led to the discovery of
 a typed lambda calculus called *System F* in the 1970s. Haskell has improved on
 System F in some key ways, such as by allowing general recursion and the
 Hindley-Milner system to permit type inference.<br>
The `(->)` type constructor takes arguments and has no data constructures:
 `data (->) a b`<br>
The function type has no data constructors.<br>
The arrow associates to the right (although function *application* is left
 associative).<br>
funcIgnoresArgs :: a -> a -> a -> String<br>
funcIgnoresArgs x y z = "Blah"<br>
funcIgnoresArgs undefined undefined :: a -> String<br>
curry f a b = f (a, b)<br>
uncurry f (a, b) = f a b<br>
Currying and uncurrying functions of three or more arguments automatically is
 quite possible, but trickier.<br>
`Prelude> f :: a -> a -> a -> a; f = undefined`<br>
[How to make *ad-hoc* (or constrained) polymorphism less *ad hoc*][ad_hoc_polymorphism]<br>
if a variable could be anything, then there's little that can be done to it
 because it has no methods. If it is a concrete type, you lose the type
 flexibility but, due to the additive nature of typeclass inheritance, gain more
 potential methods.<br>
Parametricity is the property we get from having parametric polymorphism.
 *Parametricity* means that the behavior of a function with respect to the types
 of its arguments is uniform.<br>
`6 / fromIntegral (length [1, 2, 3])` (?),
 `fromIntegral :: (Num b, Integral a) => a -> b`<br>
Haskell's [type inference][type_inference] is built on an extended version of
 the [Damas-Hindley-Milner type system][damas_hindley_milner_type_system].<br>
The *monomorphism restriction* means that top-level declarations by default will
 have a concrete type if any can be determined. You can fix this by
 `{-# LANGUAGE NoMonomorphismRestriction #-}`.<br>
With respect to Haskell, the *principal type* is the most generic type which
 still typechecks.<br>
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

As Philip Wadler put it, "The goal is to define a datatype by cases, where one
 can add new cases to the datatype and new functions over the datatype, without
 recompiling existing code, and while retaining static type safety (e.g., no
 casts)." [The Expression Problem][expression_problem] / *ad hoc* because type
 class code is dispatched by type.<br>
Don't use `Read`. No seriously, don't.<br>
All members of `Ord` must be members of `Eq`, and all members of `Enum` must be
 members of `Ord`.<br>
`:set -Wall`<br>
In some cases, particularly when you're working in the GHCi REPL, you will not
 have specified a concrete type for a polymorphic value. In those situations,
 the type class will default to a concrete type, and the default types are
 already set in the libraries. The use of polymorphic values without the ability
 to infer a specific type and no default rule will cause GHC to complain about
 an ambiguous type. [Haskell Report 2010][haskell_report_2010]<br>
What sets Haskell apart from most other functional programming languages is that
 it introduced and refined a means of writing ordinary programs that talk to the
 outside world without adding anything to the pure lambda calculus it is founded
 on.<br>
When you have a value of type `IO String` it's more of a *means of producing* a
 `String`, which may require performing side effects along the way before you
 get your `String` value.<br>
partial function vs total function<br>
Don't use type classes to define default values. Seriously.<br>
`Num` doesn't imply `Ord`.<br>
One of the nice things about parametricity and type classes is that you are
 being explicit about what you mean to do *with* your data which means you are
 less likely to make a mistake. This isn't a panacea, but it can be worth
 avoiding concrete types for these (and other) reasons sometimes.<br>
*Effects* are how we refer to *observable* actions programs many take other than
 compute a value.<br>
[Type Classes in Haskell][type_classes_in_haskell]

[expression_problem]: http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt
[haskell_report_2010]: https://www.haskell.org/onlinereport/haskell2010/
[type_classes_in_haskell]: http://ropas.snu.ac.kr/lib/dock/HaHaJoWa1996.pdf

## 7. Functions

A value that can be used as an argument to a function is a *first-class*
 value. *Higher-order functions* (HOFs) are functions that accept functions as
 arguments or return functions as results.<br>
Pattern matching proceeds from left to right and outside to inside.<br>
GHCi's `:{` and `:}` block syntax<br>
Incomplete pattern matches applied to data they don't handle will return
 *bottom*, a non-value used to denote that the program cannot return a value or
 result. This will throw an exception, which if unhandled, will make your
 program fail.<br>
`newtype` is different in that in permits only one constructor and only one
 field.<br>
Fun fact: `returnBroke :: (((a -> b) -> c) -> d) -> d` is an impossible
 function.<br>
*Bottom* (⊥) is a non-value used to denote that the program cannot return a
 value or result. The two main varieties of bottom are computations that failed
 with an error or those that failed to terminate. (for example, `error`, failed
 pattern matches of partial functions, `undefined` and infinite loops)<br>
*[Pointfree][pointfree]* refers to a style of composing functions without
 specifying their arguments. The "point" in "pointfree" refers to the arguments.

* [Case Expressions and Pattern Matching chapter of A Gentle Introduction to Haskell, Version 98][gentle_intro]
  by Paul Hudak, John Peterson and Joseph Fasel
* [The Implementation of Functional Programming Languages][implementation_fp_lang]
  by Simon Peyton Jones
* [An introduction to pointfree programming][pointfree_intro] by J.N. Oliveira
* [Point-free Program Calculation][pointfree_calc] by Manuel Alcino Pereira da
  Cunha

[pointfree]: https://wiki.haskell.org/Pointfree
[gentle_intro]: https://www.haskell.org/tutorial/patterns.html
[implementation_fp_lang]: http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm
[pointfree_intro]: http://www.di.uminho.pt/~jno/ps/iscalc_1.ps.gz
[pointfree_calc]: http://www4.di.uminho.pt/~mac/Publications/phd.pdf

## 8. Recursion

The lambda calculus does not appear on the surface to have any means of
 recursion, because of the anonymity of expressions. Being able to write
 recursive functions, though, is essential to Turing completeness. We use a
 combinator --- known as the [Y combinator][y_combinator] or fixed-point
 combinator --- to write recursive functions in the lambda calculus. Haskell has
 native recursion ability based on the same principle as the Y combinator.<br>
In logic, ⊥ corresponds to *false*.<br>
`let x = x in x` was never going to return and short-circuited the never-ending
 computation.

[y_combinator]: http://mvanier.livejournal.com/2897.html

## 9. Lists

Lists do double duty in Haskell. The first purpose lists serve is as a way to
 refer to and process a collection or plurality of values. The second is as an
 infinite series of values, usually generated by a function, which allows them
 to act as a stream datatype.<br>
Ranges use `enumFromTo` and `enumFromThenTo` of `Enum` type class.<br>
We can also write list comprehensions that have multiple generators. One thing
 to note is that the rightmost generator will be exhausted first, then the
 second rightmost, and so on.<br>
Because of this and the way nonstrict evaluation woeks, you can evaluate cons
 cells independently of what they contain. It is possible to evaluate only (part
 of) the spine of the list without evaluate individual values (and the rest of
 spine).<br>
`:sprint` - see what has been evaluated, caution: opportunistic optimizations
 and polymorphism (like `Num a => a`)<br>
Weak head normal form (WHNF) is a larger set and contains both the possibility
 that the expression is fully evaluated (normal form) and the possibility that
 the expression has been evaluated to the point of arriving at a data
 constructor or lambda awaiting an argument. For an expression in weak head
 normal form, further evaluation may be possible once another argument is
 provided.<br>
NF (normal form) implies WHNF (weak head normal form). An expression cannot be
 in normal form or weak head normal form if the outermost part of the expression
 isn't a data constructor. It can't be in normal form if any part of the
 expression is unevaluated.<br>
`length` is strict in the spine but not the values.<br>
A common mantra for performance sensitive code in Haskell is, "lazy in the
 spine, strict in the leaves."<br>
`bool` function in Data.Bool module

* [Data.List][data_list]
* [Ninety-nine Haskell problems][ninety_nine_problems]

[data_list]: http://hackage.haskell.org/package/base/docs/Data-List.html 
[ninety_nine_problems]: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems 

## 10. Folds

Catamorphisms ("cata-" means "down" or "against") are a means of deconstructing
 data (breaking down structure). A catamorphism for a non-collection datatype,
 `data Maybe a = Nothing | Just a`, `maybe :: b -> (a -> b) -> Maybe a -> b`<br>
`foldr :: (a -> b -> b) -> b -> [a] -> b`<br>
`foldr f acc (x:xs) = f **x** (foldr f acc xs)` can be used with infinite lists.
 Everything is fine unless the first piece (cons cell) of the spine is
 bottom.<br>
`[1, 2, 3, 4, undefined] -- undefined value`<br>
`[1, 2, 3, 4] ++ undefined -- undefined spine`<br>
`length $ take 2 $ take 4 ([1, 2]++undefined) -- no error`<br>
`foldl :: (b -> a -> b) -> b -> [a] -> b`<br>
`foldl f acc (x::xs) = foldl f (f acc x) xs -- left associativity` must evaluate
 its whole spine before it starts evaluating values in each cell. Use `foldl'`
 almost always (for long lists).<br>
The next recursion of the spine isn't intermediated by the folding function as
 it is in `foldr`, which also means recursion of the spine is unconditional.
 This feature means that `foldl` is generally inappropriate with lists that are
 or could be infinite, but the combination of the forced spine evaluation with
 nonstrictness means that it is also usually inappropriate even for long lists,
 as the forced evaluation its whole spine before it starts evaluating values in
 each cell, it accumulates a pile of unevaluated values as it traverses the
 spine.<br>
Evaluating *the rest of the fold* evaluates the next application of `foldr`.
 `foldl` self-calls (tail-call) through the list, only the beginning to produce
 values after reaching the end of the list.<br>
`scanr (+) 0 [1..5] -- [15,14,12,9,5,0]`<br>
`scanl (+) 0 [1..5] -- [0,1,3,6,10,15]`<br>
`foldr f z xs = foldl (flip f) z (reverse xs) -- only for finite lists` or
 `reverse $ foldl (flip f) z xs`<br>
[Twitter waterflow problem and loeb][waterflow_problem] for a particularly
 elegant use of scans<br>
`fibs = 1 : scanl (+) 1 fibs`<br>
`map' f = foldr (\x xs -> f x : xs) []`, `foldr ((:) . f) []`<br>
Tail recursion is a function whose *tail calls* (the final result of a function)
 are recursive invocations of itself (not `foldr` but `foldl`).

* [Fold, Haskell Wiki][fold_wiki]
* [Introduction to Haskell][intro_haskell] by Antoni Diller
* [A tutorial on the universality and expressiveness of fold][tut_uni_exp_fold]

[waterflow_problem]: http://chrisdone.com/posts/twitter-problem-loeb
[fold_wiki]: https://wiki.haskell.org/Fold
[intro_haskell]: http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html
[tut_uni_exp_fold]: http://www.cs.nott.ac.uk/~gmh/fold.pdf

## 11. Algebraic datatypes

Haskell oofers sum types, product types, product types with record syntax, type
 aliases, and a special datatype called a *newtype* that provides for a
 different set of options and constraints from either type synonyms or data
 declarations.<br>
The Haskell Report calls these *type constants* to distinguish them from type
 constructors that take arguments.<br>
[Typed type-level programming in Haskell, part I: functional dependencies][type_level_prog]<br>
:kind, :k<br>
not all type arguments to constructors have value-level witnesses! Some are
 *phantom*.<br>
In standard Haskell, we can't choose specific values of types as type
 arguments.<br>
arity - nullary, unary, products (data constructors that take more than one
 argument), tuple (anonymous products)<br>
Algebraic datatypes in Haskell are algebraic because we can describe the
 patterns of argument structures using two basic operations: sum and product.
 (algebraic rules such as the distributive property) Type theory was developed
 as an alternative mathematical foundation to set theory.<br>
A `newtype` cannot be a product type, sum type, or contain nullary constructors.
 One advantage is that it has no runtime overhead, as it reuses the
 representation of the type it contains. The difference between newtype and the
 type it contains is gone by the time the compiler generates the code. The
 newtype declaration will allow you to define a custom typeclass instance. We
 can't do this if it's just a type synonym (`type`).
 `{-# LANGUAGE **X**GeneralizedNewtypeDeriving #-}` pragma will tell the
 compiler to allow our newtype
 (`newtype Goats = Goats Int deriving (Eq, Show, TooMany)`) to rely on a
 typeclass instance for the type it contains.<br>
[`FlexibleInstances`][flexibleinstances] for the type class for the type `(Int, String)`<br>
(not now) `:set -XNegativeLiterals` extension prevents an warning when using not
 `(negate 128)` but `(-128)`. `(-128)` desugars into `(negate 128)`.<br>
Type aliases create type constructors, not data constructors.
 `type TowBs = (Bool, Bool)`<br>
record -
 `data Person = Person { name :: String, age :: Int } deriving (Eq, Show)`,
 `Person "Papu" 5`<br>
normal form - sum of products, like
 `data Expr = Number Int | Add Expr Expr | Minus Expr`<br>
A stricter interpretation of normal form or "sum of products" would require
 representing products with tuples and sums with `Either`. This representation
 finds applications in problems where one is writing functions or *folds* over
 the representations of datatypes, such as with generics and
 metaprogramming.<br>
The nullary data constructor is equivalent to the () unit type.<br>
You can construct values of products that use record syntax in a manner
 identical to that of non-record products (or
 `myRecord = RecordProduct { pfirst = 42, , psecond = 0.00001 }`). Records are
 just syntax to create field references.<br>
`nub` (means essence) of Data.List keeps the first occurrence of each element
 and removes duplicate elements (O(n^2)).<br>
Partial applications of the record data constructor make exceptions. And keep
 sum types apart to prevent accidental bottoms (via record accessors).<br>
cardinality of function a -> b = b^a,
 a -> b -> c = (c ^ b) ^ a = c ^ (b * a)<br>
There are only a few kinds and you'll most often see `*`. Only types have
 inhabitants at the term level. Lists, for example, are *higher-kinded
 (data)types* (vs higher-kinded polymorphism) in Haskell.<br>
`data Silly a b c d = MkSilly a b c d deriving Show` is the same kind as
 `(,,,)`.<br>
[bloodhound][bloudhound] ElasticSearch package by the auther<br>
In Haskell, we do not conventionally put constraints on datatypes. The type
 class will likely constrain the variable in the type signature(s) for the
 function(s) that will process this data.

```haskell
instance (FromJSON a) => FromJSON (EsResultFound a) where -- ?
  parseJSON (Object v) = EsResultFound <$>
                         v .: "_version" <*>
                         v .: "_source"
  parseJSON _          = empty
```

All infix data constructors must start with a colon. Exceptions: (->), type
 constructor of functions and :: that is reserved for type (and kind)
 assertions.<br>
type constructors cannot exist at runtime (static type).<br>
`t@(a, _)` as-patterns<br>
[1HaskellADay][one_haskell_a_day]<br>
[Graham Hutton: Publications][graham_hutton_publications]

[type_level_prog]: https://byorgey.wordpress.com/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies/
[flexibleinstances]:  https://prime.haskell.org/wiki/FlexibleInstances
[bloodhoud]: http://hackage.haskell.org/package/bloodhound
[one_haskell_a_day]: https://twitter.com/1haskelladay
[graham_hutton_publications]: http://www.cs.nott.ac.uk/~pszgmh/bib.html#semantics

## 12. Signaling Adversity

Case expressions and pattern matching will work without an Eq instance, but
 guards using (==) will not.

```haskell
mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
```

A lifted type (kind `*`), which includes any datatype you could define yourself,
 is any that can be inhabited by bottom. Lifted types are represented by a
 pointer and include most of the datatypes we've seen and most that you're
 likely to encounter and use. / Unlifted types (kind `#`) are any type which
 cannot be inhabited by bottom. Types of kind `#` are often native machine types
 and raw pointers. / Newtypes are a special case in that they are kind `*`, but
 are unlifted because their representation is identical to that of the type they
 contain, so the newtype itself is not creating any new pointer beyond that of
 the type it contains.<br>
:k [] Int == [Int]

```haskell
catMaybes              :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([],[])
 where
  left  a ~(l, r) = (a:l, r)
  right a ~(l, r) = (l, a:r)

length :: t a -> Int
length = foldl' (\c _ -> c+1) 0
```

unfolds (anamorphisms, dual of catamorphisms) let us build data structures
 up.<br>
`unfoldr :: (b -> Maybe (a, b)) -> b -> [a]`

## 13. Building projects in Haskell

Haskell Cabal (Common Architecture for Building Applications and Libraries)
 package manager<br>
Stack helps you manage both projects made up of multiple packages as well as
 individual packages, whereas Cabal exists primarily to describe a single
 package with a Cabal file that has the `.cabal` file extension. Stack is built
 on top of Cabal in some important senses, so we will still be working with
 `.cabal` files. However, Stack simplifies the process somewhat, especially in
 large projects with multiple dependencies, by allowing you to build those large
 libraries only once and use them across projects. Stack also relies on an LTS
 snapshot of Haskell packages from [Stackage][stackage] that are guaranteed to
 work together.<br>
sample project - https://github.com/haskellbook/hello<br>
hello.cabal, stack.yaml (determine the versions of your packages and what
 version of GHC they'll work best with)<br>
stack setup, stack build, stack ghci, stack exec -- hello
 (`Linking .stack-work/dist/{...noise...}/hello`, executable stanza in
 `hello.cabal`), stack new hangman simple<br>
library stanza uses exposed-modules foo, bar instead of main-is Main.<br>
test-suite stanza (in the next chapter)<br>
module Hello where ..., module Hello ( sayHello ) where ...<br>
The ordering of import declarations is irrelevant.<br>
stack ghci --ghci-options -XNoImplicitPrelude<br>
import Data.Bool, import Data.Bool (bool), import qualified Data.Bool,
 import qualified Data.Bool as B<br>
import System.IO, hSetBuffering stdout NoBuffering<br>
Foldable instance - [], Maybe, Either<br>
If you evaluate `exitSuccess` (from `System.Exit`) in the REPL, it'll report
 that an exception occurred. In a normal running program that doesn't catch the
 exception, it'll end your whole program.

* [Stack][stack] - [Commercial Haskell][commercialhaskell], [FP Complete][fpcomplete]
* [How I Start: Haskell][how_i_start_haskell]
* [Cabal FAQ][cabal_faq]
* [Cabal User's Guide][cabal_user_guide]

[stackage]: https://www.stackage.org/
[stack]: https://github.com/commercialhaskell/stack
[commercialhaskell]: http://commercialhaskell.com/
[fpcomplete]: https://www.fpcomplete.com
[how_i_start_haskell]: http://bitemyapp.com/posts/2014-11-18-how-i-start-haskell.html
[cabal_faq]: https://www.haskell.org/cabal/FAQ.html
[cabal_user_guide]: https://www.haskell.org/cabal/users-guide/

## 14. Testing

unit testing, spec testing ([Hspec][hspec] [manual][hspec_manual], HUnit),
 property testing (QuickCheck)<br>
stack init

```haskell
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

-- shouldBe :: (Eq a, Show a) => a -> a -> Expectation -- augmented == with Show

-- in Test.QuickCheck
arbitrary :: Arbitrary a => Gen a
sample :: Show a => Gen a -> IO ()
sample' :: Gen a -> IO [a]
choose :: System.Random.Random a => (a, a) -> Gen a
elements :: [a] -> Gen a
frequency :: [(Int, Gen a)] -> Gen a
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property -- !
coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b
-- in Test.QuickCheck.Gen
oneof :: [Gen a] -> Gen a

runQc :: IO ()
runQc = quickCheck prop_additionGreater -- prop_additionGreater :: Int -> Bool
```

If you leave the types unspecified, the extended defaulting behavior of GHCi
 will (helpfully?) pick unit for you. If you run `sample arbitrary` directly in
 GHCi without specifying a type, it will default the type to `()` and give you a
 very nice list of empty tuples. Outside of GHCi, you'll get an error about an
 ambiguous type.<br>
`stack new project-name` automatically generates a file called `Setup.hs`.<br>
[containers][containers] package<br>
import - Control.Monad (forever, when), Data.List(intercalate), Data.Traversable
 (traverse), System.Environment (getArgs), System.Exit (exitFailure,
 exitSuccess), System.IO (hGetLine, hIsEOF, stdin)<br>
stack install (into $HOME/.local/bin, [XDG][xdg] guideline),
 stack ghci morse:tests

```haskell
morseToLetter = M.foldWithKey (flip M.insert) M.empty letterToMorse
M.lookup :: Ord k => k -> Map k a -> Maybe a
-- , M.keys and M.elems

-- in Data.Traversable
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
sequence :: Monad m => t (m a) -> m (t a)

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen
  (\c -> ((charToMorse c)
    >>= morseToChar) == Just c)
```

* [An introduction to QuickCheck testing][intro_to_quickcheck] by Pedro Vasconcelos
* QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs by Koen Claessen and John Hughes
* [Verifying a Simple Compiler Using Property-based Random Testing][verify_compiler] by Pedro Vasconcelos

```
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
-- vs
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing),
                         (3, liftM Just arbitrary)]
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r

import GHC.Generics
data Bool' = True' | False' deriving (Generic)
instance CoArbitrary Bool' -- randomly generate a function?
```

[How QuickCheck generates random functions][quickcheck_random_function]<br>
isomorphism?

[hspec]: http://hackage.haskell.org/package/hspec
[hspec_manual]: http://hspec.github.io/
[containers]: http://hackage.haskell.org/package/containers
[xdg]: https://wiki.archlinux.org/index.php/XDG_user_directories
[intro_to_quickcheck]: https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing
[verify_compiler]: http://www.dcc.fc.up.pt/dcc/Pubs/TReports/TR13/dcc-2013-06.pdf
[quickcheck_random_function]: https://kseo.github.io/posts/2016-12-14-how-quick-check-generate-random-functions.html

## 15. Monoid, Semigroup

One of the finer points of the Haskell community has been its propensity for
 recognizing abstract patterns in code which have well-defined, lawful
 representations in mathematics. A word frequently used to describe these
 abstractions in *algebra*, by which we mean one or more *operations* and the
 *set* they operate over.<br>
A monoid is a binary associative operation with an identity.

```haskell
class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m -- (<>)
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

To resolve the conflict, we have the `Sum` and `Product` newtypes to wrap
 numeric values and signal which `Monoid` instance we want.<br>
mappend (Sum 1) (Sum 5), mappend (Product 5) (Product 5)<br>
`newtype` is like a single-member C union that avoids creating an extra
 pointer.<br>
A variant of monoid that provides even stronger guarantees is the Abelian or
 commutative monoid.<br>
Among (many) other things, laws provide us guarantees that let us build on solid
 foundations for the predictable composition (or combination) of programs.<br>
The monoidal operation is less about combining the values and more about finding
 a (condensing or reducing) summary value for the set.<br>
Bool - All/Any, Maybe - First/Last/...(Monoid a)...<br>
`Monoid` abides by the law of associativity but not the law of
 commutativity.<br>
An orphan instance is when an instance is defined for a datatype and typeclass,
 but not in the same module as either the declaration of the typeclass or the
 datatype. Writing orphan instances should be avoided at all costs. If you don't
 "own" the typeclass or the datatype, newtype it! Your type class methods will
 start behaving differently depending on what modules are imported, which breaks
 the fundamental assumptions and niceties of type classes.<br>
`ghc -I. --make ListyInstances.hs`<br>
`quickCheck (monoidAssoc :: String -> String -> String -> Bool` (without
 `forAll`)<br>
`verboseCheck` - quickCheck while printing test values<br>
semigroup - binary, associative, no identity<br>
magma (or groupoid in abstract algebra, not groupoids in category theory) -
 binary, no associative, no identity<br>
`data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)`, head and length in
 Data.List.NonEmpty<br>
A symbolic data constructor can't be used as a prefix. An alphanumeric data
 constructor can't be used as an infix.<br>
`newtype NonEmpty a = NonEmpty (a, [a])` (possible! exactly one constructor with
 exactly one field)<br>
As of GHC 8.0.1, `Semigroup` and `NonEmpty` are both in `base` but not in
 `Prelude`.<br>
The strength of an algebra is the number of operations it provides.<br>
`(a -> b)` is the instance of Semigroup and Monoid.

* [Algebraic structure][algebraic_structure], Simple English Wikipedia
* [Haskell Monoids and their Uses][monoids_uses]

group, semiring, ring?

[algebraic_structure]: https://simple.wikipedia.org/wiki/Algebraic_structure
[monoids_uses]: http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html

## 16. Functors

Lifting is the "cheat mode" of type tetris. -- Michael Neale<br>
Rudolf Carnap, in the 1930s<br>
Functors are combinators: they take a sentence or phrase as input and produce a
 sentence or phrase as an output, with some logical operation applied to the
 whole. It lifts the concept of ... over the entire sentence or phrase structure
 without changing the internal structure. Functor is in some sense just a
 special sort of function application.<br>
If you are using GHC 8 or newer, `:set -XTypeApplications`, `:type fmap @Maybe`,
 `:type fmap @(Either _)`<br>
The application of `(->)`'s arguments must result in kind `*`.<br>
Just as GHC has type inference, it also has kind inference.<br>
`(<$>) == fmap`<br>
Earlier versions of Haskell didn't have a facility for expressing type classes
 in terms of higher-kinded types at all. This was developed by Mark P. Jones
 while he was working on an implementation of Haskell called *Gofer*. This work
 generalized type classes from being usable only with types of kind `*` (also
 called type *constants*) to being usable with higher-kinded types, called type
 *constructors*, as well. In Haskell, the two use cases have been merged such
 that we don't call out constructor classes as being separate from type classes.
 [A system of constructor classes: overloading and implicit higher-order polymorphism][gofer_generalizes_typeclasses]<br>
Functor laws - identity (`fmap id == id`), composition
 (`fmap (f . g) == fmap f . fmap g`)<br>
fmap == (.) for function<br>
A function is lifted over some structure f.<br>
The `IO` type is an abstract datatype; there are no data constructors that
 you're permitted to pattern match on, so the type classes `IO` provides are the
 only way you can work with values of type `IO a`. `IO` doesn't guarantee that
 effects will be performed, but it does mean that they *could* be performed.
 GHCi prints `IO` values unless the type is `IO ()`, in which case it hides the
 `Unit` value because it's meaningless.<br>
What if we wanted to transform only the *structure* and leave the *type
 argument* to that structure or type constructor alone? With this, we've arrived
 at *natural transformations*. This type is impossible because we can't have
 higher-kinded types as argument types to the function type. If you want to
 transform the values, write a pline old fold! We're going to return to the
 topics of natural transformations in the next chapter.

```haskell
functorCompose :: (Eq (f c), Functor f) =>
                       (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

Prelude> c = functorCompose (+1) (*2)
Prelude> li x = c (x :: [Int])
Prelude> quickCheck li

{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

Prelude> type IntToInt = Fun Int Int
Prelude> type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
Prelude> quickCheck (functorCompose' :: IntFC)

Prelude> type Nat f g = f a -> g a -- Not in scope: type variable 'a'
Prelude> :set -XRank2Types -- or XRankNTypes
Prelude> type Nat f g = forall a . f a -> g a
Prelude> type Nat f g a = f a -> g a
```

In Haskell, `Functor` instances will be unique for a given datatype, in part
 because of parametricity, in part because arguments to type constructors are
 applied in order of definition.

```haskell
{-# LANGUAGE FlexibleInstances #-} -- ?

data Tuple a b = Tuple a b deriving (Eq, Show)
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b
```

*Functor* is a mapping between categories. In Haskell, this manifests as a
 typeclass which lifts a function between two types over two new types. This
 conventionally implies some notion of a function which can be applied to a
 value with more structure than the unlifted function was originally designed
 for.

* [Haskell Wikibook; The Functor class][haskell_wikibook_functor]
* [The functor design pattern][functor_design_pattern] by Gabriel Gonzalez

https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns ?<br>
https://wiki.haskell.org/Rank-N_types ?<br>
[Haskell Programming with Nested Types : A Principled Approach][haskell_programming_with_nested_types]

[gofer_generalizes_typeclasses]: http://www.cs.tufts.edu/~nr/cs257/archive/mark-jones/fpca93.pdf
[haskell_wikibook_functor]: https://en.wikibooks.org/wiki/Haskell/The_Functor_class
[functor_design_pattern]: http://www.haskellforall.com/2012/09/the-functor-design-pattern.html
[haskell_programming_with_nested_types]: https://www.semanticscholar.org/paper/Haskell-Programming-with-Nested-Types-%3A-A-Approach-Johann-Ghani/3cb7d5c74497fc23d813de4d121c0796eb82962b

## 17. Applicative

Applicative is a monoidal functor. The `Applicative` typeclass allows for
 function application lifted over structure (like Functor). But with Applicative
 the function we're applying is also embedded in some structure. Because the
 function *and* the value it's being applied to both have structure, we have to
 smash those structures together. So, Applicative involves monoids and functors.
 (Monoid for a structure and function application)

```haskell
-- in Control.Applicative
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b -- called apply, ap or tie-figher

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

fmap f x = pure f <*> x -- f <?> x

instance Monoid a => Applicative ((,) a) -- Defined in 'GHC.Base'
instance (Monoid a, Monoid b) => Moniod (a, b)
("Woo", (+1)) <*> (" Hoo!", 0)

-- Cartesian product
(,) <?> [1, 2] <*> [3, 4] == liftA2 (,) [1, 2] [3, 4]

instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)
```

The monoidal bit may not be what you recognize as the canonical `mappend` of
 that type, because some types can have multiple monoids. `Applicative`
 instances, unlike `Functor`s, are not guaranteed to have a unique
 implementation for a given datatype. (due to many monoids, for example,
 `ZipList`)<br>
(`new type Constant a b = Constant { getConstant :: a }`) You use this when
 whatever you wawnt to do involves throwing away a function application.

Applicative laws
* Identity - `pure id <*> v = v`
* Composition - `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
* Homomorphism - `pure f <*> pure x = pure (f x)`
 * A homomorphism is a structure-preserving map between two categories
   (algebraic structures). The effect of applying a function that is embedded in
   some structure to a value that is embedded in some structure should be the
   same as applying a function to a value without affecting any outside
   structure.
* Interchange - `u <*> pure y = pure ($ y) <*> u`

`($ 2)` is the same as `\f -> f $ 2`.
Conal Elliott has a nice library called [checkers][checkers_module] on Hackage
 and GitHub which provides some nice properties and utilities for `QuickCheck`.
 We need to define `EqProp` for our custom datatype. This is straightforward
 because *checkers* exports a function called `eq` which reuses the preexisting
 `Eq` instance for the datatype. We may use an `eq` here. Finally, we're passing
 a value of our type to `monoid` so it knows which `Arbitary` instance to use to
 get random values --- note it doesn't *use* this value for anything.<br>
`EqProp` has the weird "check only the first 3,000 values" semantics. If you use
 a typical `EqProp` instance, the test for homomorphism in your `Applicativ`
 instance will chase the infinite lists forever.

```haskell
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance EqProp Bull where (=-=) = eq

main = quickBatch (monoid Twoo) -- A value does not matter. It uses a type only.
Prelude> quickBatch $ applicative [("b", "w", 1)]
Prelude> let trigger = undefined :: [(String, String, Int)]
Prelude> quickBatch (applicative trigger)

applicative
:: ( Show a, Show (m a), Show (m (a -> b))
   , Show (m (b -> c)), Applicative m
   , CoArbitrary a, EqProp (m a)
   , EqProp (m b), EqProp (m c)
   , Arbitrary a, Arbitrary b
   , Arbitrary (m a)
   , Arbitrary (m (a -> b))
   , Arbitrary (m (b -> c))
   => m (a, b, c) -> TestBatch
```

Rather than just short-circuiting when it has two error values (like `Either`),
 'Validation` will use the `Monoid` type class to combine them.

* [Validation library][validation_library] by Tony Morris and Nick Partridge
* [Applicative Programming with Effects][applicative_prog_effects] by Conor
  McBride and Ross Paterson
* Essence of the Iterator Pattern by Jeremy Gibbons and Bruno C. d. S. Oliveira
* [Constructing Applicative Functors][construct_applicative_functors] by Ross
  Paterson
* Idioms are oblivious, arrows are meticulous, monads are promiscuous by Sam
  Lindley, Philip Wadler and Jeremy Yallop
 * Idiom means applicative functor and is a useful search term for published
   work on applicative functors.

[checkers_module]: https://github.com/conal/checkers
[validation_library]: http://hackage.haskell.org/package/validation
[applicative_prog_effects]: http://staff.city.ac.uk/~ross/papers/Applicative.html
[construct_applicative_functors]: http://staff.city.ac.uk/~ross/papers/Constructors.html

## 18. Monad (SPOOKY? No.)

```haskell
class Applicative m => Monad m where -- GHC 7.10+
  (>>=) :: m a -> (a -> m b) -> m b -- bind, like (join . fmap)
    -- (=<<) :: (a -> f b) -> f a -> f b
  (>>) :: m a -> m b -> m b         -- sequencing operator
    -- (*>) :: Applicative f => f a -> f b -> f b
  return :: a -> m a                -- the same as pure

-- Monad is stronger than Applicative and Applicative is stronger than Functor.
-- You can derive Applicative and Functor in terms of Monad,
-- just as you can derive Functor in terms of Applicative
fmap f xs = xs >>= return . f

join :: Monad m => m (m a) -> m a
-- concat :: Foldable t => t [a] -> [a]
-- The ability to fallten those two layers of structure into one is what makes Monad special.
-- (where most of the monad logic is implemented)

-- before applicatives were discovered, still exists to maintain compatibility
liftA :: Applicative f => (a -> b)  -> f a  -> f b
liftM :: Monad m =>       (a1 -> r) -> m a1 -> m r
liftA2 :: Applicative f => (a -> b -> c)   -> f a  -> f b  -> f c
liftM2 :: Monad m =>       (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- zipWith is liftA2 or liftM2 specialized to lists, but uses a different list monoid.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith (+) [3, 4] [5, 6] -- [8,10]
liftA2 (+) [3, 4] [5, 6] -- [8,9,9,10]
-- liftA3, liftM3, zipWith3
ap == (<*>)

binding = do
  name <- getLine
  putStrLn name
binding' =
  getLine >>= putStrLn
  -- getLine >>= \name -> putStrLn name
```

Monads are applicative functors with some unique features. Monads are not,
 strictly speaking, necessary to Haskell. Older implementations of Haskell did
 not use monad for constructing and transforming IO actions. Monads are powerful
 and fun, but they do not define Haskell. Rather, monads are defined in terms of
 Haskell.<br>
["Monads are like burritos."][monads_like_burritos]<br>
Mr. Pointy sequences two actions while discarding any resulting value of the
 first action. `Applicative` has a similar operator as well, although we didn't
 talk about it.<br>
Monad is not 1) Impure, 2) An embedded language for imperative programming
 (there are commutative monads that do not order actions. we'll see one when we
 talk about `Reader`), 3) A value, 4) About strictness (some operations can be
 made strict within a specific instance). Using monads also doesn't require
 knowing math. Or category theory.<br>
"Haskell is the world's finest imperative programming language." -- Simon
 Peyton-Jones<br>
One of the strengths of Haskell is that we can refer to, compose, and map over
 effectful computations withour performing them or bending over backwards to
 make that pattern work. This merged `IO` action performs the effects in the
 order determined by the nesting of the `IO` actions. As it happens, the
 cleanest way to express ordering in a lambda calculus without bolting on
 something unpleasant is through nesting of expressions or lambdas.<br>
`Either` always short-circuits on the *first* thing to have failed. You can't
 make a `Monad` for `Validation` that accumulates the errors like the
 `Applicative` does. Instead, any `Monad` instance for `Validation` would be
 indentical to `Either`'s `Monad` instance.

Monad laws
* Identity laws - (right identity) `m >>= return = m`, (left identity)
  `return x >>= f = f x`
* Associativity - `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

```haskell
instance Monad CountMe where -- example
  return = pure

  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
mapM = sequence . fmap f
forM = flip mapM
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence = mapM id
```

Kleisli composition operator<br>
`(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c`

* [What a Monad is not][what_monad_is_not]
* [What I wish I knew when Learning Haskell][what_i_wish_i_knew] by Stephen
  Diehl
* [Monads Made Difficult][monads_made_difficult] by Stephen Diehl
* [Typeclassopedia][typeclassopedia] by Brent Yorgey

[monads_like_burritos]: http://blog.plover.com/prog/burritos.html
[what_monad_is_not]: https://wiki.haskell.org/What_a_Monad_is_not
[what_i_wish_i_knew]: http://dev.stephendiehl.com/hask/#monads
[monads_made_difficult]: http://www.stephendiehl.com/posts/monads.html
[typeclassopedia]: https://wiki.haskell.org/Typeclassopedia

## 19. Applying structure (Abstract structure applied, Monads gone wild)

[scotty (GitHub)][scotty],
 [Making A WebSite With Haskell][making_a_website_with_haskell], xmonad, snap,
 hgrev, [haproxy-haskell][haproxy_haskell], network, Seraph, `EitherT`<br>
`(<*) :: Applicative f => f a -> f b -> f a`<br>
[URL shortener example][url_shortener_example]<br>
[Stack video tutorial][stack_video_tutorial]<br>
`OverloadedStrings` make `String` literals polymorphic. (`Text`, `ByteString`,
 `fromString` of `IsString` typeclass)<br>
Conventionally, one uses `MonadIO` as a sort of auto-lift for `IO` actions, but
 you could do it manually. Using `liftIO` so that we can perform an `IO` action
 inside a `scotty ActionM`. `liftIO :: MonadIO m => IO a -> m a`

* [The case of the mysterious explosion in space][mysterious_explosion_in_space]
  (how GHC handles string literals)  by Bryan O'Sullivan

understand code samples?

[scotty]: https://github.com/scotty-web/scotty
[making_a_website_with_haskell]: http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html
[haproxy_haskell]: https://github.com/MichaelXavier/haproxy-haskell
[url_shortener_example]: https://github.com/bitemyapp/shawty-prime/blob/master/app/Main.hs
[stack_video_tutorial]: https://www.youtube.com/watch?v=sRonIB8ZStw
[mysterious_explosion_in_space]: http://www.serpentine.com/blog/2012/09/12/the-case-of-the-mysterious-explosion-in-space/

## 20. Foldable

The folding function is always dependent on some `Monoid` instance.<br>
While the `Prelude` as of GHCi 7.10 includes many changes related to the
 `Foldable` type class, not all of `Foldable` is in the `Prelude`.

```haskell
class Foldable (t :: * -> *) where
  {-# MINIMAL foldMap | foldr #-}
  fold :: Data.Monoid.Monoid m => t m -> m
  foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z t = appEndo (foldMap (Endo #. f) t) z

let xs :: [Sum Integer]
xs = [1, 2, 3, 4, 5]

toList :: t a -> [a]
null :: t a -> Bool -- null (Left 3) == null Nothing
length :: t a -> Int
-- fmap length Just [1, 2, 3] :: Int -- not Maybe Int
elem :: Eq a => a -> t a -> Bool
maximum :: forall a . Ord a => t a -> a -- the largest element of a non-empty structure
minimum :: forall a . Ord a => t a -> a -- forall is removed in the final book.
sum :: (Foldable t, Num a) => t a -> a
product :: (Foldable t, Num a) => t a -> a

-- Data.Monoid
newtype First a = First { getFirst :: Maybe a }
newtype Last a = Last { getLast :: Maybe a }
```

* [Foldable and Traversable][foldable_and_traversable] by Jakub Arnold

[foldable_and_traversable]: http://blog.jakubarnold.cz/2014/07/30/foldable-and-traversable.html

## 21. Traversable

`Traversable` was introduced in the same paper as `Applicative` and its
 introduction to Prelude didn't come until the release of GHC 7.10. However, it
 was available as part of the `base` library for much longer than that.
 `Traversable` depends on `Applicative`, and thus `Functor`, and is also
 superclassed by `Foldable`.<br>
`Traversable` allows you to transform elements inside the structure like a
 functor, producing applicative effects along theway, and lift those potentially
 multiple instances of applicative structure outside of the traversable
 structure.<br>
[vector package][vector_package], [wreq package][wreq_package]

```haskell
-- in Data.Traversable
class (Functor t, Foldable t) => Traversable t where
  {-# MINIMAL traverse | sequenceA #-}
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
         -- mapM :: Monad m => (a -> m b) -> [a] -> m [b] -- GHC prior to 7.10
                   -- (=<<) :: (a -> m b) -> m a -> m b -- flip bind
  traverse f = sequenceA . fmap f
         -- (sequence .) . fmap = \ f xs -> sequence (fmap f xs)
    -- implementation pattern: traverse f (Right y) = Right <$> f y
  sequenceA :: Applicative f => t (f a) -> f (t a)
      -- sequence :: Monad m => [m a] -> m [a] -- GHC prior to 7.10
  sequenceA = traverse id

(fmap . fmap) sum Just [1, 2, 3] -- step by step
-- (fmap . fmap) sum :: (Foldable t, Num b, Functor f1, Functor f) =>
--                      f (f1 (t b)) -> f (f1 b)
-- (fmap . fmap) sum Just :: (Foldable t, Num b) => t b -> Maybe b
-- If you turn `f (f1 (t b))` into `Just`, `f (f1 b)` becomes `t b -> Maybe b`.

-- in Data.Maybe
catMaybes [Just 1, Just 2, Nothing] == [1, 2]
-- cf. sequenceA [Just 1, Just2, Nothing] == Nothing
fromMaybe :: a -> Maybe a -> a

-- The weird looking composition, (join .) . fmap is because fmap takes two
--  arguments, so the expressions aren't proper unless we compose twice to await
--  a second argument for fmap to get applied to.
-- (f .) :: (a -> a1) -> a -> c
-- ((f .) .) :: (a -> a1 -> a2) -> a -> a1 -> c

-- Traversable is stronger than Functor and Foldable. Because of this, we can
-- recover the Functor and Foldable instance for a type from the Traversable,
-- just as we can recover the Functor and Applicative from the Monad.
fmap f t = runIdentity $ traverse (Identity . f) t
foldMap f t = getConstant $ traverse (Constant . f) t
```

Traversable Laws
* Naturality - `t . traverse f = traverse (t . f)`
* Identity - `traverse Identity = Identity`
* Composition - `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

sequenceA laws
* Naturality - `t . sequenceA = sequenceA . fmap t`
* Identity - `sequenceA . fmap Identity = Identity`
* Composition - `sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA`

```haskell
type TI = []

main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
```

[vector_package]: http://hackage.haskell.org/package/vector
[wreq_package]: http://hackage.haskell.org/package/wreq

## 22. Reader (functions waiting for input)

We don't want to simply pass this information as arguments because it would be
 present in the type of almost every function. To address this, we use the
 Reader Monad. It is a way of stringing functions together when all those
 functions are awaiting one input from a shared environment.<br>
*Reader* is not always `Reader`, sometimes it's the ambient `Applicative` or
 `Monad` associated with the partially applied function type, here that is
 `r ->`.

```haskell
-- Functor, Applicative, and Monad for partially-applied functions
fmap f1 f2 = f1 . f2
(+) <$> f1 <*> f2 = liftA2 (+) f1 f2
f1f2 = do
    a <- f1
    b <- f2
    return (a + b)

newtype Reader r a = -- out of date since mtl 2.0 (2010)
  Reader { runReader :: r -> a }
-- reader or asks :: MonadReader r m => (r -> a) -> m a
--                   Monad m => (r -> a) -> ReaderT r m a

(>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a -- ?
withReaderT f m = ReaderT $ runReaderT m . f
```

`InstanceSigs` is an extension we need in order to assert a type for the type
 class methods. The compiler already knows the type of the functions, so it's
 not usually necessary to assert the types in instances anyway.<br>
Speaking generally in terms of the algebras alone, you *cannot* get a `Monad`
 instance from the `Applicative`. However, our instances above aren't in terms
 of an abstract datatype; we *know* it's the type of functions.
 `m >> k = flip k <*> m`<br>
`Reader` rarely stands alone. Usually it's one `Monad` in a *stack* of multiple
 types providing a `Monad` instance. Usually if you have a `Reader`, it's of a
 record of several values that you're getting out of the `Reader`.<br>
A *monad transformer* is a special type that takes a monad as an argument and
 returns a monad as a result. It allows us to combine two monads into one that
 shares the behaviors of both.

* [All About Monads][all_about_monads]
* Programming with monads; Real World Haskell

[all_about_monads]: https://wiki.haskell.org/All_About_Monads

## 23. State

If you need in-place mutation, then the `ST` type is what you want.<br>
[random library][random]

```haskell
mkStdGen :: Int -> StdGen
next :: g -> (Int, g)
-- The range generated will be determined by the type.
random :: (RandomGen g, Random a) => g -> (a, g)
randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
ramdomIO :: Random a => IO a -- without needing to create a unique value for the StdGen

newtype State s a = State { runState :: s -> (a, s) } -- out of date
-- type State s = StateT s Identity
-- runStateT :: StateT s m a -> s -> m (a, s)
state :: Monad m => (s -> (a, s)) -> StateT s m a

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) [] -- execState :: State s a -> s -> s -- exec

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get -- get :: Monad m => StateT s m s -- State s s
    let result = fizzBuzz n
    put (result : xs) -- put :: Monad m => s -> StateT s m () -- State s ()

evalState :: State s a -> s -> a -- eval
modify :: Monad m => (s -> s) -> StateT s m () -- (s -> s) -> State s ()
```

[difference list][difference_list] `Data.DList` - `DL.empty`,
 `DL.apply dlist []`, `DL.snoc xs result`<br>
`DList`'s `Foldable` instance converts to a list before folding because of
 limitations specific to the datatype.

* [State Monad; Haskell Wiki][state_monad]
* [Understanding monads; Haskell Wikibook][understanding_monads]

[random]: https://hackage.haskell.org/package/random
[difference_list]: https://github.com/spl/dlist
[state_monad]: https://wiki.haskell.org/State_Monad
[understanding_monads]: https://en.wikibooks.org/wiki/Haskell/Understanding_monads

## 24. Parser combinators

[trifecta][trifecta]<br>
Critically, any *effect* the `m a` action had upon the monadic context remains.
 The result value of the parse function gets thrown away, but the effect of
 "moving the cursor" remains. Another possible effect is causing the parse to
 fail.<br>
simpler variant, Hutton-Meijer parser (polyparse-1.11,
 `Text.ParserCombinators.HuttonMeijer`)<br>
Realistically, not all `Monad`s have a proper implementation of `fail`, so it
 will be moved out into a `MonadFail` class eventually.<br>
`parsec` and `attoparsec` are perhaps the two most well known parser combinator
 libraries in Haskell, but there is also `megaparsec` and others. `aeson` and
 `cassava` are among the libraries designed for parsing specific types of data
 (JSON data and CSV data, respectively). If you intend to do a lot of parsing
 in production, you may need to get comfortable using `attoparsec`, as it
 particularly known for very speedy parsing.<br>
The design of `trifecta` has evolved such that the API is split across two
 libraries, [`parsers`][parsers_package] and `trifecta`. The reason for this is
 that the `trifecta` package itself provides the concrete implementation of the
 `trifecta` parser as well as `trifecta`-specific functionality, but the
 `parsers` API is a collection of type classes that abstract over different
 kinds of things parsers can do. The `Text.Trifecta` module handles exporting
 what you need to get started from each package.

```haskell
type Parser a = String -> Maybe (a, String)

-- Text.Trifecta.Parser
parseString :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a
-- Delta is a starting cursor position. (mempty == Columns 0 0)
parseByteString :: Parser a -> Text.Trifecta.Delta.Delta -> ByteString -> Result a
decimal :: Integral a => Parser a -- TokenParsing m => m Integer in Text.Parser.Token

class Alternative m => Parsing m where -- Text.Parser.Combinators
  -- Minimal complete definition: try, (<?>), not FollowedBy
  try :: m a -> m a
  (<?>) :: m a -> String -> m a -- Give a parser name
  notFollowedBy :: Show a => m a -> m ()
  -- keywordLet = try $ string "let" <* notFollowedBy alphaNum
  eof :: (MonadTrans t, Monad n, Parsing n, m ~ t n) => m ()
  eof = notFollowedBy anyChar <?> "end of input"

class Parsing m => CharParsing m where
  char :: Char -> m Char
  notChar :: Char -> m Char
  anyChar :: m Char
  string :: String -> m String
  string s = s <$ try (traverse_ char s) <?> show s
  -- <$ :: Functor f => a -> f b -> f a
  text :: Text -> m Text
  text t = t <$ string (unpack t)

class Applicative f => Alternative f where -- Control.Applicative
  empty :: f a -- the identity of <|>
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  -- many_v = some_v <|> pure []
  -- some_v = (fmap (:) v) <*> many_v -- liftA2 (:) v many_v
```

The `[r|` is beginning a [quasiquoted][quasiquotation] section , using the
 quasiquoter named `r`. Now we had to enable the `QuasiQuotes` language
 extension to use this syntax. At time of writing `r` is defined in
 `raw-strings-qq` version 1.1 as follows.<br>
If you want to see what a quasiqouter or [Template Haskell][template_haskell] is
 generating at compile-time, you can enable the `-ddump-splices` flag.<br>
Lexing is sometimes done with *regular expressions*, but a parsing library in
 Haskell will usually intend that you do your lexing and parsing with the same
 API.<br>
The `integer` function is already a tokenizer. The tokenization behavior (such
 as ignoring whitespaces) only applies to what followed. Overuse of tokenizing
 parsers or mixture with character parsers can make your parser slow or hard to
 understand.<br>
[Parsec: "try a <|> b" considered harmful][parsec_try_alt_considered_harmful]

```haskell
r :: QuasiQuoter
r = QuasiQuoter {
  quoteExp =
    return . LitE . StringL . normaliseNewlines,
  qoutePat  = \_ -> fail "some error message",
  qouteType = \_ -> fail "some error message",
  qouteDec  = \_ -> fail "some error message",

class CharParsing m => TokenParsing m where -- Text.Parser.Token
  token :: m a -> m a
  token p = p <* (someSpace <|> pure ())

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

p' :: Parser [Integer]
p' = some $ do
  i <- token (some digit)
  return (read i)

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = -- recommend annotations like this
       (try (char '1' >> char '2')
       <?> "Tried 12")
   <|> (char "3" <?> "Tried 3")

encode :: ToJSON a => a -> LBS.ByteString
decode :: FromJSON a => LBS.ByteString -> Maybe a
eitherDecode :: FromJSON a => ByteString -> Either String a
(.:) :: FromJSON a => Object -> Text -> Data.Aeson.Types.Parser a

instance FromJSON TetData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ =
    fail "Expected an object for TestData"

instance FromJSON Color where
  parseJSON (Object v) =
        (Red <$> v. .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ =
    fail "Expected an object for Color"
```

TODO: chapter exercises

* Code case study: parsing a binary data format, The Parsec parsing library;
  Real World Haskell
* [An introduction to parsing text in Haskell with Parsec][parsec_intro]
* [Parsing CSS with Parsec][parsing_css_with_parsec]
* [Parsec: A practical parser library][parsec_practical_parser_library]
* [How to Replace Failure by a List of Success][how_to_replace_failure_by_a_list_of_success]
* [Two kinds of backtracking][two_kinds_of_backtracking]
* [LL and LR in Context: Why Parsing Tools Are Hard][ll_and_lr_in_context]
* *Parsing Techniques: A Practical Guide (second edition)* by Grune and Jacobs
* [Parsing JSON with Aeson][parsing_json_with_aeson]
* [24 Days of Hackage: aeson][aeson_ocharles]

[trifecta]: http://hackage.haskell.org/package/trifecta-1.5.2
[parsers_package]: http://hackage.haskell.org/package/parsers
[quasiquotation]: https://wiki.haskell.org/Quasquotation
[template_haskell]: https://wiki.haskell.org/Template_Haskell
[parsec_try_alt_considered_harmful]: http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/
[parsec_intro]: https://www.cnblogs.com/ncore/p/6892500.html
[parsing_css_with_parsec]: https://blog.jakuba.net/2014/08/10/Parsing-CSS-with-Parsec/
[parsec_practical_parser_library]: https://www.researchgate.net/publication/2407206_Parsec_A_practical_parser_library
[how_to_replace_failure_by_a_list_of_success]: https://rkrishnan.org/files/wadler-1985.pdf
[two_kinds_of_backtracking]: http://gelisam.blogspot.com/2015/09/two-kinds-of-backtracking.html
[ll_and_lr_in_context]: http://blog.reverberate.org/2013/09/ll-and-lr-in-context-why-parsing-tools.html
[parsing_json_with_aeson]: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
[aeson_charles]: https://ocharles.org.uk/posts/2012-12-07-24-days-of-hackage-aeson.html

## 25. Composing Types (e pluribus monad)

We'll be using types that correspond to `id` and `(.)`.<br>
A monad transformer is a type constructor that takes a monad as an argument.
 Monad transformers are most commonly written as newtypes.<br>
Getting another `Monad` given the composition of two *arbitrary* types that have
 a `Monad` instance is impossible. The fundamental problem with composing two
 monads lies in the impossibility of joining two unknown monad.s In order to
 make that `join` happen, we need to reduce the polymorphism and get concrete
 information about one of the monads that we're working with.
 [Composing monads][composing_monads]

```haskell
newtype Identity a =
  Identity { runIdentity :: a}
  deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
    -- Compose ((<*>) <$> f <*> x), not Compose $ f <*> x
    liftA2 f (Compose x) (Compose y) = Compose (liftA2 (liftA2 f) x y)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose empty
    (<|>) = coerce ((<|>) :: f (g a) -> f (g a) -> f (g a))
      :: forall a. Compose f g a -> Compose f g a -> Compose f g a

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

(>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
(IdentityT ma) >>= f = IDentityT $ ma >>= runIdentityT . f
m >>= k = IdentityT $ runIdentityT . k <=<< runIdentityT m -- from the `transformers` library
```

We needed to know one of the types concretely so that we could use
 `runIdentityT` (`f (g (f b))` → `f (f b)`, essentially `fmap`ping a *fold* of
 the `IdentityT` structure) and then repack the value in `IdentityT`. Since
 `IdentityT` is so simple, the record accessor is sufficient to fold up the
 structure.<br>
We use transformers when we want a `>>=` operation over *f* and *g* of different
 types (but both have `Monad` instances). You have to create new types called
 monad transformers and write `Monad` instances for those types to have a way of
 dealing with the extra structure generated.<br>
basic pattern: `m (T m b) -> m (m b) -> m b -> T m b`<br>
Lifting means you're embedding an expression in a larger context by adding
 structure that doesn't do anything.

[composing_monads]: http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf

## 26. Monad transformers

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

newtype Reader r a = Reader { runReader :: r -> a }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

newtype Writer w a = Writer { runWriter :: (a, w) }
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
-- tell :: MonadWriter w m => w -> m ()
--         (Monoid w, Monad m) => w -> WriterT w m ()

newtype State s a = State { runState :: s -> (a, s) }
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
-- similarity: type Parser = StateT String Maybe

newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

type MyIdentity a = IdentityT Identity a
type Maybe    a = MaybeT Identity a
type Either e a = EitherT e Identity a
type Reader r a = ReaderT e Identity a
type State  s a = StateT s Identity a

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) } -- deprecated

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = return 1
(runReaderT . runExceptT . runMaybeT $ embedded) :: () -> IO (Either String (Maybe Int))

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

newtype Scotty e m a =
  ScottyT
  { runS :: State (ScottyState e m) a }
  deriving (Functor, Applicative, Monad)

newtype ActionT e m a =
  ActionT
  { runAM
    :: ExceptT
         (ActionError e)
         (ReaderT ActionEnv
           (StateT ScottyResponse m))
         a
  }
  deriving ( Functor, Applicative )

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO

class (Monad m) => MonadIO m where -- Control.Monad.IO.Class
    liftIO :: IO a -> m a
-- example: liftIO :: IO a -> ExceptT e (StateT s (ReaderT r IO)) a
-- law 1. liftIO . return = return
-- law 2. lift (m >>= f) = liftIO m >>= (liftIO . f)
```

The constraint on *m* of `StateT` is *not* `Applicative`, but rather `Monad`.
 This is because you can't express the order-dependent computation you'd expect
 the `StateT Applicative` to have without having a Monad for *m*. In essence,
 without `Monad`, you're feeding the initial state to each computation in
 `StateT` rather than threading it through as you go.
 [1][statet_applicative_issue1], [2][statet_applicative_issue2]<br>
The `Writer Applicative` and `Monad` work by combining the *w* values
 monoidally. It's a bit too easy to get into a situation where `Writer` is
 either too lazy or too strict for the problem you're solving, and then it'll
 use more memory than you'd like. `Writer` can accumulate unevaluated thunks,
 causing memory leaks. It's also inappropriate for logging long-running or
 ongoing programs due to the fact that you can't retrieve any of the logged
 values until the computation is complete.
 [Streaming logging][streaming_logging]<br>
Usually when `Writer` is used in an application, it's not called `Writer`.
 Instead a one-off is created for a specific type *w*. It's still useful to know
 when you're looking at something that's a `Reader`, `Writer`, or `State`, even
 if the author didn't use the types by those names from the `transformers`
 library. Sometimes this is because they wanted a stricter `Writer` than the one
 already available.<br>
ListT is not very fast and most people's first attempt won't pass the
 associativity law. Streaming libraries like `pipes` and `conduit` do it better
 for most use cases. Prior art for "`ListT` done right" also includes
 [`AmbT`][ambt], although you may find it challenging to understand if you
 aren't familiar with `ContT` and the motivation behind `Amb`.<br>
Lists in Haskell are as much a control structure as a data structure, so
 streaming libraries such as `pipes` generally suffice if you need a
 transformer.<br>
When Haskellers say *base monad* they usually mean what is structurally
 outermost.<br>
Rather than (`MonadTrans`) lifting through one layer at a time, `MonadIO` is
 intended to keep lifting your `IO` action until it is lifted over *all*
 structure embedded in the outermost `IO` type. Monads in which IO computations
 may be embedded. Any monad built by applying a sequence of monad transformers
 to the IO monad will be an instance of this class.

[statet_applicative_issue1]: https://stackoverflow.com/questions/18673525/is-it-possible-to-implement-applicative-m-applicative-statet-s-m
[statet_applicative_issue2]: https://github.com/data61/fp-course/issues/134
[streaming_logging]: http://www.haskellforall.com/2014/02/streaming-logging.html
[ambt]: https://wiki.haskell.org/Amb

## 27. Non-strictness

Technically Haskell is only obligated to be nonstrict, not lazy. This is one of
 the ways in which nonstrictness makes Haskell expressive---we can refer to
 values before we've done the work to create them.<br>
`seq :: a -> b -> b`<br>
In some old versions of Haskell, `Eval` is short for *evaluation to weak head
 normal form*, and is provided a method for forcing evaluation. Instances were
 provided for all the types in `base`.<br>
WHNF evaluation means it stops at the first data constructor or lambda.<br>
[GHC Core][ghc_core], `:set -ddump-simpl` and `:set -dsuppress-all`<br>
In Core, a case expression *always* evaluates what it cases on---even if no
 pattern matching is performed---whereas in Haskell proper, values are forced
 when matching on data constructors. In Haskell, case matgching is strict---or,
 at least, the pattern matching of it is---up to WHNF. In Core, cases are always
 strict to WHNF. Case in Core is strict even if there's one case and it doesn't
 match on anything.<br>
Essentially [thunks][thunk] are computations not yet evaluated up to weak head
 normal form. If you read the GHC notes on this you'll see references to *head
 normal form*---it's the same thing as weak head normal form. This is an
 opportunistic strictness. GHC will not thunk (and thus delay) data
 constructors. Data constructors are known to be constant, which justifies the
 safety of the optiminzation. This is one of the things that confounds the use
 of `sprint` to observe evaluation in GHCi---GHC will often be opportunistically
 strict with data constructors if it knows the contents definitely can't be a
 bottom, such as when they're a literal value.<br>
It turns sharing on and off (that is, it oscillates between call-by-need and
 call-by-name) based on necessity and what it thinks will produce faster code.
 Part of the reason it can do this at all without breaking your code is because
 the compiler knows when your code does or does not perform I/O. `Debug.Trace`
 in the *base* library is a means of cheating the type system and putting a
 `putStrLn` without having `IO` in the type.<br>
Names turn out to be a pretty good way to make GHC share something, *if* it
 could've otherwise been shared.<br>
Inlining expressions (such as functions) where they get used prevents sharing
 because it creates independent thunks that will get computed separately. Being
 a function with **explicit, named arguments** also prevent sharing. Eta
 reduction (i.e., writing pointfree code, thus dropping the named arguments)
 will change the sharing properties of your code.<br>
Type class constraints are a function in Core. They are awaiting application to
 something that will make them become concrete types. Sharing doesn't work in
 the presence of constraints (type classes or implicit parameters) because type
 class constraints and implicit parameters decay into function arguments when
 the compiler simplifies the code. In most cases where you believe you want
 implicit parameters (`-XImplicitParams`), more likely you want `Reader,
 `ReaderT`, or a plain old function argument.<br>
Values of a concrete, constant type can be shared, once evaluated. Functions
 aren't shared when there are named arguments but are when the arguments are
 elided, as in pointfree. You can force sharing by giving your expression a
 name. `forever :: (Monad m) => m a -> m b` `forever a = let a' = a >> a' in a'`
 (Due to the `let` expression,) the sharing here causes GHC to overwrite the
 thunk as it runs each step in the evaluation, which is quite handy. Otherwise,
 it would keep constructing new thunks indefinitely and that would be very
 unfortunate.<br>
An *irrefutable pattern* is one which will never fail to match. A *refutable
 pattern* is one which has potential failures. The *pattern* is refutable or
 not, not the function itself. Lazy patterns are also *irrefutable*.<br>
`BangPatterns` - sometimes we want to evaluate an argument to a function whether
 we use it or not.<br>
A good rule to follow is lazy in the spine, strict in the leaves!<br>
If you're using GHC 8.0 or newer, you can avail yourself of the `Strict` and
 `StrictData` extensions. It makes functions defined in that module processing
 lazy data structures behave differently. You can use the tilde for irrefutable
 patterns to recover laziness on a case by case basis.

* [The Incomplete Guide to Lazy Evaluation (in Haskell)][incomplete_guide_to_lazy_evaluation]
* [Lazy evaluation illustrated for Haskell divers][lazy_evaulation_illustrated]
* [A Natural Semantics for Lazy Evaluation][natural_semantics_for_lazy_evaluation]
* [An operational semantics for parallel lazy evaluation][operational_semantics_for_parallel_lazy_evaluation]

[ghc_core]: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
[thunks]: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects
[incomplete_guide_to_lazy_evaluation]: https://github.com/pushcx/hpffp-resources/blob/master/Chapter%2027/The%20Incomplete%20Guide%20to%20Lazy%20Evaluation%20(in%20Haskell).pdf
[lazy_evaulation_illustrated]: https://github.com/takenobu-hs/lazy_evaluation
[natural_semantics_for_lazy_evaluation]: https://galois.com/wp-content/uploads/2014/08/pub_JL_NaturalSemanticsForLazyEvaluation.pdf
[operational_semantics_for_parallel_lazy_evaluation]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.8875&rep=rep1&type=pdf

## 28. Basic libraries

> Bad programmers worry about the code. Good programmers worry about data
> structures and their relationships. (Linux Torvalds)

[`criterion`][criterion] (by Bryan O'Sullivan)
 [tutorial][criterion_tutorial]<br>
The reason it wants a function it can apply an argument to is so that the result
 isn't shared.<br>
Keep in mind that if you want to use your own datatype with `nf`, which has an
 `NFData` constraint you will need to provide your own instance. You can find
 examples in the `deepseq` library on Hackage.<br>
Report version `#ifdef USE_REPORT_PRELUDE`<br>
`foldr` often benefits from the various rewrite rules and optimizations attached
 to it.

```haskell
defaultMain :: [Benchmark] -> IO ()
whnf :: (a -> b) -> a -> Benchmarkable
nf :: Control.DeepSeq.NFData b => (a -> b) -> a -> Benchmarkable
-- require this function application in order to disable sharing

infixl 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise =
      foldr
      (\x r k ->
        case k of
          0 -> Just x
          _ -> r (k-1))
      (const Nothing) xs n

((\_ -> Just undefined) 0) `seq` 1 -- 1
((\_ -> undefined) 0) `seq` 1      -- *** Exception: Prelude.undefined
```

`whnf` isn't sufficient for benchmarking, something that uses guarded recursion
 (when a data constructor is interposed between each recursion step). Guarded
 recursion lets us consume the recursion steps up to weak head normal form
 incrementally on demand. `foldr` can be used to implement guarded and unguarded
 recursion, depending *entirely* on what the folding function does rather than
 any special provision made by `foldr` itself.<br>
[Profiling; GHC User's Guide][profiling]<br>
`stack ghc -- -prof -fprof-auto -rtsopts -O2 profile.hs; ./profile +RTS -P; cat profile.prof`<br>
`-prof-auto` assigns all bindings not marked inline a cost center named after
 the binding.<br>
`-rtsopts` enables you to pass GHC RTS options to the generated binary. This is
 optional so you can get a smaller binary if desired. We need this to tell our
 program to dump the profile to the `-prof` file named after our program.<br>
Most Haskell programmers feel pretty free to default to `-O2`.<br>
(heap usage) `./loci +RTS -hc -p; hp2ps loci.hp`<br>
CAFs (constant applicative forms) are expressions that have no free variables
 and are held in memory to be shared with all other expressions in a module.
 They can be literal values or partially applied functions that have no named
 arguments (or fully applied functions, although that would be rare in real
 code). If you find your program using much more memory than you expected, find
 the golden CAF and kill it. Real world code is usually pulling the data from
 somewhere, which avoids the problem of holding large amounts of data in memory.

```haskell
data Map k a = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a) -- strict or lazy
             | Tip
type Size = Int

iterate :: (a -> a) -> a -> [a]

newtype Seq a = Seq (FingerTree (Elem a))
-- Elem is so elements and nodes can be
-- distinguished in the types of the
-- implementation. Don't sweat it.
newtype Elem a = Elem { getElem :: a }
data FingerType a
    = Empty
    = Single a
    = Deep {-# UNPACK #-} !Int !(Digit a) (FingerTree (Node a)) !(Digit a)
data Digit a = One a | Two a a | Three a a a | Four a a a a
data Node a = Node2 a a | Node3 a a a

data Vector a =
     Vector {-# UNPACK #-} !Int
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !(Array a)
     deriving ( Typeable )

(//) :: Vector a -> [(Int, a)] -> Vector a -- batch update
update :: Vector a -> Vector (Int, a) -> Vector a -- even faster

import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = return v
        go n v =
          (MV.write v n 0) >> go (n-1) v

mutableUpdateST  :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = V.freeze V
        go n v =
          (MV.write v n 0) >> go (n-1) v

-- type role ST nominal representational
newtype ST s a = -- Control.Monad.ST
  GHC.ST.ST (GHC.ST.STRep s a)
type STRep s a =
     GHC.Prim.State# s
  -> (# GHC.Prim.State# s, a #)
```

Using an `Int` as your key type is usually a sign you'd be better off with a
 `HashMap`, `IntMap`, or `Vector`.<br>
`Sequence` appends cheaply on the front and the back.
 [Finger Trees: A Simple General-purpose Data Structure][finger_trees]<br>
One rarely uses arrays, or more specifically, `Array` in Haskell. `Vector` is
 almost always what you want instead of an array. The default `Vector` type is
 implemented as a slice wrapper of `Array`. These include boxed, unboxed,
 immutable, and storable vectors, but the plain version above is the usual one
 you'd use. *Boxed* means the vector can reference anhy datatype you want;
 *unboxed* represents raw values without pointer indirection. The latter can
 save a lot of memory but is limited to types like `Bool`, `Char`, `Float`,
 `Int`, `Word`, tuples of unboxable values.<br>
[Fusion][stream_fusion] ([1][stream_fusion_long]), or loop fusion, means that as
 an optimization the compiler can fuse several loops into one megaloop and do it
 in one pass. The `vector` library has loop fusion built in, through the use of
 [GHC RULES][ghc_rules].<br>
What sets Haskell apart is that we cannot do so in a way that compromises
 referential transparency or the nice equational properties our expressions
 have.<br>
The added time in the `ST` version is from freezing the mutable vector into an
 ordinary vector. `ST` can be handy when you wnt to temporarily make something
 mutable and ensure no mutable references are exposed outside of the `ST` monad. `ST` can be thought of as a mutable variant of the strict `State` monad. From
 another angle, it could be thought of as `IO` restricted exclusively to
 mutation which is guaranteed safe. The idea behind this "morally effect-free"
 understanding of mutable state was introduced in the paper
 [*Lazy Functional State Threads*][lazy_functional_state_threads]. For `ST` to
 work properly, the code that mutates the data must not get reordered by the
 optimizer or otherwise monkeyed with, much like the code in `IO`. What's
 important is that *s* is getting its type from the thing you're mutating, but
 it has no value-level witness. The `State` monad here is therefore erasable; it
 can encapsulate this mutation process and then melt away. The mutation is a
 safe effect of having entered the *closures* that perform the effect. This
 strict, unlifted state transformer monad happens to structure your code in a
 way that preserves the intended order of the computations and their associated
 effects. Entering each lamb da performs its effects. `ST` enforces it at
 compile time by making it so that *s* will never unify with anything outside of
 the `ST Monad`. The trick for this is called
 [existential quantification][existential_quantification]. It does prevent you
 from accidentally leaking mutable references to code outside `ST`, which could
 then lead to code that does unpredictable things depending on the state of the
 bits in memory.<br>
However, `Text` is encoded as UTF-16.<br>
The issue is that (not Lazy) `Text` went ahead and loaded the entire file into
 memory. The proper way to handle incrementally processing data is with
 [streaming][pipes], but this is not something we'll cover in detail in this
 book. We do strongly recommend using streaming rather than relying on a lazy IO
 API.<br>
Many Haskellers will mistakenly use the `Char8` module in the `bytestring`
 library. The `Char8` module is really a convenience for data that mingles bytes
 and ASCII data there. It doesn't work for Unicode and shouldn't be used
 anywhere there's even a *hint* of possibility that there could be Unicode data.
 The `pack` function the `Char8` module contains is for ASCII data only!

* [Demystifying DList][demystifying_dlist]
* [Memory Management; GHC; Haskell Wiki][wiki_ghc_memory_management]
* [Performance; Haskell Wiki][wiki_performance]
* [Pragmas; GHC User's Guide][pragmas]
* [High Performance Haskell][high_performance_haskell],
  [Haskell Performance Patterns][haskell_performance_patterns],
  [Faster persistent data structures through hashing][faster_persistent_data_structures_through_hashing]
* [Write Haskell as fast as C: exploiting strictness, laziness and recursion][write_haskell_as_fast_as_c]
* [Haskell as fast as C: A case study][haskell_as_fast_as_c]
* [Haskell FFT 11: Optimisation Part 1][haskell_fft_11]
* [Understanding the RealWorld][understanding_the_realworld]
* *Purely functional data structures*

[criterion]: http://hackage.haskell.org/package/criterion
[criterion_tutorial]: http://www.serpentine.com/criterion/tutorial.html
[profiling]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
[finger_trees]: http://www.staff.city.ac.uk/~ross/papers/FingerTree.html
[stream_fusion]: http://fun.cs.tufts.edu/stream-fusion.pdf
[stream_fusion_long]: https://ora.ox.ac.uk/objects/uuid:b4971f57-2b94-4fdf-a5c0-98d6935a44da/download_file?file_format=pdf&safe_filename=Thesis%2BPDF%252C%2Bebook%2Blayout%2B%2528A5%252C%2Bno%2Bmargins%2529
[ghc_rules]: https://wiki.haskell.org/GHC/Using_rules
[lazy_functional_state_threads]: https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf
[existential_quantification]: https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
[pipes]: https://wiki.haskell.org/Pipes
[demystifying_dlist]: http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/
[wiki_ghc_memory_management]: https://wiki.haskell.org/GHC/Memory_Management
[wiki_performance]: https://wiki.haskell.org/Performance 
[pragmas]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pragmas
[high_performance_haskell]: http://johantibell.com/files/slides.pdf
[haskell_performance_patterns]: https://johantibell.com/files/haskell-performance-patterns.html
[faster_persistent_data_structures_through_hashing]: http://johantibell.com/files/galois-2011.pdf
[write_haskell_as_fast_as_c]: https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
[haskell_as_fast_as_c]: http://lambda.jstolarek.com/2013/04/haskell-as-fast-as-c-a-case-study/
[haskell_fft_11]: https://www.skybluetrades.net/blog/posts/2014/01/10/data-analysis-fft-11/index.html
[understanding_the_realworld]: https://www.well-typed.com/blog/2014/06/understanding-the-realworld/

## 29. IO

Haskell expressions which aren't in `IO` will always return the same result
 regardless of what order they are evaluated in. The important thing about `IO`
 is that it's a special kind of datatype that disallows sharing in some
 cases.<br>
Somebody is writing an explanation of `IO` right now that uses van Laarhoven
 Free Monads and costate comonad coalgebras to explain something that's much
 simpler than either of those topics.<br>
While `IO` is a type that has a `Monad` instance, it is not *only* a `Monad` and
 monads are not only `IO`.<br>
(early in the documentation of `GHC.IO`) The IO Monad is just an instance of the
 ST monad, where the state is the real world.

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

System.Environment.getArgs :: IO [String]
System.IO.hPutStr :: Handle -> String -> IO ()
System.IO.hGetChar :: Handle -> IO Char
System.IO.stdout :: Handle
System.IO.stdin :: Handle
System.IO.hWaitForInput :: Handle -> Int -> IO Bool
System.IO.stderr :: Handle
```

The `#` indicates a *primitive type*. These are types that cannot be defined in
 Haskell itself and are exported by the `GHC.Prim` module. The `State` here is a
 signalling mechanism for telling GHC what order your `IO` actions are in and
 what a unique `IO` action is. / (In the `GHC.Prim` documentation) State# is the
 primitive, unlifted type of states. It has one type parameter, thus State#
 RealWorld, or State# *s*, where *s* is a type variable. The only purpose of the
 type parameter is to keep different state threads separate. It is represented
 by nothing at all (i.e., it literally uses zero bits of memory). RealWorld is
 deeply magical. It is primitive, but it is not unlifted (hence ptrArg). We
 never manipulate values of type RealWorld; it's only used in the type system,
 to parameterise `State#`. / The state tokens underlying the `IO` type are
 erased during compile time and add no overhead to your runtime.<br>
In fact, the reason we have `Monad` is because it was a means of abstracting
 away the nested lambda noise that underlies `IO`.<br>
Where you do not have a value but only a means of getting a value, it wouldn't
 make sense to say that value could be shared.
 ([cake analogy][upenn_lecture_io])<br>
The `MVar` type is a means of synchronizing shared data in Haskell.<br>
What was originally meant when describing a pure functional programming language
 is that the semantics of the language would only be lambda calculus. We use
 nested lambdas (hidden behind a `Monad` abstraction) to order and encapsulate
 effects while maintaining referential transparency. Put casually, it means that
 any function, when given the same inputs, returns the same result. More
 precisely, an expression is referentially transparent when it can be replaced
 with its value without changing the behavior of a program. A function that
 returns `IO a` is still referentially transparent, because given the same
 arguments, it'll generate the same `IO` action every time! Haskellers will
 often get confused when they are told `Monad`'s bind is associative because
 they'll think of `IO` as a counterexample.<br>
[A Correspondence Between ALGOL 60 and Church's Lambda-Notation][algol_and_lambda]<br>
We'll use this opportunity to examine an *unsafe* means of enabling sharing for
 an `IO` action: `unsafePerformIO :: IO a -> a`! Do *not* use `unsafePerformIO`
 when unnecessary or where it could break referential transparency in your code!
 There are other unsafe `IO` functions, too, but there is rarely a need for any
 of then, and in general you should prefer explicit rather than implicit
 (sharing).

* [Referential Transparency; Haskell Wiki][referential_transparency]
* [IO Inside; Haskell Wiki][io_inside]
* [Unraveling the mystery of the IO Monad][unraveling_mystery_io_monad]
* [Primitive Haskell][primitive_haskell]
* [Evaluation order and state tokens][evaluation_order_and_state_tokens]
* [Haskell GHC Illustrated][haskell_ghc_illustrated]
* [Tackling the Awkward Squard: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell][tackling_the_awkard_squad]
* [Note \[IO hack in the demand analyser\]; GHC source code][io_hack_in_demand_analyser]
* [Monadic I/O in Haskell 1.3][monadic_io_in_haskell_1_3]
* [Notions of computation and monads][notions_of_computation_and_monads]
* [The Next 700 Programming Languages][next_700_programming_languages]
* [Haskell Report 1.2][haskell_report_1_2]

[upenn_lecture_io]: http://www.cis.upenn.edu/~cis194/spring13/lectures/08-IO.html
[algol_and_lambda]: https://fi.ort.edu.uy/innovaportal/file/20124/1/22-landin_correspondence-between-algol-60-and-churchs-lambda-notation.pdf
[referential_transparency]: https://wiki.haskell.org/Referential_transparency
[io_inside]: https://wiki.haskell.org/IO_inside
[unraveling_mystery_io_monad]: http://blog.ezyang.com/2011/05/unraveling-the-mystery-of-the-io-monad/
[primitive_haskell]: https://haskell.fpcomplete.com/tutorial/primitive-haskell
[evaluation_order_and_state_tokens]: https://wiki.haskell.org/Evaluation_order_and_state_tokens
[haskell_ghc_illustrated]: https://github.com/takenobu-hs/haskell-ghc-illustrated
[tackling_the_awkard_squad]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf
[io_hack_in_demand_analyser]: https://github.com/ghc/ghc/blob/master/compiler/stranal/DmdAnal.hs#L361
[monadic_io_in_haskell_1_3]: https://www.microsoft.com/en-us/research/publication/monadic-io-in-haskell-1-3/
[notions_of_computation_and_monads]: https://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf
[next_700_programming_languages]: https://www.cs.cmu.edu/~crary/819-f09/Landin66.pdf
[haskell_report_1_2]: http://haskell.org/definition/haskell-report-1.2.ps.gz

## 30. Exceptions

[An Extensible Dynamically-Typed Hierarchy of Exceptions][ext_exceptions]

```haskell
class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String

-- IOException, Deadlock, BlockedIndefinitelyOnSTM, BlockedIndefinitelyOnMVar,
-- AsyncException, AssertionFailed, AllocationLimitExceeded, SomeException,
-- ErrorCall, ArithException = Overflow | Underflow | LossOfPrecesion |
-- DivideByZero | Denormal | RatioZeroDenominator

data SomeException where -- GADT
  SomeException
    :: Exception e => e -> SomeException
-- data SomeException = forall e . Exception e => SomeException e
-- ^ existential quantification vs universally quantified

data MyException =
  forall e .
  (Show e, Typeable e) => MyException e
data SomeError =
    Arith ArithException
  | Async AsyncException
  | SomethingElse
discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
  case cast e of
    (Just arith) -> Arith arith
    Nothing ->
      case cast e of
        (Just async) -> Async async
        Nothing -> SomethingElse

cast :: (Typeable a, Typeable b) => a -> Maybe b
catch :: Exception e => IO a -> (e -> IO a) -> IO a
typeOf :: forall a. Typeable a => a -> TypeRep
try :: Exception e => IO a -> IO (Either e a)
throwIO :: Exception e => e -> IO a
-- for arithmetic exceptions, such as DivideByZero
-- throw :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e. Exception e => e -> a
threadDelay :: Int -> IO ()
when :: Applicative f => Bool -> f () -> f ()

instance Exception NotEven -- automatically deriving Exception

catches :: IO a -> [Handler a] -> IO a
data Handler a where
  Handler :: Exception e
          => (e -> IO a) -> Handler a
forkIO :: IO () -> IO ThreadId
throwTo :: Exception e => ThreadId -> e -> IO ()
mask_ :: IO a -> IO a
```

`MyException` doesn't appear to have a polymorphic argument in the type
 constructor, but it does in the data constructor. This is the essence of why we
 need existential quantification for exceptions---so that we can throw various
 exception types without being forced to centralize and unify them under a sum
 type. The combination of `SomeException` and the `Typeable` type class gives
 you a means of throwing different exceptions of different types and then
 catching some or all of them in a handler without wrapping them in a sum
 type.<br>
`Typeable` allows you to learn the type of a value at runtime and also to
 compare the types of two values and check that they are the same. it is
 particularly useful when you have code that needs to allow various types to be
 passed to it but needs to enforce or trigger on specific types. This is
 ordinarily unwise, but it makes sense when you're talking about exceptions.<br>
`case` gets called for us by the `fromException` function, and `fromException`
 is called by the `catch` function.<br>
[twitter-conduit][twitter_conduit], [http-client][http_client]<br>
Running code is an I/O action, so most of the time when you need to worry about
 exceptions, you'll be in `IO`. Even when they happen in pure code, exceptions
 may only be caught, or handled, in `IO`. `IO` contains the implicit contract,
 "You cannot expect this computation to succeed unconditionallly." Catching and
 handling exceptions means you could produce different results from the same
 inputs. That breaks referential transparency.<br>
You almost never want `throw` as it throws exceptions without any warning in the
 type, even `IO`.<br>
Due to nonstrictness, the bottom could've been forced before or after your
 exception handler, so you might be surprised. The exception handling mechanism
 is not for, nor should be used for, catching bottoms. Having caught an
 exception, even `SomeException`, without rethrowing an exception doesn't mean
 your program won't fail.<br>
We used `mask_` from `Control.Exception` in order to mask or delay exceptions
 thrown to our child thread until the `IO` action was complete.

* [Erin Swenson-Healey: A Beginner's Guide to Exceptions in Haskell][beginner_guide_to_exceptions]

[ext_exceptions]: https://simonmar.github.io/bib/papers/ext-exceptions.pdf
[twitter_conduit]: https://www.stackage.org/package/twitter-conduit
[http_client]: https://www.stackage.org/package/http-client
[beginner_guide_to_exceptions]: https://www.youtube.com/watch?v=PWS0Whf6-wc

## 31. Final Project

[`network`][network], `sqlite-simple` library
If you are on Windows, `withSocketsDo` is obligatory to use the sockets API in
 the `network` library.

```haskell
{-# LANGUAGE RecordWildCards #-}
wew Blah{..} = print myThing

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

```

`$ stack ghci --main-is fingerd:exe:fingerd`, Target syntax
 `packagename:comptype:compname`

[network]: https://www.stackage.org/package/network

