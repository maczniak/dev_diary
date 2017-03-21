# [Implementing Functional Languages][homepage] by Simon L. Peyton Jones and David R. Lester, Prentice Hall (1992)

`pjlester-1.11.tar.Z` | `pj-lester-book.tar.gz` and `pjlester.tar.Z`
 ([site][download_location]) | [source code with Haskell files][with_haskell]

TODO: comparison to the Haskell Core

[homepage]: https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/
[download_location]: http://www.cs.tufts.edu/~nr/cs257/archive/simon-peyton-jones/
[with_haskell]: https://github.com/logicshan/pj-lester-book

## Preface

This book gives a practical approach to understanding implementations of
 non-strict functional languages using lazy graph reduction.<br>
The Core language is designed to be as small as possible, so that it is easy to
 implement, but still rich enough to allow medern non-strict functional
 languages to be translated into it without losing efficiency.<br>
One important way in which the Core language is restrictive is in its lack of
 local function definitions. There is a well-known transformation, called
 *lambda lifting*, which turns local function definitions into global ones, thus
 enabling local function definitions to be written freely and transformed out
 later. *Full laziness* is a property of functional programs which had
 previously been seen as inseparable from lambda lifting. In Chater 6 we show
 that they are in fact quite distinct, and show how to implement full laziness
 in a separate pass from lambda lifting.<br>
We have chosen to use an existing functional language, Miranda. One of the major
 uses of functional language is for rapid prototyping, because they allow us to
 express the fundamental aspects of the prototype without getting bogged down in
 administrative detail.<br>
We focus exclusively in this book on the 'back end' of functional-language
 compilers. We make no attempt to discuss how to translate programs written in a
 fully fledged functional language, such as Miranda, into the Core language, or
 how to type-check such programs.<br>
The scope of this book is somewhat more modest, corresponding to Parts 2 and 3
 of *The implementation of functional programming languages* (1987).

## 1. The Core language

A Core program consists of a set of *supercombinator definitions*, including a
 distinguished one, `main`. The program looks quite similar to the top level of
 a Miranda script, except that no pattern matching is permitted for function
 arguments. Supercombinators with no arguments are also called *constant
 applicative forms* or CAFs and, as we shall see, often require special
 treatment in an implementation.<br>
Functions can only be defined at the top level, as supercombinators, and pattern
 matching is done only `case` expressions. In short, *the left-hand side of a*
 `let` *or* `letrec` *binding must be a simple variable*. The `let`/`letrec`
 construct is an *expression*.<br>
It is possible to transform a program involving explicit lambda abstractions
 into an equivalent one which uses only top-level supercombinator definitions.
 This process is called *lambda lifting*.<br>
A universal feature of all modern functional programming languages is the
 provision of *structured types*, often called *algebraic data types*.<br>
In particular, our goal is to avoid having data type declarations in the Core
 language altogether. The approach we take breaks into two parts:
* Use a simple, uniform representation for constructors. `Pack{`*tag,arity*`}`
* Transform pattern matching into simple `case` expressions.

In a well-typed program, objects of different type will never need to be
 distinguished at run-time, so tags only need to be unique *within a data type*.

```
depth t = case t of
                <1> n -> 0 ;
                <2> t1 t2 -> 1 + max (depth t1) (depth t2)
```

| Precedence | Associativity | Operator
|------------|---------------|---------
| 6 | Left  | Application
| 5 | Right | `*`
|   | None  | `/`
| 4 | Right | `+`
|   | None  | `-`
| 3 | None  | `==` `~=` `>` `>=` `<` `<=`
| 2 | Right | `&`
| 1 | Right | `|`

There is no special operator symbol for unary negation. Instead, the `negate`
 funcion is provided, which behaves syntactically like any normal function. The
 boolean negation operator, `not`, is handled in the same way.

```miranda
data Expr a
  =  EVar Name                -- Variables
   | ENum Int                 -- Numbers
   | EConstr Int Int          -- Constructor tag arity
   | EAp (Expr a) (Expr a)    -- Applications
   | ELet                     -- Let(rec) expressions
        IsRec                 --   boolean with True = recursive,
        [(a, Expr a)]         --   Definitions
        (Expr a)              --   Body of let(rec)
   | ECase                    -- Case expression
        (Expr a)              --   Expression to scrutinise
        [Alter a]             --   Alternatives
   | ELam [a] (Expr a)        -- Lambda abstractions
```

A binder is the name used at the binding occurrence of a variable; that is, on
 the left-hand side of a `let(rec)` definition, or in a lambda abstraction. For
 the most of the book we always use `name` in these binding positions, so we use
 a *type synonym* to define the type of `coreExpr`, which is the type we will
 normally use:

```miranda
type CoreExpr = Expr Name
type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns =  [name | (name, rhs) <- defns]
rhssOf        :: [(a,b)] -> [b]
rhssOf defns  =  [rhs  | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name
```

## 2. Template instantiation

## 3. The G-machine

## 4. TIM: the three instruction machine

## 5. A Parallel G-machine

## 6. Lambda Lifting

## A. Utilities module

## B. Example Core-language programs

