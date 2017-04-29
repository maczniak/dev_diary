# [Implementing Functional Languages][homepage] by Simon L. Peyton Jones and David R. Lester, Prentice Hall (1992)

`pjlester-1.11.tar.Z` | `pj-lester-book.tar.gz` and `pjlester.tar.Z`
 ([site][download_location]) | [source code with Haskell files][with_haskell]

TODO: Haskell version, comparison to the Haskell Core

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

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- I x = x ;
-- K  x y = x ;
-- K1 x y = y ;
-- S f g x = f x (g x) ;
-- compose f g x = f (g x) ;
-- twice f = compose f f
preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x"),
      ("K", ["x","y"], EVar "x"),
      ("K1",["x","y"], EVar "y"),
      ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f","g","x"], EAp (EVar "f")
                                      (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]
```

A pretty-printer whose cost is quadratic in the size of the program to be
 printed is clearly unacceptable, so we had better find a way around it. We can
 separate this problem into two parts: *'what operations do we want to
 perform?'*, and *'what is an efficient way to perform them?'*. In common with
 other languages, Miranda provides a way to make this distinction clear by
 introducing an *abstract data type*.<br>
In Miranda, writing a dollar sign in front of an identifier turns it into an
 infix operator, allowing us to write `iAppend` between its arguments, instead
 of in front of them. Such infix operations are right-associate.

```miranda
type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String
pLit s (tok:toks) = s == tok | [(s, toks)]
                  = otherwise | []
pLit s []         = []
pVar :: Parser String
pVar []         = []
pSat :: (String -> Bool) -> Parser String -- generalise `pLit` and `pVar`

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks,
                               (v2,toks2) <- p2 toks1]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])
pEmpty :: a -> Parser a
pOneOrMore :: Parser a -> Parser [a]
pApply :: Parser a -> (a -> b) -> Parser b
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]
```

Our parsing tools simply cannot cope with left-recursive grammars. Fortunately,
 it is usually possible to transform the grammar so that it is no longer
 left-recursive, though the resulting grammar does not then reflect the
 structure of the result we are trying to construct.<br>
The first problem with infix operators is that their precedence is implicit in
 this grammar. The standard way to make this explicit is to have several sorts
 of expression. But now the second problem arises: a parser implemented directly
 from these rules would be horribly inefficient! We want to share the parsing of
 the *expr*2 between the two productions, and this is not hard to do, by
 splitting the *expr*1 production into two. As usual, transforming the grammar
 has destroyed the structure.<br>
The grammars that can be handled efficiently by our library of parser-building
 functions are called LL(1) grammars, exactly the same class that can be dealt
 with by conventional recursive-descent parsers.

## 2. Template instantiation

This chapter introduces the simplest possible implementation of a functional
 language: a graph reducer based on *template instantiation*.<br>
Evaluation takes place by carrying out a sequence of *reductions*. A reduction
 replaces (or *updates*) a *reducible expression* in the graph by its reduced
 form. The term 'reducible expression' is often abbreviated to 'redex'.
 Evaluation is complete when there are no more redexes; we say that the 
 expression is in *normal form*. At any time there may be more than one redex in
 the expression being evaluated, so there is a choice about which one to reduce
 next. Fortunately, whatever reduction sequence we choose, we will always get
 the same answer (that is, normal form). There is one caveat: some reduction
 sequences may fail to terminate. However, if any choice of redexes makes
 evaluation terminate, then the policy of always selecting the outmost redex
 will also do so. This choice of reduction order is called *normal order
 reduction*, and it is the one we will always use.<br>
remember that a tree is just a special sort of graph.<br>
the expression `x*x` is just short for `((* x) x)`.<br>
The first step of the reduction cycle is to find the site of next reduction to
 be performed; that is, the outermost reducible function application. Follow the
 left branch of the application nodes, starting at the root, unti you get to a
 supercombinator or built-in primitive. This left-branching chain of application
 nodes is called the *spine* of the expression, and this process is called
 *unwinding* the spine. Typically a *stack* is used to remember the addresses of
 the nodes encountered on the way down. Now, check how many arguments the
 supercombinator or primitive takes and go back up that number of application
 nodes; you have now found the root of the outermost function application.<br>
If there are not enough application nodes in the spine, the expression has
 reached *weak head normal form* (WHNF). most evaluators will stop when they
 reach WHNF rather than trying to reduce the sub-expressions also. If the
 program has been type-checked, and the result is guaranteed to be a number,
 say, or a list, then this underflow check can be omitted.<br>
Notice that we have only found the root of the outermost *function application*.
 It may or may not be a *redex* as well. If the function is a supercombinator,
 then it will certainly be a redex, but if it is a primitive, then it depends on
 whether its arguments are evaluated. If a primitive requires the value of a
 currently unevaluated argument, we must evaluate the argument before the
 primitive reduction can proceed. This is conveniently done by keeping a stack
 of stacks, called the *dump*. Of course, in a real implementation we would not
 copy whole stacks! Since the 'new' stack will be finished with before the 'old'
 one is again required, the 'new' one could be built physically on top of the
 'old' one. The dump stack would then just keep track of where the boundary
 between 'new' and 'old' was.<br>
Supercombinator reduction - A supercombinator redex is reduced by replacing the
 redex with an instance of the supercombinator body, substituting pointers to
 the actual arguments for corresponding occurrences of the formal parameters.
 Notice that the arguments are not copied; rather, by the device of using
 pointers to them, they are shared.<br>
ordinary expressions describe trees; `let` expressions describe acyclic graphs;
 and `letrec` expressions describe cyclic graphs.<br>
After performing a reduction, we must update the root of the redex with the
 result, so that if the redex is shared, the reduction is only done once. This
 updating is the essence of *lazy evaluation*.<br>
The easiest way out of this dilemma is to add a new sort of graph node, an
 *indirection node*, which will be depicted as a `#` sign. An indirection node
 can be used to update the root of a redex to point to the result of the
 reduction.<br>
Some supercombinators have no arguments; they are called *constant applicative
 forms*, or CAFs. The interesting thing about CAFs is that *the supercombinator
 itself is a redex*.<br>
An invariant of a sequence of states is a predicate which is true of all of the
 states.<br>
The state of the template instantiation graph reduction machine is a quadruple:
 *(stack, dump (unused in the Mark 1 machine), heap (collection of tagged
 nodes), globals (address of heap node))*

## 3. The G-machine

## 4. TIM: the three instruction machine

## 5. A Parallel G-machine

## 6. Lambda Lifting

## A. Utilities module

## B. Example Core-language programs

