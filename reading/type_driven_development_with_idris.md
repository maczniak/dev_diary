# [Type-Driven Development with Idris][homepage] by Edwin Brady, Manning (2017)

treat a type as a plan for a program

[homepage]: https://www.manning.com/books/type-driven-development-with-idris

## Part 1: Introduction

### 1. Overview

The difference is that unlike tests, which can usually only be used to show the
 *presence* of errors, types (used appropriately) can show their *absence*. /
 Even if two processes are correctly coordinated when we run a test once,
 there's no guarantee they'll be correctly coordinated when we next run the
 test. On the other hand, if we can express the coordination between processes
 in *types*, we can be sure that a concurrent program which type checks has
 properly coordinated processes.<br>
Idris is designed from the beginning to support Type-driven Development. A
 prototype implementation first appeared in 2008, with development of the
 current implementation beginning in 2011.<br>
types are a *firsr-class* language construct. assumptions to be made explicit
 and checkable by the compiler. if desired, program behaviour to be formally
 stated and proven correct.<br>
If we try to implement a program which dispenses cash without validating a PIN,
 the program *won't compile*.<br>
*dependent types* allow us to express detailed properties of our programs. A
 *dependent type* is a type which is calculated from (that is, *depends on*)
 some other values.<br>
`List` encodes generic lists with no explicit length, and `Vect` (short for
 "vector") encodes lists with the length explicitly in the type.<br>
In pure functional programming in general, and Idris in particular, we solve
 this problem by writing functions which *describe* side effects, rather than
 functions which *execute* them, and deferring the details of execution to the
 compiler and run-time system.<br>
Total functions will return a value of the given type in finite time. More
 precisely, a total function is guaranteed to produce a *finite prefix* of a
 potentially infinite result. If a partial function does not crash, or enter an
 infinite loop, then the value it returns will be the given type. In most modern
 languages, we must assume that functions are partial and can therefore only
 make the latter, weaker, claim. Idris checks whether functions are total, and
 we can therefore often make the former, stronger, claim. A total function with
 that type is then *guaranteed* by the type checker to perform those operations
 as precisely as the type requires. Although Idris can't solve the Halting
 Problem in general, Idris can identify a large class of functions which are
 definitely total.<br>
I recommend Atom, because it has a mode for interactive editing of Idris programs.<br>
`:exec`, `:r` (reload)<br>
`idris Hello.idr -o Hello`<br>
when Idris encounters the hole `?greeting`, it creates a new name `greeting`
 which has a type but no definition.<br>
`cast` function<br>
In most statically typed languages, there are restrictions on where types can be
 used, and there is a strict syntactic separation between types and values. In
 Idris, there are no such restrictions, and types are first-class. This means
 that we can write functions which compute types, and allows the return type of
 a function to differ depending on the input *value* to a function. In Idris, we
 can write a function similar to `printf` directly, by taking advantage of types
 as a first-class construct.

* A database schema determines the allowed forms of queries on a database.
* A form on a web page determines the number and type of inputs expected.
* A network protocol description determines the types of values which may be
  sent or received over a network.

```idris
Idris> :t Integer
Integer : Type
Idris> :t Type
Type : Type 1

*Hello> :t greeting
--------------------------------------
greeting : String
*Hello> greeting
?greeting : String
```

### 2. Getting Started with Idris

If you know Haskell, the most important difference is that Idris doesn't use
 lazy evaluation by default.<br>
String is a primitive type, unlike some other languages (notably Haskell, where
 a string is represented as a list of characters).<br>
repl is a function that repeatedly displays a prompt, reads a String from the
 console, and then displays the result of running a function on that String.<br>
`Nat` - unbounded unsigned integer type (Idris will treat a number as `Integer`
 by default)<br>
`it`, `:let`, `the <type> <expression>` (Prelude function)<br>
Idris supports casting between all of the primitive types, and it's possible to
 add user-defined casts.<br>
Idris, by default, will evaluate the *innermost* expression first. In other
 words, it will evaluate function arguments before function definitions. Idris
 supports lazy evaluation using explicit types.<br>
`getStringOrInt : (x : Bool) -> StringOrInt x`<br>
Functions in Idris *must* have an explicit type declaration. Some other
 functional languages, most notably Haskell and ML, allow programmers to omit
 type declarations and have the compiler *infer* the type. In a language with
 first-class types, however, this generally turns out to be impossible. In any
 case, it's undesirable to omit type declarations in type-driven development.
 Our philosophy is to use types to help us write programs, rather than to use
 programs to help us infer types!<br>
Note that I'm careful to call these *variables*, rather than *type variables*.
 This is because, with dependent types, variables in types don’t necessarily
 stand for only types.<br>

```idris
the : (ty : Type) -> ty -> ty
the ty x = x

Shape : Type
rotate : Shape -> Shape -- type declarations with no definitions
```

Tuples can have any number of components, including zero. Internally, all tuples
 other than the empty tuple are stored as nested pairs. That is, if you write
 `(1, 2, 3, 4)`, Idris will treat this in the same way as `(1, (2, (3, 4)))`.
 The REPL will always display a tuple in the non-nested form.<br>
Values can be converted between compatible types using the `cast` function, and
 can be given explicit types with the `the` function.

```idris
Idris> []
(input):Can't infer argument elem to []
Idris> the (List Int) []
[] : List Int

-- Idris allows function names to be "overloaded" to work on multiple types.
*lists> :t length
Prelude.List.length : List a -> Nat
Prelude.Strings.length : Strig -> Nat

export
average : String -> Double
average str = ...
```

`[1..5]`, `[5,4..1]`<br>
Modules themselves can be combined into *packages* and distributed separately.
 Technically the Prelude is defined in a module called `Prelude`, which itself
 imports several other modules, and which is part of a package called
 `prelude`.<br>
all of the types in the Prelude support the `Show` interface.

```idris
-- single-line comments
{- multi-line nested comments -}

-- documentation comments below

||| Calculate the average length of words in a string.
||| @str a string containing words separated by whitespace.
average : (str : String) -> Double
average str = ...

*Average> :doc average
Main.average : (str : String) -> Double
    Calculate the average length of words in a string.
    Arguments:
        str : String  -- a string containing words separated by whitespace.

    The function is Total
```

## Part 2: Core Idris

### 3. Intractive Development with Types

"type, define, refine" process<br>
Atom provides an interactive editing mode that communicates with a running Idris
 system and uses types to help direct function development. it's reasonably
 straightforward to add Idris support to other text editors, and similar editing
 modes currently exist for Emacs and Vim.

Shortcut | Command | Description
---------|---------|------------
Ctrl-Alt-A | Add definition | Adds a skeleton definition for the name under the cursor
Ctrl-Alt-C | Case split | Splits a definition into pattern matching clauses for the name under the cursor
Ctrl-Alt-D | Documentation | Displays documentation for the name under the cursor
Ctrl-Alt-L | Lift hole | Lifts a hole to the top level as a new function declaration
Ctrl-Alt-M | Match | Replaces a hole with a `case` expression that matches on an intermediate result
Ctrl-Alt-R | Reload | Reloads and type-checks the current buffer
Ctrl-Alt-S | Search | Searches for an expression that satisfies the type of the hole name under the cursor
Ctrl-Alt-T | Typecheck name | Displays the type of the name under the cursor

```idris
*WordLength> :total allLengths
Main.allLengths is Total
```

Thanks to the halting problem, Idris can’t decide in general whether a function
 is total, but by analyzing a function's syntax it can decide that a function is
 total in many specific cases. We'll discuss totality checking in much more
 detail in chapters 10 and 11. For the moment, it's sufficient to know that
 Idris will consider a function total if:

* It has clauses that cover all possible well-typed inputs
* All recursive calls converge on a *base case*

As you'll see in chapter 11 in particular, the definition of totality also
 allows interactive programs that run indefinitely, such as servers and
 interactive loops, provided that they continue to produce intermediate results
 in finite time.

`Nat` - `Z : Nat` zero or `S : Nat -> Nat` successor

(mutually defined functions) Idris processes input files from top to bottom and
 requires types and functions to be defined before use. This is necessary due to
 complications that arise with dependent types, where the *definition* of a
 function can affect a type.

```idris
mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k
```

(`import Data.Vect`) The Prelude is defined in a package called `prelude`, from
 which all modules are imported automatically. Idris programs also have access
 to a package called `base`, which defines several commonly useful data
 structures and algorithms including Vect, but from which modules must be
 imported explicitly.<br>
`Vect` - `Nil : Vect 0 a` or `(::) : (x : a) -> (xs: Vect k a) -> Vect (S k) a`<br>
this syntactic sugar (`[1, 2, 3]`) applies to any data type with constructors
 called `Nil` and `::`.

```idris
Idris> the (List _) ["Hello", "There"]
["Hello", "There"] : List String
Idris> the (Vect _ _) ["Hello", "There"]
["Hello", "There"] : Vect 2 String
```

The underscore (`_`) indicates to Idris that you would like it to infer a value
 for that argument. You can use `_` in an expression whenever there's only one
 valid value that would stand for that expression.<br>
(`total allLengths : ...`) you can annotate in the source code that a function
 must be total. then Idris will report an error if its definition isn't total.

### 4. User Defined Data Types

### 5. Intractive Programs: Input and Output Processing

### 6. Programming with First Class Types

### 7. Interfaces: Using Constrained Generic Types

### 8. Equality: Expressing Relationships Between Data

### 9. Predicates: Expressing Assumptions and Contracts in Types

### 10. Views: Extending Pattern Matching

### 11. Streams and Processes: Working with Infinite Data

## Part 3: Idris and the Real World

### 12. Writing Programs with State

### 13. State Machines: Verifying Protocols in Types

### 14. Dependent State Machines: Handling Feedback and Errors

### 15. Type-safe Concurrent Programming

## Appendixes

### A. Installing Idris & Editor Types

### B. Interactive Editing Commands

### C. REPL Commands

### D. The Packaging System

