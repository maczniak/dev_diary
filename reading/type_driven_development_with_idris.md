# [Type-Driven Development with Idris][homepage] by Edwin Brady, Manning (2017)

[Idris Hackers][idris_hackers] repo ([demos][idris_hackers_demos],
 [software-foundations][idris_hackers_software_foundations]),
 [Pruviloj][pruviloj] (proof automation and program construction toolkit,
 tactics)

[source code][source_code]<br>
treat a type as a plan for a program

[homepage]: https://www.manning.com/books/type-driven-development-with-idris
[idris_hackers]: https://github.com/idris-hackers
[idris_hackers_demos]: https://github.com/idris-hackers/idris-demos
[idris_hackers_software_foundations]: https://github.com/idris-hackers/software-foundations
[pruviloj]: https://github.com/idris-lang/Idris-dev/tree/master/libs/pruviloj
[source_code]: https://manning-content.s3.amazonaws.com/download/6/1b63c2e-89cf-47df-90a7-0520262c1d1d/source-code.zip

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
`repl : String -> (String -> String) -> IO ()`<br>
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
 must be total. then Idris will report an error if its definition isn't
 total.<br>
These type-level variables aren't declared anywhere else. Because types are
 first class, type-level variables can also be brought into scope and used in
 definitions. These type-level variables are referred to as *implicit* arguments
 to the functions.

```idris
append : {elem : Type} -> {n : Nat} -> {m : Nat} ->
         Vect n elem -> Vect m elem -> Vect (n + m) elem
-- Here the implicit arguments have been explicitly *bound* in the type.
-- Otherwise, the names elem, n, and m are called *unbound* implicits.

length : Vect n elem -> Nat
length [] = Z
length (x :: xs) = 1 + length xs

length : Vect n elem -> Nat
length {n} xs = n
-- The notation {n} in a pattern brings the implicit argument n into scope,
--  allowing us to use it directly.

append {elem = Char} {n = 2} {m = 3} -- partial apply
-- or, the (Vect 5 Char) append
-- append : Vect 2 Char -> Vect 3 Char -> Vect 5 Char
```

(type and argument erasure) the Idris compiler will analyze a program before
 compiling it so that any arguments that are only used for type checking won't
 be present at runtime.

### 4. User Defined Data Types

* Enumerated types - Types defined by giving the possible values directly
* Union types - Enumerated types that carry additional data with each value
* Recursive types - Union types that are defined in terms of themselves
* Generic types - Types that are parametrized over some other types
* Dependent types - Types that are computed from some other value

By convention, I'll use an initial capital letter for both *type constructors*
 and *data constructors*, and an initial lowercase letter for functions. There's
 no requirement to do so.

```idris
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

data Shape : Type where -- more general and flexible
     Triangle : Double -> Double -> Shape
     Rectangle : Double -> Double -> Shape
     Circle : Double -> Shape

-- instead of x, y, z
%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

data PowerSource = Petrol | Pedal
data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
-- Dependent data types are sometimes referred to as *families* of types,
--  because you're definig multiple related types at the same time.
-- The power source is an *index* of the Vehicle family.
--  The index tells you exactly which Vehicle type you mean.

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
```

`Vect` defines a family of types, and we say that a `Vect` is *indexd* by its
 length (An *index* may change across a structure.) and *parameterized* by an
 element type (A *parameter* is unchanged across the entire structure). the
 specific value of a *parameter* can play no part in a function's definition.

```idris
-- in Data.Vect

-- *finitely bounded*
-- an unsigned number that has a non-inclusive *upper bound* of n
index : Fin n -> Vect n a -> a

integerToFin : Integer -> (n : Nat) -> Maybe (Fin n)
*Data/Vect> integerToFin 2 5
Just (FS (FS FZ)) : Maybe (Fin 5)

Prelude.Interactive.replWith : (state : a) ->
    (prompt : String) ->
    (onInput : a -> String -> Maybe (String, a)) -> IO ()
```

### 5. Intractive Programs: Input and Output Processing

```idris
Idris> putStrLn (show (47 * 2))
io_bid (prim_write "94\n") (\__bindx => io_return ()) : IO ()
Idris> :exec putStrLn (47 * 2)
94

*Hello> :c hello -- compile
```

`let x = expression` assigns the result of the *evaluation* of an `expression`
 to a variable. `x <- action` assigns the result of the execution of an action
 to a variable.

```idris
-- concise notation for pattern-matching on the result of an interactive action
readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do Just num1_ok <- readNumber | Nothing => pure Nothing
     Just num2_ok <- readNumber | Nothing => pure Nothing
     pure (Just (num1_ok, num2_ok))
```

Totality checking is based on *evaluation*, not execution. The result of
 totality checking an `IO` program therefore tells you whether Idris will
 produce a finite sequence of actions, but nothing about the runtime behavior of
 those actions.<br>
A *dependent pair* is a more expressive form of a tuple, where the *type* of the
 second element in a pair can be computed from the *value* of the first element.
 `anyVect : (n : Nat ** Vect n String)`<br>
`Data.Vect.exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)`

### 6. Programming with First Class Types

Type-level functions exist at *compile time only*. Only functions that are total
 will be evaluated at the type level. A function that isn't total may not
 terminate, or may not cover all possible inputs. Therefore, to ensure that
 type-checking itself terminates, functions that are not total are treated as
 *constants* at the type elvel, and don't evaluate further.<br>

```idris
record DataStore where
       constructor MkData -- There can be *only one* constructor.
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)
-- The fields give rise to *projection* functions, automatically generated from
--  the types of the fields.
```

### 7. Interfaces: Using Constrained Generic Types

Interfaces in Idris are similiar to type classes in Haskell and are often used
 in the same way, though there are some differences. The most important are,
 first, that interfaces in Idris can be parameterized by values of *any* type,
 and are not limited to types or type constructures, and second, interfaces in
 Idris can have multiple implementations, though we won't go into the details in
 this chatper.

```idris
interface Cast from to where
    cast : (orig : from) -> to

-- Gives the type of f explicitly, because it's not Type
interface Functor (f : Type -> Type) where
    map : (func : a -> b) -> f a -> f b

Functor List where
    map func []      = []
    map func (x::xs) = func x :: map func xs
```

### 8. Equality: Expressing Relationships Between Data

In this chapter, we'll look at a simple property, using types to express
 guarantees that values are equal. You'll also see how to express guarantees
 that values are *not* equal. Properties such as equality and inequality are
 sometimes required when you're defining more complex functions with dependent
 types, where the relationship between values might not be immediately obvious
 to Idris. For example, as you'll see when we define `reverse` on vectors, the
 input and output vector lengths must be the same, so we'll need to explain to
 the compiler why the length is preserved.<br>
the type of `==` isn't informative enough to guarantee that `m` and `len` are
 equal, even if it returns `True`. Instead, you'll need to create a more precise
 type for the equality test, where the type guarantees that a comparison between
 two inputs can only be successful if the inputs really are identical.<br>
The type `EqNat 3 4` is an *empty type*, meaning that there are *no* values of
 that type.<br>
We'll begin by writing a `checkEqNat` function that either returns a proof that
 its inputs are the same, in the form of `EqNat`, or `Nothing` if the inputs are
 different.<br>
`sameS : (eq : EqNat k j) -> EqNat (S k) (S j)` is a function that takes
 evidence that `k` and `j` are equal, and returns evidence that `S k` and `S j`
 are equal. By expressing equality between numbers as a dependent data type,
 `EqNat`, you're able to write functions like `sameS` that take an instance of
 `EqNat` as an input and manipulate them, essentially deducing additional
 information about equalities. In practice, you'll rarely need to manipulate
 equalities much more complex than the implementation of `sameS`.<br>
Idris provides a generic equality type. This is built into Idris's syntax, so
 you can't define this yourself (because `=` is a reserved symbol), but
 conceptually it would be defined as follows.

```idris
data (=) : a -> b -> Type where -- built-in equality type
     Refl : x = x
     -- The name `Refl` is short for "reflexive," a mathematical term that
     --  (informally) means that a value is equal to itself.

Idris> the ("Hello" = "Hello") Refl
Refl : "Hello" = "Hello"

Idris> the (2 + 2 = 4) Refl
Refl : 4 = 4

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just prf => Just (cong prf)

-- cong is a generic version of sameS
cong : {func : a -> b} -> x = y -> func x = func y
```

note: `(foo : type)` (in the type level), `the (type) foo` (in the term level)

### 9. Predicates: Expressing Assumptions and Contracts in Types

### 10. Views: Extending Pattern Matching

### 11. Streams and Processes: Working with Infinite Data

## Part 3: Idris and the Real World

### 12. Writing Programs with State

### 13. State Machines: Verifying Protocols in Types

### 14. Dependent State Machines: Handling Feedback and Errors

### 15. Type-safe Concurrent Programming

## Appendixes

### A. Installing Idris & Editor Modes

[installing and editor modes][installing_and_editor_modes],
 [Idris mode for Atom][idris_mode_for_atom]

[installing_and_editor_modes]: http://www.idris-lang.org/download/
[idris_mode_for_atom]: https://atom.io/packages/language-idris

### B. Interactive Editing Commands

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
Ctrl-Alt-W | With block insertion | Add a with block after the current line, containing a new pattern matching clause with an extra argument

### C. REPL Commands

### D. The Packaging System

