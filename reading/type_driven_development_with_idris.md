# [Type-Driven Development with Idris][homepage] by Edwin Brady, Manning (2017)

[Idris Hackers][idris_hackers] repo ([demos][idris_hackers_demos],
 [software-foundations][idris_hackers_software_foundations]),
 [Pruviloj][pruviloj] (proof automation and program construction toolkit,
 tactics)

[source code with answers to exercises][source_code]<br>
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
 potentially infinite result (in finite time). If a partial function does not
 crash, or enter an infinite loop, then the value it returns will be the given
 type. In most modern languages, we must assume that functions are partial and
 can therefore only make the latter, weaker, claim. Idris checks whether
 functions are total, and we can therefore often make the former, stronger,
 claim. A total function with that type is then *guaranteed* by the type checker
 to perform those operations as precisely as the type requires. Although Idris
 can't solve the Halting Problem in general, Idris can identify a large class of
 functions which are definitely total.<br>
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
  do Just num1_ok <- readNumber | Nothing => pure Nothing -- terminate here!
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
 *constants* at the type level, and don't evaluate further.<br>

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

Idris> Refl {x = 94}
Refl : 94 = 94

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

note: `(foo : type)` (in the type level), `the (type) foo` (or
 `Refl {x = 94}`-like in the term level)

```idris
Idris> \k, elem => Vect (1 + k) elem
\k => \elem => Vect (S k) elem : Nat -> Type -> Type

Idris> \k, elem => Vect (k + 1) elem
\k => \elem => Vect (plus k 1) elem : Nat -> Type -> Type
-- Because `plus` is defined by pattern-matching on its first argument, Idris
--  can't evaluate `plus k 1` any further.

Idris> :printdef plus
plus : Nat -> Nat -> Nat
plus 0 right = right
plus (S left) right = S (plus left right)

plusCommutative : (left : Nat) -> (right : Nat) -> left + right = right + left

Idris> :t \k => plusCommutative 1 k
\k => plusCommutative 1 k : (k : Nat) -> S k = plus k 1

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) -- to bring `k` into scope
        = let result = myReverse xs ++ [x] in
              rewrite plusCommutative 1 k in result
              -- replace the right side with the left side!

-- delegating proofs
-- keep the relevant *computation* part separate from the details of the proof
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} result = rewrite plusCommutative 1 k in result

append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs
append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs
append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys
-- append_nil stands for a proof that it's valid to return ys here.
append (x :: xs) ys = append_xs (x :: append xs ys)
-- append_xs stands for a proof that it's valid to return x :: append xs ys here.

plusZeroRightNeutral : (left : Nat) -> left + 0 = left -- defined in the Prelude
plusSuccRightSucc : (left : Nat) -> (right : Nat) ->
                    S (left + right) = left + S right
sym : left = right -> right = left
```

When you construct a value in a type, you're effectively giving evidence that an
 element of that type exists. To show that two values `x` and `y` are *not*
 equal, you need to able to give evidence that an element of the type `x = y`
 *can't* exist. In this section you'll see how to use the *empty type*, `Void`,
 to express that something is impossible. If a function returns a value of type
 `Void`, that can only mean that it's impossible to construct values  of its
 inputs (or, logically, that the types of its inputs express a *contradiction*.)
 We'll use `Void` to express guarantees in types that values *can't* be equal,
 and then use it to use a more precise type (`Dec`) for `checkEqNat`, which
 *guarantees* that
* If its inputs are equal, it will produce a proof that they are equal
* If its inputs are not equal, it will produe a proof that they are not equal

Just as you can use `=` to write functions that express facts about how
 functions *do* behave, you can use `Void` to express facts about how functions
 *don't* behave.

```idris
data Void : Type where

twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible
-- twoPlusTwoNotFive is total.
-- loop : Void, loop = loop, is not total.

valueNotSuc : (x : Natt) -> x = S x -> Void
valueNotSuc _ Refl impossible
```

`void : Void -> a` (in the Prelude). If you were able to provide a value of the
 empty type, you'd be able to produce a value of *any* type. In other words, if
 you have a proof that an impossible value has happened, you can do
 anything.<br>
if you know that something *can't* happen, you can use this knowledge to express
 limitations about what *can* happen.<br>
you'd need a way of stating that for any pair of numbers, `num1` and `num2`,
 you'll always be able to produce either a proof that they're equal (of type
 `num1 = num2`) or a proof that they're not equal (of type
 `num1 = num2 -> Void`). That is, you'd like to state that checking whether
 `num1 = num2` is a *decidable* property. A property of some values is
 *decidable* if you can always say whether the property holds or not for
 specific values.

```idris
data Dec : (prop : Type) -> Type where
     Yes : (prf : prop) -> Dec prop
     No  : (contra : prop -> Void) -> Dec prop

Idris> the (Dec (2 + 2 = 4)) (Yes Refl)
Yes Refl : Dec (4 = 4)

Idris> the (Dec (2 + 2 = 5)) (No twoPlusTwoNotFive)
No twoPlusTwoNotFive : Dec (4 = 5)

zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSuc
checkEqNat (S k) Z = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              No contra => No (noRec contra)
                              Yes prf => Yes (cong prf)

interface DecEq ty where -- use this instead of checkEqNat
    decEq : (val1 : ty) -> (val2 : ty) -> Dec (val1 = val2)
```

### 9. Predicates: Expressing Assumptions and Contracts in Types

Dependent type like `EqNat` and `=`, which you saw in the previous chapter, are
 used entirely for describing relationships between data. These types are often
 referred to as *predicates*, which are data types that exist entirely to
 describe a property of some data.<br>
By expressing relationships between data in types, you can be explicit about the
 *assumptions* you're making about the inputs to a function, and have those
 assumptions checked by the type checker when those functions are called. You
 can even think of these assumptions as expressing compile time *contracts* that
 other arguments must satisfy before anything can call the function. if a
 function call violates a contract, the program will not compile.

```idris
data Elem : a -> Vect k a -> Type where -- in Data.Vect
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

removeElem : (value : a) -> (xs : Vect (S n) a -> (prf : Elem value xs) ->
             Vect n a

absurd : Uninhabited t => (h : t) -> a -- in the Prelude
absurd h = void (uninhabited h)

interface Uninhabited t where
  -- can be implemented for any type that has no values
  uninhabited : t -> Void

removeElem_auto : (value : a) -> (xs : Vect (S n) a) ->
                  {auto prf : Elem value xs} -> Vect n a -- auto-implicit argument
removeElem_auto value xs {prf} = removeElem value xs prf
-- Unlike ordinary implicits, Idris will search for a value for an auto implicit
--  using the same machinery it uses for expression search with Ctrl-Alt-S.

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notThere : Elem value xs -> Void) ->
              (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs)
   = case decEq value x of
          Yes Refl => Yes Here
          No notHere => case isElem value xs of
                             Yes prf => Yes (There prf)
                             No notThere => No (notInTail notThere notHere)
```

instead of using `getLine`, write a `readGuess` function that returns the user's
 guess, along with an instance of a predicate that guarantees the user's guess
 is a valid input:

```idris
readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               x <- getLine
               case isValidString (toUpper x) of
                    Yes prf => pure (_ ** prf)
                    No contra => do putStrLn "Invalid guess"
                                    readGuess

-- "letters"
data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
     mkWordState : (word : String) -> (missing : Vector letters Char)
                   -> WordState guesses_remaining letters
```

A value of type `WordState guesses letters` holds *concrete* information about
 system state (the exact word to be guessed and exactly which letters are still
 missing). The type itself expresses the *abstract* information about the game
 state (guesses remaining and number of missing letters).

### 10. Views: Extending Pattern Matching

in general there is no way for Idris to automatically deduce the inputs to an
 arbitrary function fro its output. We can, however, extend the forms of
 patterns we can use by defining informative data types, called *views*. Views
 are dependent types which are parameterised by the data we'd like to match, and
 give us new ways of observing that data.

```idris
describeHelper : (input : List Int) -> ListLast input -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x)
        = "Non-empty, initial portion = " ++ show xs

-- The total flag means Idris will report an error if listLast is not fully defined.
total
listLast : (xs : List a) -> ListLast xs
```

We call `listLast` the *covering function* of the view `ListLast`. A covering
 function of a view describes how to convert a value into a view of that value.
 By convention, we give covering functions the same name as the view type, but
 with an initial lower case letter. Since dependent pattern matching in this way
 is a common programming idiom in Idris, there is a construct for expressing
 extended pattern matching more concisely: the `with` construct. The `with`
 construct allows us to use views directly, without defining an intermediate
 (helper) function.

```idris
describeListEnd : List Int -> String
describeListEnd input with (listLast input)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (xs ++ [x]) | (NonEmpty xs x)
          = "Non-empty, initial portion = " ++ show xs
```

There is one important difference between `with` and `case`: `with` introduces a
 new pattern to match on the *left hand side* of a definition.<br>
We can't just use any expression in a pattern, in general, because it is not
 possible in general to decide what the inputs to a function must be given only
 its result. Idris therefore allows patterns only when it *can* deduce those
 inputs.

if Idris can determine that a function is total, we have a strong guarantee that
 the type accurately describes what the function will do. If not, we only have a
 guarantee that the function will produce a value of the given type if it
 terminates without crashing. Furthermore, if Idris can't determine that a
 function is total, it can't determine that any functions which call it are
 total either. Briefly, Idris tries to decide whether a function is total by
 checking two things:
* There must be patterns for all possible well-typed inputs.
* When there is a recursive call (or a sequence of mutually recursive calls),
  there must be a *decreasing* argument which converges towards a base case.

To determine which arguments are decreasing, Idris looks at the patterns for the
 inputs in a definition. If a pattern is in the form of a data constructor,
 Idris considers the arguments in that pattern to be *smaller* than the input.
 This restriction is to keep the concept of *decreasing argument* as simple as
 possible for Idris to check. In general, Idris can't tell whether the inputs to
 a function will always be smaller than the result. we can work around this
 restriction by defining *recursive* views.

`Prelude.List.merge : Ord a => List a -> List a -> List a`<br>
In principle, we could make the type of `SplitPair` more precise, and carry a
 proof that `lefts` and `rights` differ in size by at most one. In fact, the
 Idris library module `Data.List.Views` exports such a view, called
 `SplitBalanced`.

```idris
data SnocList : List a -> Type where
     Empty : SnocLit []
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])
     -- takes a *recursive* argument of type `SnocList xs`

snocList : (xs -> List a) -> SnocList xs

snocListHelp : (snoc : SnocList input) -> (rest : List a) ->
               SnocList (input ++ rest)
snocListHelp {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp {input} snoc (x :: xs)
        = rewrite appendAssociative input [x] xs in
                  snocListHelp (Snoc snoc {x}) xs
snocList : (xs : List a) -> SnocList xs -- reusable
snocList xs = snocListHelp Empty xs

appendNilRightNeutral : (l : List a) -> l ++ [] = l
appendAssociative : (l : List a) -> (c : List a) -> (r : List a) ->
                    l ++ (c ++ r) = (l ++ c) ++ r

myReverse : List a -> List a
myReverse input with (snocList input)
  myReverse [] | Empty = []
  myRevere (xs ++ [x]) | (Snoc rec) = x :: myReverse xs | rec
  -- bypassing the construction of `snocList input` and using `rec` directly
```

The call to `myReverse xs | rec` recursively calls `myReverse`, but bypassing
 the construction of `snocList input` and using `rec` directly. The resulting
 definition is total. This also has the effect of making `myReverse` run in
 linear time. In practice, when we use the `with` construct Idris introduces a
 new function definition for the body of the `with` block.

```idris
-- nested `with` blocks
isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc rec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc rec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc rec) | (Snoc z)
               = if x == y then isSuffix xs ys | xsrec | ysrec
                           else False

-- SnocList, SplitRec, ... in Data.List/Vect/Nat/String.Views module
data SplitRec : List a -> Type where
     SplitRecNil : SplitRec []
     SplitRecOne : SplitRec [x]
     SplitRecPair : (lrec : Lazy (SplitRec lefts)) ->
                    (rrec : Lazy (SplitRec rights)) ->
                    SplitRec (lefts ++ rights)

Delay : a -> Lazy a
Force : Lazy a -> a
-- When type checking, Idris will insert applications of `Delay` and `Force`
--  implicitly, as required.

data HalfRec : Nat -> Type where
     HalfRecZ : HalfRec Z
     HalfRecEven : {n : Nat} -> (rec : Lazy (HalfRec n)) -> HalfRec (n + n)
     HalfRecOdd : {n : Nat} -> (rec : Lazy (HalfRec n)) -> HalfRec (S (n + n))

data VList : List a -> Type where
     VNil : VList []
     VOne : Vlist [x]
     VCons : {x : a} -> {y : a} -> .{xs : List a} ->
             (rec : VList xs) -> Vlist (x :: xs ++ [y])
     -- dots mark fields that are not intended to be used at runtime.
     --  free (unbound) implicits are dotted by default(, and can be omitted).
```

we find out how a value was constructed by looking at the *view*, rather than by
 looking directly at the *constructors* of that value. one use of views in
 practice is to allow us to hide the *representation* of data in a module, while
 still allowing interactive type-driven development of functions which use that
 data, by case splitting on a view of that data.<br>
The idea of views was proposed by [Philip Wadler][philip_wadler] for Haskell in
 1987, in his paper "Views: a way for pattern matching to cohabit with data
 abstraction." Views as a programming idiom, using depedent types and a notation
 similar to the `with` notation in Idris, was later proposed by
 [Conor McBride][conor_mcbride] and James McKinna in their 2004 paper "The view
 from the left."<br>
export modifier - `private` (default), `export` (except the definition),
 `public export`<br>
Exporting a function's definition as well as its type (via `public export`) is
 important if we want to use the function's behaviour in a type.

[philip_wadler]: http://homepages.inf.ed.ac.uk/wadler/
[conor_mcbride]: http://strictlypositive.org/

## Part 3: Idris and the Real World

totality is about more than termination. A function is also total if it produces
 some portion of a potentially infinite result, which means you can write
 interactive systems such as servers and read-eval-print loops that run forever,
 but which are nevertheless total.

### 11. Streams and Processes: Working with Infinite Data

Idris checks that functions that generate streams are guaranteed to be
 *productive*, so that any function that consumes the output of a stream
 generator will always have data to process. What you really need to know about
 `countFrom` is not that it always *terminates*, but rather that it will always
 produce as many numbers as you need.

```idris
data InfList : Type -> Type where
     -- There's no Nil constructor, so no end to the list.
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

%name InfList xs, ys, zs

-- an idea from the Agda programming language,
--  see Nils Anders Danielsson "Total Parser Combinators" (2010)
Inf : Type -> Type
Delay : (value: ty) -> Inf ty
Force : (computation : Inf ty) -> ty

countFrom : Integer -> InfList Integer -- total
countFrom x = x :: Delay (countFrom (x + 1))
```

You may hear Idris programmers referring to functions such as `countFrom` as
 *corecursive* rather than *recursive*, and infinite lists as *codata* rather
 than *data*. The distinction between data and codata is that data is *finite*
 and is intended to be *consumed*, whereas codata is potentially *infinite* and
 is intended to be *produced*. Whereas recursion operates by taking data and
 breaking it down toward a base case, corecursion operates by starting at a base
 case and building up codata.<br>
Functions that produce infinite data can be used as components of terminating
 functions, provided they'll always produce a new piece of data on request. In
 the case of `countFrom`, it will always produce a new `Integer` before making a
 delayed recursive call. A total function is a function that, for all well-typed
 inputs, does one of the following:
* Terminates with a well-typed result
* Produces a non-empty finite prefix of a well-typed infinite result in finite
  time

We can describe total functions as either *terminating* or *productive*. The
 halting problem is the difficulty of determining whether a specific program
 terminates or not, and, thanks to Alan Turning, we know that it's a impossible
 in general to write a program that solves the halting problem. In other words,
 Idris can't determine whether one of these conditions holds for *all* total
 functions. Instead, it makes a conservative approximation by analyzing a
 function's syntax. Idris considers a function to be total if there are patterns
 that cover all well-typed inputs, and it can determine that one of the
 following conditions holds:
* When there's a recursive call (or a sequence of mutually recursive calls),
  there's a decreasing argument that converges toward a base case.
* When there's a recursive call as an argument to `Delay`, the delayed call will
  always be an argument to a data constructor (or sequence of nested data
  constructors) after evaluation, for all inputs.

In pratice, you can omit calls to `Delay` and `Force` and let Idris insert them
 where required. If, during type checking, Idris encounters a value of type
 `Inf ty` when it requires a value of type `ty`, it will add an implicit call to
 `Force`. Simlarly, if it encounters a `ty` when it requires an `Inf ty`, it
 will add an implicit call to `Delay`.<br>
You can therefore treat `Inf` as an annotation on a type, mark the parts of a
 data structure that may be infinite, and let the Idris type checker manage the
 details of when computations must be delayed or forced.

```idris
data Stream : Type -> Type where
     (::) : (value : elem) -> Inf (Stream elem) -> Stream elem

repeat : elem -> Stream elem
take : (n : Nat) -> (xs : Stream elem) -> List elem
iterate : (f : elem -> elem) -> (x : elem) -> Stream elem
// syntactic sugar for stream generation - [1..], [1,3..]
cycle : (xs : List a) -> {auto ok : NonEmpty xs} -> Stream a

data Divides : Int -> (d : Int) -> Type where -- in Data.Primitives.Views
     DivByZero : Int.Divides x 0
     DivBy : (prf : rem >= 0 && rem < d = True) ->
             Int.Divides ((d * div) + rem) d

divides : (val : Int) -> (d : Int) -> Divides val d
```

linear congruential generator

```idris
data InfIO : Type where -- represent infinite sequences of `IO` actions
     Do : IO a -> (a -> Inf InfIO) -> InfIO
     -- you may want the value produced by the first action
     --  to influence the rest of a computation

run : InfIO -> IO () -- convert `InfIO` programs to `IO` actions
run (Do action cont) = do res <- action
                          run (cont res)

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO -- for do notation
(>>=) = Do
```

IT would be nice if `run` were also total, but it would seem to be impossible:
 the only way to have a total, nonterminating function is to use the `Inf` type,
 and `IO` is a type of terminating action that doesn't use `Inf`. And, indeed,
 if you want functions to execute indefinitely at runtime, you'll need at least
 *some* way to escape from total functions. You can, however, try to make the
 escape as quietly as possible. To achieve this, we'll begin by making a
 terminating version of `run` that takes as an argument an upper bound on the
 number of actions it's willing to execute.<br>
It's valuable to ensure that `run` is total, because it guarantees that the
 implementation of `run` itself won't be the cause of any unexpected
 nontermination.

```idris
data Fuel = Dry | More (Lazy Fuel)

forever : Fuel -- nontotal
forever = More forever
```

It's necessary for `forever` to be nontotal because it (deliberately) introduces
 nontermination. Fortunately, this is the *only* nontotal function you need in
 order to be able to execute programs forever.

```idris
Lazy : Type -> Type
Delay : (value : ty) -> Lazy ty
Force : (computation : Lazy ty) -> ty

-- internal definition of `Inf` and `Lazy`
data DelayReason = Infinite | LazyValue

data Delayed : DelayReason -> Type -> Type where
     Delay : (val : ty) -> Delayed reason ty

Inf : Type -> Type
Inf ty = Delayed Infinite ty

Lazy : Type -> Type
Lazy ty = Delayed LazyValue ty

Force : Delayed reason ty -> ty
```

At runtime, `Inf` and `Lazy` behave the same way. The key difference between
 them is the way the totality checker treats them. If the argument has `Inf ty`,
 for some type `ty`, it's *not* considered smaller than the constructor
 expression, because it may continue expanding indefinitely. Instead, Idris will
 check that the overall expression is productive.<br>
`loopPrint` (total, continue to produce `IO` actions) + `run` (total, consume
 `IO` actions) + `forever` (only nontotal/partial, will never terminate and
 don't produce any data inside an `Inf` type)<br>
the `%default total` compiler directive means that Idris will report an error if
 there are any functions that it can't guarantee to be total. You can override
 this for individual functions with the `partial` keyword.

```idris
-- for a potentially infinite data type idea, see Peter Hancock and Anton Setzer
--  "Interactive programs and weakly final coalgebras in dependent type theory"
--  (2004)
data RunIO : Type -> Type where -- for interactive programs with termination
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit value) = pure (Just value)
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = pure Nothing

-- domain-specific commands
data Command : Type -> Type where
     -- describes a single command, which terminate
     PutStr : String -> Command ()
     GetLine : Command String

data ConsoleIO : Type -> Type where
     -- describes sequence of terminating commands, which might be infinite
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing

-- The namespace is given by the module where you define functions. Namespaces
--  are also *hierarchical*, so you can introduce further namespaces inside a
--  module.
namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do
```

### 12. Writing Programs with State

In type-driven development, a function's type tells you exactly what a function
 can do in terms of its allowed inputs and outputs. So, if you want to write a
 function that manipulates state, you can do that, but you need to be explicit
 about it in the function's type.

```idris
-- describe sequences of stateful operations
State : (stateType : Type) -> (ty : Type) -> Type
-- run a sequence of stateful operations
runState : State stateType a -> stateType -> (a, stateType)
get : State stateType stateType
   -- MonadState stateType m => m stateType
put : stateType -> State stateType ()
   -- MonadState stateType m => stateType -> m ()
(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
evalState : State stateType a -> stateType -> a
execState : State stateType a -> stateType -> stateType
update : (stateType -> stateType) -> State stateType () -- exercises
```

Although `State` encapsulates sequences of stateful operations, internally it's
 defined using pure functions.<br>
Like `IO`, `State` gives you a way of writing functions with side effects (here,
 modifying mutable state) by *describing* sequences of operations and
 *executing* them separately.

```idris
-- custom implementation
runState : State stateType a -> (st: stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put newState) st = ((), newState)
runState (Pure x) st = (x, st)
runState (Bind cmd prog) st = let (val, nextState) = runState cmd st in
                                  runState (prog val) nextState
```

`traverse` is similiar to `map` and applies a computation across a structure.
 For example, you could print every element of a `List` to the console.

```idris
interface Functor (f : Type -> Type) where
    map : (func : a -> b) -> f a -> f b
    map func x = Bind x (\val => Pure (func val))

interface Functor f => Applicative (f : Type -> Type) where
    pure  : a -> f a
    pure = Pure
    (<*>) : f (a -> b) -> f a -> f b
    (<*>) f a = Bind f (\f' =>
                Bind a (\a' =>
                Pure (f' a')))

interface Applicative m => Monad (m : Type -> Type) where
    (>>=) : m a -> (a -> m b) -> m b
    (>>=) = Bind
    join : m (m a) -> m a -- define one or both

-- or
mutual
  Functor (State stateType) where
      map func x = do val <- x
                      pure (func val)

  Applicative (State stateType) where
      pure = Pure
      (<*>) f a = do f' <- f
                     a' <- a
                     pure (f' a')

  Monad (State stateType) where
      (>>=) = Bind
```

Idris provides a library called `Effects` that supports combining different
 kinds of *side effects* like `State` and `IO` in types, as well as other
 effects such as exceptions and nondeterminism. You can find more details in the
 [Effects library tutorial][effects_library_tutorial].<br>
You might want to use a *dependent* type in your `State`, in which case updating
 the state will also update its type! For example, if you add an element to a
 `Vect 8 Int`, it would become a `Vect 9 Int`.<br>
When you define a record, the projection functions are defined in their own
 namespace, given by the name of the record. This allows the same field name to
 be used multiple times within the same module.

```idris
-- the record-update syntax itself is first-class, where `record` is a keyword
--  that begins a record update, so the record update has a type.
addCorrect : GameState -> GameState
addCorrect state
    = record { score->correct = correct (score state) + 1,
               score->attempted = attempted (score state) + 1 } state

-- The $= syntax arises from a combination of the function application operator
--  $ and the record-update syntax.
addCorrect : GameState -> GameState
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1) }
```

[effects_library_tutorial]: http://www.idris-lang.org/documentation/effects/

### 13. State Machines: Verifying Protocols in Types

This chapter covers 1) Specifying protocols in types, 2) Describing
 preconditions and postconditions of operations, and 3) Using dependent types in
 state.<br>
With dependent types, you can make the types of these commands more precise and
 include any relevant details about the state of the system in the type itself.

```idris
data DoorState = DoorClosed | DoorOpen

data DoorCmd : Type ->
               DoorState ->
               DoorState ->
               Type where
     Open : DoorCmd     () DoorClosed DoorOpen
     Close : DoorCmd    () DoorOpen   DoorClosed
     Ringbell : DoorCmd () DoorClosed DoorClosed

     Pure : ty -> DoorCmd ty state state
     (>>=) : DoorCmd a state1 state2 ->
             (a -> DoorCmd b state2 state3) ->
             DoorCmd b state1 state3
```

Notice that the type that a sequence of operations produces in the first
 argument to `DoorCmd`, and it's followed by the input and output states. This
 is a common convention when defining types for describing state transitions.

### 14. Dependent State Machines: Handling Feedback and Errors

In this chapter, you'll see how to deal with the possibility of an operation
 failing by allowing a state transition to *depend* on the result of an
 operation.

```idris
data DoorResult = OK | Jammed

data DoorCmd : (ty : Type) -> DoorState -> (ty -> DoorState) -> Type where
     Open : DoorCmd DoorResult DoorClosed
                               (\res => case res of
                                             OK => DoorOpen
                                             Jammed => DoorClosed)
     Close : DoorCmd () DoorOpen (const DoorClosed)
     RingBell : DoorCmd () DoorClosed (const DoorClosed)

     Display : String -> DoorCmd () state (const state)

     Pure : (res : ty) -> DoorCmd ty (state_fn res) state_fn
     (>>=) : DoorCmd a state1 state2_fn ->
             ((res : a) -> DoorCmd b (state2_fn res) state3_fn) ->
             DoorCmd b state1 state3_fn

logOpen : DoorCmd DoorResult DoorClosed
                             (\res => case res of
                                           OK => DoorOpen
                                           Jammed => DoorClosed)
logOpen = do Display "Trying to open the door"
             OK <- Open | Jammed => do Display "Jammed"
                                       Pure Jammed
             Display "Success"
             Pure OK

data HasCard : ATMState -> Type where
     HasCI      : HasCard CardInserted
     HasSession : HasCard Session

EjectCard : {auto prf : HasCard state} -> ATMCmd () state (const Ready)
```

### 15. Type-safe Concurrent Programming

Message passing is supported as a primitive by the Idris runtime system. In
 effect, sending a message to a process and receiving a reply corresponds to
 calling a method that returns a result in an object-oriented language.

```idris
-- in System.Concurrent.Channels
data PID : Type
data Channel : Type

spawn : (process : IO ()) -> IO (Maybe PID)
connect : (pid : PID) -> IO (Maybe Channel)
listen : (timeout : Int) -> IO (Maybe Channel)
unsafeSend : Channel -> (val : a) -> IO Bool
unsafeRecv : (expected : Type) -> Channel -> IO (Maybe expected)
```

It might seem surprising that Idris, a language that's designed to support
 type-driven developement, supports such unsafe concurrency primitives rather
 than something more sophisticated. The reason is that there are many possible
 methods for implementing safe concurrent programs in a type-driven way, and by
 prividing unsafe underlying primitives, Idris is not limited to only one of
 them.<br>
We'll define a type for *describing* the coordination between communicating
 processes, and then write a `run` function that executes the description using
 the unsafe primitives.<br>
In this section, you'll see how to solve these problems by defining a `Process`
 type, which allows you to describe well-typed communicating processes. There
 has been significant research into types for concurrent programming, most
 notably the study of *session types* that began with Kohei Honda's 1993 paper
 "Types for Dyadic Interaction." The type we'll implement in this section is an
 instance of a session type with a minimal protocol where a client sends one
 message and then receives one reply. If you're interested in exploring further,
 a recent (2016) paper, "Certifying Data in Multipart Session Types" by Bernardo
 Toninho and Nobuko Yoshida, describes a more sophisticated  way of using types
 in concurrent programs.<br>
Totality means that you're guaranteed that a function behaves in exactly the way
 described by its type, so if the type isn't precise enough, neither is the
 guarantee!

```idris
module ProcessLib

import System.Concurrency.Channels

%default total

export
data MessagePID : (iface : reqType -> Type) -> Type where
     MkMessage : PID -> MessagePID iface

public export
NoRecv : Void -> Type
NoRecv = const Void

public export
data ProcState = Ready | Sent | Looping

public export
data Process : (iface : reqType -> Type) ->
               Type -> ProcState -> ProcState -> Type where
     Request : MessagePID service_iface ->
               (msg : service_reqType) ->
               Process iface (service_iface msg) st st
     Respond : ((msg : reqType) -> Process iface (iface msg) Ready Ready) ->
               Process iface (Maybe reqType) st Sent
     Spawn : Process service_iface () Ready Looping ->
             Process iface (Maybe (MessagePID service_iface)) st st

     Loop : Inf (Process iface a Ready Looping) ->
            Process iface a Sent Looping
     Action : IO a -> Process iface a st st
     (>>=) : Process iface a st1 st2 -> (a -> Process iface b st2 st3) ->
             Process iface b st1 st3

public export
data Fuel = Dry | More (Lazy Fuel)

export parial
forever : Fuel
forever = More forever

export total
run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)
run fuel (Request {service_iface} (MkMessage process) msg)
          = do Just chan <- connect process
                    | _ => pure Nothing
               ok <- unsafeSend chan msg
               if ok then do Just x <- unsafeRecv (service_iface msg) chan
                                  | Nothing => pure Nothing
                             pure (Just x)
                     else pure Nothing
run fuel (Respond {reqType} calc)
          = do Just sender <- listen 1
                    | Nothing => pure (Just Nothing)
               Just msg <- unsafeRecv reqType sender
                    | Nothing => pure (Just Nothing)
               Just res <- run fuel (calc msg)
                    | Nothing => pure Nothing
               unsafeSend sender res
               pure (Just (Just msg))
run (More fuel) (Loop proc) = run fuel proc
run fuel (Spawn proc) = do Just pid <- spawn (do run fuel proc
                                                 pure ())
                                | Nothing => pure (Just Nothing)
                           pure (Just (Just (MkMessage pid)))
run fuel (Action act) = do res <- act
                           pure (Just res)
run fuel (Pure val) = pure (Just val)
run fuel (act >>= next) = do Just x <- run fuel act
                                  | Nothing => pure Nothing
                             run fuel (next x)
run Dry _ = pure Nothing

public export
Service : (iface : reqType -> Type) -> Type -> Type
Service iface a = Process iface a Ready Looping

public export
Client : Type -> Type
Client a = Process NoRecv a Ready Ready

partial export
runProc : Process iface () in_state out_state -> IO ()
runProc proc = do run forever proc
                  pure ()
```

This doesn't solve all possible concurrent programming problems, but you've
 defined a type that encapsulates the behavior of one kind of concurrent
 program. If a function describing a `Process` type-checks and is total, you can
 be confident that it won't deadlock and that all requests will receive replies.

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

`:let` - adds a new definition<br>
`:module` - import an extra module for use at the REPL<br>
`:apropos` - searches function names, types, and documentation for the given
 word<br>
`:search` - searches for functions with the given type<br>
`:browse` - displays the names and types defined in the given namespace

### D. Further reading

(This chapter was named as "The Packaging System" in the draft version.)

other languages and tools with expressive type systems
* [Agda][agda] - supports type-driven development using dependent types in the
  same way as Idris, but with a stronger emphasis on theorem proving
* [F*][fstar] - a functional programming language that aims to support program
  verification using refinement types, which are types augmented with a
  predicate that describes properties of values in that type
* [Coq][coq] - a proof management system based on dependent types with support
  for extracting functional programs from proofs

theoretical foundations
* [Type Theory and Functional Programming][type_theory_and_functional_programming] (1991)
* [Software Foundations][software_foundations] (2016)
* Types and Programming Languages (2002)
* Propositions as Types (2015, paper)

total functional programming
* Elementary Strong Functional Programming (1995, paper)
* [Why Dependent Types Matter][why_dependent_types_matter] (2004, paper)
* Modelling general recursion in type theory (2005)

types for concurrency
* Types for Dyadic Interaction (1993)
* Multipart Asynchronous Session Types (2008)

[agda]: http://wiki.portal.chalmers.se/agda/pmwiki.php
[fstar]: https://www.fstar-lang.org/
[coq]: https://coq.inria.fr/
[type_theory_and_functional_programming]: https://www.cs.kent.ac.uk/people/staff/sjt/TTFP/
[software_foundations]: http://www.cis.upenn.edu/~bcpierce/sf/current/index.html
[why_dependent_types_matter]: http://www.cs.nott.ac.uk/~psztxa/publ/ydtm.pdf

