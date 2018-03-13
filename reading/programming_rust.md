# [Programming Rust: Fast, Safe Systems Development][homepage] by Jim Blandy and Jason Orendorff, O'Reilly (2017)

[source code][source_code]

Rust's modern. flexible types ensure your program is free of null pointer
 dereferences, double frees, dangling pointers, and similar bugs, all at compile
 time, without runtime overhead. In multi-threaded code, Rust catches data races
 at compile time, making concurrency much easier to use.<br>
Jim Blandy works on Firefox's web developer tools for Mozilla. He's been a
 maintainer of GNU Emacs and GNU Guile, as well as GDB, the GNU Debugger. Jim is
 one of the original designers of the Subversion version control system.<br>
Jason Orendorff hacks C++ for Mozilla, where he is module owner of Firefox's
 JavaScript engine. He is an active member of the Nashville developer community
 and an occasional organizer of homegrown tech events.

In short, systems programming is *resource-constrained* programming. It is
 programming when every byte and every CPU cycle counts.

[homepage]: http://shop.oreilly.com/product/0636920040385.do
[source_code]: https://github.com/ProgrammingRust

## 1. Why Rust?

Enter Rust: a safe, concurrent language with the performance of C and C++.<br>
In fact, Servo and Rust have grown up together, with Servo using the latest new
 language features, and Rust evolving based on feedback from Servo's
 developers.<br>
An undefined operation doesn't just produce an unspecified result: It is allowed
 to cause the program to do *anything at all*. If a program has been written so
 that no possible execution can exhibit undefined behavior, we say that program
 is *well defined*. If a language's safety checks ensure that every program is
 well defined, we say that language is *type safe*.

## 2. A Tour of Rust

`rustup` (Rust installer)<br>
`cargo new --bin hello`<br>
Four-space indentation is standard Rust style.<br>
There is also a `debug_assert!` macro, whose assertions are skipped when the
 program is compiled for speed (`--release`).<br>
Rust only infers types within function bodies: you must write out the types of
 function parameters and return values.<br>
If a function body ends with an expression that is *not* followed by a
 semicolon, that's the function's return value. In fact, any block surrounded by
 curly braces can function as an expression.<br>
The `#[test]` atop the definition marks `test_gcd` as a test function, to be
 skipped in normal compilations, but included and called automatically if we run
 our program with the `cargo test` command. Attributes are used to control
 compiler warnings and code style checks, include code conditionally, tell Rust
 how to interact with code written in other languages, and so on.<br>
Any type that implements the `Write` trait has a `write_fmt` method that writes
 formatted text to a stream. The `std::io::Stderr` type implements `Write`, and
 we'll use the `writeln!` macro to print error messages; that macro expands to
 code that uses the `write_fmt` method.<br>
Functions that perform input or output or otherwise interact with the operating
 system all return `Result` types (`Ok(v)` and `Err(e)`). Unlike most modern
 languages, Rust does not have exceptions: all errors are handled using either
 `Result` or panic. We check the success of our parse by using `Result`'s
 `expect` method. The `.unwrap()` call is a terse way to check that the attempt
 to print the error message did not itself fail; an `expect` call would work
 too, but that's probably not worth it.<br>
`rustup doc --std`, https://doc.rust-lang.org/

We'll put together a simple web server using the `iron` web framework, the
 `hyper` HTTP server, and various other [crates][crates] on which they
 depend.<br>
By convention, when a module is named `prelude`, that means that its exports are
 intended to provide the sort of general facilities that any user of the crate
 will probably need. So in this case, a wildcard `use` directive makes a bit
 more sense.<br>
Rust "raw string" syntax: the letter `r`, zero or more hash marks, a double
 quote, and ...<br>
Rust allows delarations to occur in any order, as long as they appear at the
 appropriate level of nesting. (Macro definitions and `extern crate` item with
 `#[macro_use]` attributes are exceptions to this rule: they must appear before
 they are used.)<br>
Rust allows you to define your own types like `Result` with value-carrying
 variants, and use match expressions to analyze them. Rust calls these types
 *enums*; you may know them from other languages as *algebraic data types*.

Rust guarantees this is safe to do, no matter how elaborate your server gets: if
 your program compiles, it is free of data races. All Rust functions are
 thread-safe.<br>
Plotting the Mandelbrot set is often called an *embarrassingly parallel*
 algorithm, because the pattern of communication between the threads is so
 simple.<br>
`#[allow(dead_code)]`<br>
Since a complex number `c` has both real and imaginary components `c.re` and
 `c.im`, we'll treat these as the `x` and `y` coordinates of a point on the
 Cartesian plane, and color the point black if `c` is in the Mandelbrot set, or
 a lighter color otherwise.<br>
The comments that start with `///` are *documentation comments*; the `rustdoc`
 utility knows how to parse them, together with the code they describe, and
 produce online documentation.<br>
It's common to initialize a struct's fields with variables of the same name, so
 Rust lets you simply write `Complex { re, im }`. This is modeled on similar
 notations in JavaScript and Haskell.

```rust
let output = match File::create(filename) {
    Ok(f) => { f }
    Err(e) => { return Err(e); }
};
let output = File::create(filename)?;

type std::io::Result<T> = std::result::Result<T, std::io::Error>
```

The `?` operator is only used within functions that themselves return `Result`.
 Since `main` has no return value, this won't work; you should use Result's
 `expect` method instead.<br>
The `crossbeam` crate provides a number of valuable concurrency facilities,
 including a *scoped thread* facility that does exactly what we need here. The
 buffer's `chunks_mut` method returns an iterator producing mutable,
 nonoverlapping slices of the buffer. The `crossbeam::scope` function waits for
 all such threads to finish execution before returning itself. The `into_iter()`
 iterator gives each iteration of the loop body exclusive ownership of one band,
 ensuring that only one thread can write to it at a time.<br>
The iterator's `collect` method builds a vector holding these mutable,
 nonoverlapping slices.<br>
`|spawner| { ... }` is a Rust *closure* expression. A closure is a value that
 can be called as if it were a function. Unlike functions declared with `fn`, we
 don't need to declare the types of a closure's arguments.<br>
The `num_cpus` crate provides a function that returns the number of CPUs
 available on the current system.<br>
The `move` keyword at the front indicates that this closure takes ownership of
 the variables it uses.<br>
`cargo build --release`

[crates]: https://crates.io/

## 3. Basic Types

* `b'*'` - `u8` byte literal (only ASCII characters may appear in byte
  literals.)
* `enum Attend { OnTime, Late(u32) }` - enumeration, algebraic data type
* `Box<Attend>` - box: owning pointer to value in heap
* `&i32`, `&mut i32` - shared and mutable references: nonowning pointers that must not outlive their referent
* `&[u8]`, `&mut [u8]` - reference to slice: reference to a portion of an array or vector, comprising pointer and length
* `&Any`, `&mut Read` - trait object: reference to any value that implements a given set of methods

Rust requires array indices to be `usize` values.<br>
In debug builds, Rust checks for integer overflow in arithmetic. In a release
 build, this addition would wrap to a negative number (unlike C++, where signed
 integer overflow is undefined behavior). When you want wrapping arithmetic, use
 the methods like `wrapping_add()`. Conversions that are out of range for the
 destination produce values that are equivalent to the original modulo 2^N.<br>
Rust defaults to `i32`, if that is among the possibilities. Otherwise, Rust
 reports the ambiguity as an error.<br>
Rust uses an entire byte for a `bool` value in memory, so you can create a
 pointer to it.<br>
Going in the other direction, `u8` is the only type the `as` operator will
 convert to `char`: Rust intends the `as` operator to perform only cheap,
 infallible conversions, but every integer type other than `u8` includes values
 that are not permitted Unicode code points, so those conversions would require
 runtime checks. Instead, the standard library function `std::char::from_u32`
 takes any `u32` value and returns an `Option<char>`.<br>
Tuples allow only constants as indices.<br>
Rust consistently permits an extra trailing comma everywhere commas are used:
 function arguments, arrays, struct and enum definitions, and so on.

Rust tracks the ownership and lifetimes of values, so mistakes like dangling
 pointers, double frees, and pointer invalidation are rules out at compile
 time.<br>
When the box goes out of scope, the memory is freed immediately, unless it has
 been *moved*--by returning it, for example.

Rust has no notation for an uninitialized array. (In general, Rust ensures that
 code can never access any sort of uninitialized value.) An array's length is
 part of its type and fixed at compile time.<br>
The useful methods you'd like to see on arrays all appear as methods of slices,
 not arrays. But Rust implicitly converts a reference to an array to a slice
 when searching for methods, so you can call any slice method on an array
 directly.<br>
Since the number of actual elements is at least half the buffer size, the vector
 has always performed less than two copies per element!<br>
Since a slice can be any length, slices can't be stored directly in variables or
 passed as function arguments. Slices are always passed by reference. A
 reference to a slice is a *fat pointer*: a two-word value comprising a pointer
 to the slice's first element, and the number of elements in the slice.

In string literals, unlike `char` literals, single quotes don't need a backslash
 escape. A string may span multiple lines. If one line of a string ends with a
 backslash, then the newline character and the leading whitespace on the next
 line are dropped. You can't include a double-qoute character in a raw string
 simply by putting a backslash in front of it---remember, we said *no* escape
 sequences are recognized.<br>
A string literal with the `b` prefix is a *byte string*. Such a string is a
 slice of `u8` values---that is, bytes---rather than Unicode text. It doesn't
 have any of the string methods.<br>
`String` is a `Vec<u8>` that is guaranteed to hold well-formed UTF-8.<br>
The length that A `String` or `&str`'s `.len()` method returns is measured in
 bytes, not characters. (cf. `.chars().count()`)<br>
The type `&mut str` does exist, but it is not very useful, since almost any
 operation on UTF-8 can change its overall byte length, and a slice cannot
 reallocate its referent.

## 4. Ownership

This is a pretty well-explored area of language design. The net effect of these
 restrictions is to bring just enough order to the chaos to allow Rust's
 compile-time checks to verify that your program is free of memory safety
 errors. At runtime, your pointers are simple addresses in memory, just as they
 would be in C and C++. The difference is that your code has been proven to use
 them safely.<br>
The nondeterministic behavior inherent in multithreaded code is isolated to
 those features designed to handle it---mutexes, message channels, atomic
 values, and so on---rather than appearing in ordinary memory references.<br>
Newer versions of the C++ specification effectively preclude that representation
 using a reference count.

When the owner is freed---*dropped*, in Rust terminology---the owned value is
 dropped too. Just as variables own their values, structs own their fields; and
 tuples, arrays, and vectors own their elements. If there were other sorts of
 collections in the picture---a `HashMap`, perhaps, or a `BTreeSet`---the story
 would be the same.<br>
Rust programs don't usually explicitly drop values at all, in the way C and C++
 programs would use `free` and `delete`. The way to drop a value in Rust is to
 remove it from the ownership tree somehow. Rust extends this picture in several
 ways:

* You can move values from one owner to another. This allows you to build,
  rear-range, and tear down the tree.
* The standard library provides the reference-counted pointer types `Rc` and
  `Arc`, which allow values to have multiple owners, under some restrictions.
* You can "borrow a reference" to a value; references are nonowning pointers,
  with limited lifetimes.

The moves always apply to the value proper, not the heap storage they own. For
 vectors and strings, the *value proper* is the three-word header alone; the
 potentially large element arrays and text buffers sit where they are in the
 heap.<br>
The `std::mem::replace`` call moves out the valye of `composer[0].name`, leaving
 `None` in its place, and passes ownership of the original value to its caller.
 In fact, using `Option` this way is common enough that the type provides a
 `take` method for this very purpose.<br>
Assigning a value of a `Copy` type copies the value, rather than moving it. The
 standard `Copy` types include all the machine integer and floating-point
 numeric types, the `char` and `bool` types, and a few others. A tuple or
 fixed-size array of `Copy` types is itself a `Copy` type.<br>
By default, `struct` and `enum` types are not `Copy`. If all the fields of your
 struct are themselves `Copy`, then you can make the type `Copy` as well by
 placing the attribute `#[derive(Copy, Clone)]` above the definition.<br>
Making a type `Copy` represents a serious commitment on the part of the
 implementer: if it's necessary to change it to non-`Copy` later, much of the
 code that uses it will probably need to be adapted.<br>
One of Rust's principles is that costs should be apparent to the programmer.
 Basic operations must remain simple. Potentially expensive operations should be
 explicit, like the calls to `clone`.

The only difference between them is that an `Arc` is safe to share between
 threads directly---the name `Arc` is short for *atomic reference
 count*---whereas a plain `Rc` uses faster non-thread-safe code to update its
 reference count. Rust will prevent you from accidentally passing one across a
 thread boundary.<br>
You can use any of `String`'s usual methods directly on an `Rc<String>`. Rust's
 memory and thread-safety guarantees depend on ensuring that no value is ever
 simultaneously shared and mutable. Rust assumes the referent of an `Rc` pointer
 might in general be shared, so it must not be mutable.<br>
You cannot create a cycle without, at some point, making an older value point to
 a newer value. Since `Rc` pointers hold their referents immutable, it's not
 normally possible to create a cycle. However, Rust does provide ways to create
 mutable portions of otherwise immutable values; this is called *interior
 mutability*. You can sometimes avoid creating cycles of `Rc` pointers by using
 *weak pointers*, `std::rc::Weak`, for some of the links instead.

## 5. References

References must never outlive their referents. To emphasize this, Rust refers to
 creating a reference to some value as *borrowing* the value: what you have
 borrowed, you must eventually return to its owner.<br>
Shared references are `Copy`. Mutable references are not `Copy`.<br>
As long as there are shared references to a value, not even its owner can modify
 it.<br>
Since references are so widely used in Rust, the `.` operator implicitly
 dereferences its left operand, if needed. The `.` operator can also implicitly
 borrow a reference to its left operand, if needed for a method call.
 (`v.sort()` is the same as `(&mut v).sort()`) Like the `.` operator, Rust's
 comparison operators "see through" any number of references, as long as both
 operands have the same type.<br>
`std::ptr::eq` compares them as addresses.<br>
`Option<&T>` is just as efficient as a nullable pointer in C or C++, even though
 it's safer: its type requires you to check whether it's `None` before you can
 use it.<br>
Rust lets you borrow a reference to the value of any sort of expression at
 all.<br>
`assert_eq!(r + &1009, 1729);` (?)<br>
Rust also includes two kinds of *fat pointers*, two-word values carrying the
 address of some value, along with some further information necessary to put the
 value to use. A reference to a slice is a fat pointer. A trait object, a
 reference to a value that implements a certain trait, carries a value's address
 and a pointer to the trait's implementation appropriate to that value, for
 invoking the trait's methods.<br>
Rust's equivalent of a global variable is called a *static*. Evert static must
 be initialized. Mutable statics are inherently not thread-safe, and even in
 single-threaded programs, they can fall prey to other sorts of reentrancy
 problems. For these reasons, you may access a mutable static only within an
 `unsafe` block.<br>
In `fn f<'a>(p: &'a i32) { ... }`, the lifetime `'a` ("tick A") is a *lifetime
 parameter* of `f`. You can read `<'a>` as "for any lifetime `'a`". (cf.
 `'static`)<br>
Whenever a reference type appears inside another type's definition, you must
 write out its lifetime.<br>
If your function is a method on some type and takes its `self` parameter by
 reference, Rust assumes that `self`'s lifetime is the one to give everything in
 your return value.<br>
Throughout its lifetime, a shared reference makes its referent read-only: you
 may not assign to the referent or move its value elsewhere.<br>
The only (shared or mutable) references whose lifetimes may overlap with a
 mutable reference are those you borrow from the mutable reference itself.<br>
The path of ownership leading to the referent cannot be changed for the
 reference's lifetime. For a shared borrow, the path is read-only; for a mutable
 borrow, it's completely inaccessible.<br>
C++'s `std::map` promises that inserting new entries doesn't invalidate pointers
 to other entries in the map, but by making that promise, the standard precludes
 more cache-efficient designs like Rust's `BTreeMap`.<br>
A concurrent Rust program that avoids `unsafe` code is free of data taces *by
 construction*.<br>
Since the rise of automatic memory management in the 1990s, the default
 architecture of all programs has been the sea of objects. Rust prefers for
 pointers, ownership, and data flow to pass through the system in one direction.
 Rust is all about transferring the pain of understanding your program from the
 future to the present.

## 6. Expressions

Rust is what is called an *expression language*.<br>
An item is simply any declaration that could appear globally in a program or
 module, such as a `fn`, `struct`, or `use`. A block can also contain *item
 declarations*. A block can even contain a whole module.<br>
A similar optimization is applied when each arm of a `match` produces a constant
 value.<br>
The comma after an arm in a `match` expression may be dropped if the *`expr`* is
 a block.<br>
Rust prohibits `match` expressions that do not cover all possible values.<br>
The value of a loop is `()`. A loop can be *labeled* with a lifetime like
 `'search:`.<br>
`0..20` is the same as `std::ops::Range { start: 0, end: 20 }`. Ranges can be
 used with `for` loops because `Range` is an iterable type: it implements the
 `std::iter::IntoIterator` trait.<br>
Expressions that don't finish normally are assigned the special type `!`, and
 they're exempt from the rules about types having to match.

In the method call `player.location()`, `player` might be a `Player`, a
 reference of type `&Player`, or a smart pointer of type `Box<Player>` or
 `Rc<Player>`.<br>
The symbol `::<...>` is affectionately known in the Rust community as the
 *turbofish*.<br>
The value to the left of the brackets is automatically dereferenced, too.<br>
Integers have a method `a.checked_div(b)` that returns an `Option` and never
 panics. `%` can be used on floating-point numbers as well as integers. Rust
 uses `!` instead of `~` for bitwise NOT. This means that `!n` can't be used on
 an integer `n` to mean "n is zero." Unlike C, Rust doesn't suppor chaining
 assignment. Rust does not have C's increment and decrement operators.<br>
automatic conversions - `mut` reference → non-`mut` reference, `&String` →
 `&str`, `&Vec<i32>` → `&[i32]`, `&Box<Chessboard>` → `&Chessboard`<br>
These are called *deref coercions*, because they apply to types that implement
 the `Deref` built-in trait. The purpose of Deref coercion is to make smart
 pointer types behave as much like the underlying value as possible.<br>
If you specify a return type, then the body of the closure must be a block, for
 the sake of syntactic sanity.

## 7. Error Handling

A panic is not a crash. It's not undefined behavior. Panic is safe. Panic is per
 thread.<br>
Rust gives you a choice. Rust can either unwind the stack when a panic happens,
 or abort the process. Unwinding is the default.<br>
`std::panic::catch_unwind()` is the mechanism used by Rust's test harness to
 recover when an assrtion fails in a test. It can also be necessary when writing
 Rust code that can be called from C or C++, because unwinding across non-Rust
 code is undefined behavior.<br>
If a `.drop()` method triggers a second panic while Rust is still trying to
 clean up after the first, this is considered fatal. Also, if you compile with
 `-C panic=abort`, the first panic in your program immediately aborts the
 process. (With this option, Rust does not need to know how to unwind the stack,
 so this can reduce the size of your compiled code.)

`Result` methods (*consume* the `result`) - `.is_ok()`, `.is_err()`, `.ok()`,
 `.err()`, `.unwrap_or(fallback)`, `.unwrap_or_else(fallback_fn)`, `.unwrap()`,
 `.expect(message)`, `.as_ref()`, `.as_mut()`<br>
The standard library's error types do not include a stack trace, but the
 `error-chain` crate makes it easy to define your own custom error type that
 supports grabbing a stack trace when it's created. It uses the `backtrace`
 crate to capture the stack.<br>
In older code, you may see the `try!()` macro, which was the usual way to
 propagate errors until the `?` operator was introduced in Rust 1.13.<br>
An easy way to handle multiple error types is to define these type aliases:
 `type GenError = Box<std::error::Error>;` and
 `type GenResult<T> = Result<T, GenError>;`<br>
If you're calling a function that returns a `GenResult`, and you want to
 handle one particular kind of error, but let all others propagate out, use
 the generic method `error.downcast_ref::<ErrorType>()`. Many languages have
 built-in syntax to do this, but it turns out to be rarely needed. Rust has a
 method for it instead.

```rust
#[derive(Debug, Clone)]
pub struct JsonError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

return Err(JsonError {
    messasge: "expected ']' at end of array".to_string(),
    line: current_line,
    column: current_column
});

use std;
use std::fmt;

// Errors should be printable.
impl fmt::Display for JsonError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} ({}:{})", self.message, self.line, self.column)
    }
}

// Errors should implement the std::error::Error trait.
impl std::error::Error for JsonError {
    fn description(&self) -> &str {
        &self.message
    }
}
```

## 8. Crates and Modules

When compiling libraries, Cargo uses the `--crate-type lib` option. This tells
 `rustc` to produce an *.rlib* file. Rust will statically link that code into
 the final executable. The *.rlib* also contains type information, so Rust can
 check that the library features we're using in our code actually exist in the
 crate, and that we're using them correctly. It also contains a copy of the
 crate's public inline functions, generics, and macros, features that can't be
 fully compiled to machine code until Rust sees how we use them. With each
 `rustc` command, Cargo passes `--extern` options giving the filename of each
 library the crate will use.<br>
Rust never compiles modules separately. When you build a Rust crate, you're
 recompiling all of its modules.<br>
When Rust sees `mod spores;`, it checks for both *spores.rs* and
 *spores/mod.rs*; if neither file exists, or both exist, that's an error.<br>
Paths in `use` declarations are automatically absolute paths, so there is no
 need for a leading `::`.<br>
Modules do *not* automatically inherit names from their parent modules.
 Submodules can access private items in their parent modules, but they have to
 import each one by name. `use super::*;` only imports items that are marked
 `pub`. The keyword `super` is an alias for the parent module, and `self` is an
 alias for the current module.<br>
Rust behaves as though every module, including the root module, started with the
 following import: `use std::prelude::v1::*;`<br>
Enforcing access control by module is surprisingly helpful for software design.
 It cuts down on biolerplate "getter" and "setter" methods, and it largely
 eliminates the need for anything for C++ `friend` declarations. Private
 methods, like private struct fields, are visible throughout the module where
 they're declared.<br>
A `impl` block can't be marked `pub`. Instead, individual methods are marked
 `pub` to make them visible outside the current module.<br>
A static is a variable that's set up before your program starts running and
 lasts until it exits. Use statics for larger amounts of data, or any time
 you'll need to borrow a reference to the constant value. Statics can be marked
 `mut`, but Rust discourages global mutable state. Rust have no way to enforce
 its rules about exclusive access on `mut` statics. They are, therefore,
 inherently non-thread-safe, and safe code can't use them at all.<br>
Rust warns about items that are declared, but never used.<br>
`cargo run --bin efern`<br>
Cargo automatically treats *.rs* files in *src/bin* as extra programs to
 build.<br>
`#[warn(...)]`, `#[allow(...)]`, `#[cfg(...)]`<br>
When a function or method defined in one crate is called in another crate, Rust
 won't inline it unless it's generic or it's explicitly marked `#[inline]`.
 Otherwise, the compiler treats `#[inline]` as a suggestion. Rust also supports
 the more insistent `#[inline(always)]` and `#[inline(never)]`.
To attach an attribute to a whole crate, add it at the top of the *main.rs* or
 *lib.rs* file, before any items, and write `#!` instead of `#`. For example,
 the `#![feature(...)]` attribute is used to turn on *unstable* features of the
 Rust language and libraries. Features are experimental, and therefore might
 have bugs or might be changed or removed in the future.<br>
`#[test]`, `#[should_panic(expected="divide by zero")]`<br>
Integration tests are *.rs* files that live in a *tests* directory alongside
 your project's *src* directory.<br>
`cargo doc --no-deps --open`<br>
When Rust sees comments that start with three slashes, it treats them as a
 `#[doc]` attribute instead. Likewise, comments starting with `//!` are treated
 as `#![doc]` attributes, and are attached to the enclosing feature, typically a
 module or crate. The content of a doc comment is treated as Markdown. You can
 also fall back on HTML. When you include a block of code (indenting four spaces
 or Markdown fenced code blocks) in a doc comment, Rust automatically turns it
 into a test. To hide a line of a code sample (such as imports or setup code),
 put a `#` followed by a space at the beginning of that line. To tell Rust to
 compile your example, but stop short of actually running it, use a fenced code
 block with the `no_run` annotation. If the code isn't even expected to compile,
 use `ignore` instead of `no_run`. If the code block isn't Rust code at all, use
 the name of the language or `text` for plain text.<br>
If your project is an executable, you should commit *Cargo.lock* to version
 control. `cargo update`<br>
`cargo package [--list]`, `cargo login ...`, `cargo publish`<br>
You can save compilation time and disk space by using a Cargo *workspace*, a
 collection of crates that share a common build directory and *Cargo.lock*
 file.<br>
When you publish an open source crate on *crates.io*, your documentation is
 automatically rendered and hosted on *docs.rs*.<br>
You can generate a *README.md* file from your crate's top-level doc-comment.
 `cargo install readme`

## 9. Structs

Rust has three kinds of struct types, *named-field*, *tuple-like*, and
 *unit-like*.<br>
The convention in Rust is for all types, structs included, to have names that
 capitalize the first letter of each word. Fields and methods are lowercase,
 with words separated by underscores. This is called *snake_case*.<br>
In a struct expression, if the named fields are followed by `.. EXPR`, then any
 fields not mentioned take their values from `EXPR`, which must be another value
 of the same struct type.<br>
Defining the tuple-like struct type implicitly defines a function.<br>
Tuple-like structs are good for *newtypes*, structs with a single component that
 you define to get stricter type checking.<br>
Rust doesn't bother actually storing unit-like struct values in memory or
 generating code to operate on them, because it can tell everything it might
 need to know about the value from its type alone. The expression `..`, a range
 omitting both endpoints, is shorthand for the unit-like struct value
 `RangeFull`.<br>
Unlike C and C++, Rust doesn't make specific promises about how it will order a
 struct's fields or elements in memory. You can ask Rust to lay out structures
 in a way compatible with C and C++, using the `#[repr(C)]` attribute.

Methods are also known as *associated functions*. The opposite of an associated
 function is a *free function*.<br>
Rust passes a method the value it's being called on as its first argument, which
 must have the special name `self`. Since `self`'s type is obviously the one
 named at the top of the `impl` block, or a reference to that, Rust lets you
 omit the type.<br>
However, when you call a method, you don't need to borrow the mutable reference
 yourself; the ordinary method call syntax takes care of that implicitly.<br>
Althrough you can have many separate `impl` blocks for a single type, they must
 all be in the same crate that defines that type.<br>
Rust uses this same syntax for defining methods on types that are not structs at
 all, such as `enum` types and primitive types. (The fact that any type can have
 methods is one reason Rust doesn't use the term *object* much, preferring to
 call everything a *value*.) The same `impl` syntax also serves neatly for
 implementing traits.<br>
As another shorthand, every `impl` block, generic or not, defines the special
 type parameter `Self` to be whatever type we're adding methods to.<br>
Rust always infers lifetime parameters for calls.<br>
`#[derive(Copy, Clone, Debug, PartialEq)]`<br>
What we need is a little bit of mutable data inside an otherwise immutable
 value. This is called *interior mutability*. Rust offers several flavors of it;
 in this section, we'll discuss the two most straightforward types: `Cell<T>`
 and `RefCell<T>`, both in the `std::cell` module. A `Cell<T>` is a struct that
 contains a single private value of type `T`. The only special thing about a
 `Cell` is that you can get and set the field even if you don't have `mut`
 access to the `Cell` itself. `RefCell` supports borrowing references to its `T`
 value.<br>
`RefCell` enforces the same (compile time) rule using runtime checks. So if
 you're breaking the rules, you get a panic. The other drawback is less obvious
 and more serious: cells---and any types that contain them---are not
 thread-safe. We'll describe thread-safe flavors of interior mutability:
 Mutex<T>, atomics, and global variables.

## 10. Enums and Patterns

By default, Rust stores C-style enums using the smallest built-in integer type
 that can accommodate them. `size_of::<Ordering>()`<br>
You can either write your own checked conversion or use the `enum_primitive`
 crate.<br>
These are called *tuple variants*. Like tuple structs, these constructors are
 functions that create new values. Enums can also have *struct variants*. In
 all, Rust has three kinds of enum variant, echoing the three kinds of struct.
 All constructors and fields of a public enum are aitomatically public.<br>
In memory, enums with data are stored as a small integer *tag*, plus enough
 memory to hold all the fields of the largest variant. However, Rust makes no
 promises about enum layout.<br>
One unobvious detail is that Rust can eliminate the tag field of `Option<T>`
 when the type `T` is a `Box` or some other smart pointer type. An
 `Option<Box<i32>>` is stored in memory as a single machine word, 0 for `None`
 and nonzero for `Some` boxed value.

Expressions *produce* values; patterns *consume* values.<br>
Rust patterns are their own little language. `ref mut field` vs `&value` (to get
 the pointed-to value),`val @ 0 ... 99`, `'a' | 'A'`, `x if x * x <= r2`<br>
It's worth noting that existing variables can't be used in patterns.<br>
It is cumbersome to match a large struct when we only care about a few fields.
 To avoid this, use `..` to tell Rust you don't care about any of the other
 fields.<br>
You can't move a value out of a reference, even a `mut` reference. It works
 because those fields are copyable.<br>
If a pattern moves any values, you can't put a guard on it. Therefore, the
 preceding code works only if copyable.<br>
Patterns are also allowed in several other places, typically in place of an
 identifier. Patterns that always match are special in Rust. They're called
 *irrefutable patterns*, and they're the only patterns allowed in the four
 places shown here (after `let`, in function arguments, after `for`, and in
 closure arguments). Refutable patterns are also allowed in `if let` and
 `while let` expressions.<br>
For cases when a value may be one thing, or another thing, or perhaps nothing at
 all, enums are beter than class hierarchies on every axis: faster, safer, less
 code, easier to document. The limiting factor is flexibility. End users of an
 enum can't extend it to add new variants. When variants are added, existing
 code breaks. In some cases, trading flexibility for simplicity is just good
 sense. But sometimes more flexibility is needed. For some those situations,
 Rust has traits.

## 11. Traits and Generics

Rust supports polymorphism with two related features: traits and generics. These
 concepts will be familiar to many programmers, but Rust takes a fresh approach
 inspired by Haskell's typeclasses.<br>
*Traits* are Rust's take on interfaces or abstract base classes. We'll cover the
 `Self` type, associated methods, and associated types, three features Rust
 lifted from Haskell that elegantly solve problems that other languages address
 with workarounds and hacks.<br>
`&mut Write` and `<T: Write>` are similar. Only calls through `&mut Write` incur
 the overhead of a virtual method call.<br>
There is one unusual rule about trait methods: the trait itself must be in
 scope. Otherwise, all its methods are hidden. The reason `Clone` and `Iterator`
 methods work without any special imports is that they're always in scope by
 default: they're part of the standard prelude, names that Rust automatically
 imports into every module. In fact, the prelude is mostly a carefully chosen
 selection of traits.<br>
Rust doesn't permit variables of type `Write`. A variable's size has to be known
 at compile time, and types that implement `Write` can be any size.<br>
A reference to a trait type is called a *trait object*. A trait object includes
 a little extra information about the referent's type (to dynamically call the
 right method depending on the type). You can't query the type information
 directly, and Rust does not support downcasting from the trait object
 `&mut Write` back to a concrete type like `Vec<u8>`.<br>
In memory, a trait object is a fat pointer consisting of a pointer to the value,
 plus a pointer to a table representing that value's type. Each trait object
 therefore takes up two machine words. In C++, the vtable pointer, or *vptr*, is
 stored as part of the struct. Rust uses fat pointers instead. The struct itself
 contains nothing but its fields. This way, a struct can implement dozens of
 traits without containing dozens of vptrs.<br>
Rust automatically converts ordinary references into trait objects when needed.
 Rust converts `&mut File` to `&mut Write`, and `Box<File>` to `Box<Write>`.
 `Box<Write>` is a fat pointer. The same goes for other pointer types, like
 `Rc<Write>`. This kind of conversion is the only way to create a trait object.
 What the computer is actually doing here is very simple. At the point where the
 conversion happens, Rust knows the referent's true type, so it just adds the
 address of the appropriate vtable, turning the regular pointer into a fat
 pointer.

`fn say_hello(out: &mut Write)` vs `fn say_hello<W: Write>(out: &mut W)`<br>
Type parameters are usually single uppercase letters, by convention.<br>
If the generic function you're calling doesn't have any argument that provide
 useful clues, you may have to spell it out.
 `(0 .. 1000).collect::<Vec<i32>>()`<br>
This kind of `where` clause is also allowed on generic structs, enums, type
 aliases, and methods---anywhere *bounds* are permitted.

```rust
fn top_ten<T: Debug + Hash + Eq>(values: &Vec<T>) { ... }

fn run_query<M, R>(data: &DataSet, map: M, reduce: R) -> Results
    where M: Mapper + Serialize,
          R: Reducer + Serialize
{ ... }

use std::ops::{Add, Mul};

fn dot<N>(v1: &[N], v2: &[N]) -> N
    where N: Add<Output=N> + Mul<Output=N> + Default + Copy
    // or use num crate, Num + Copy
{
    let mut total = N:default();
    for i in 0 .. v1.len() {
        total = total + v1[i] * v2[i];
    }
    total
}

struct Salad<V: Vegetable> {
    veggies: Vec<V>
    // Each such salad consists entirely of a single type of vegetable.
}

struct Salad {
    veggies: Vec<Box<Vegetable>>
    // Each Box<Vegetable> can own any type of vegetable, but the box itself has
    //  a constant size---two pointers---suitable for storing in a vector.
}
```

In Rust, generics are the more common choice. The first advantage is speed.
 There's no need for dynamic dispatch. The second advantage is that not every
 trait can support trait objects. Tratis support several features, such as
 static methods, that work only with generics.<br>
Everything defined in a trait `impl` must actually be a feature of the trait; if
 we wanted to add a helper method, we would have to define it in a separate
 `impl` block.<br>
Rust lets you implement any trait on any type, as long as either the trait or
 the type is introduced in the current crate. Either the trait or the type must
 be new in the current crqte. This is called the *coherence rule*. It helps Rust
 ensure that trait implementations are unique. You code can't
 `impl Write for u8`, because both `Write` and `u8` are defined in the standard
 library.<br>
A trait that uses the `Self` type is incompatible with trait objects. Rust has
 no way to know at compile time if arguments will be the same type, as
 required.<br>
Trait objects are really intended for the simplest kinds of traits, the kinds
 that could be implemented using interfaces in Java or abstract base classes in
 C++. The more advanced features of traits are useful, but they can't coexist
 with trait objects because with trait objects, you lose the type information
 Rust needs to type-check your program.<br>
Like Java and C# interfaces, trait objects don't support static methods. If you
 want to use `&StringSet` trait objects, you must change the trait, adding the
 bound `where Self: Sized` to each static method. This bound tells Rust that
 trait objects are excused from supporting this method. `StringSet` trait
 objects are then allowed; they still don't support the two static methods. The
 same trick works for any other method that is incompatible with trait
 objects.

All four of these method calls do exactly the same thing. Most often, you'll
 just write `"hello".to_string()`. The other forms (`str::to_string("hello")`
 and `ToString::to_string("hello")`) are *qualified* method calls. They specify
 the type or trait that a method is associated with. The last form,
 `<str as ToString>::to_string("hello")`, specifies both: a *fully qualified*
 method call. Rust has a method lookup algorithm that figures this out,
 depending on the types, deref coercions, and so on. With fully qualified calls,
 you can say exactly which method you mean, and that can help in a few odd
 cases: when two methods have the same name, when the type of the `self`
 argument can't be inferred, when using the function itself as a function value,
 when calling trait methods in macros<br>
Traits can describe relationships between types.<br>
The first feature of this trait, `type Item;`, is an *associated type*. The type
 is written as `Self::Item`, not just plain `Item`, because `Item` is a feature
 of each type of iterator, not a standalone type.<br>
The syntax `<RHS=Self>` means that `RHS` defaults to `Self`.<br>
The expression `lhs * rhs` is shorthand for `Mul::mul(lhs, rhs)`. So overloading
 the `*` operator in Rust is as simple as implementing the `Mul` trait.<br>
*Buddy traits* are simply traits that are designed to work together.<br>
Fortunately, there is a standard `Default` trait for types that have default
 values.<br>
The reason it was a bit of a pain is that there wasn't a single `Number` trait
 in the standard library that included all the operators and methods we wanted
 to use. There's a popular open source crate called `num` that defines such a
 trait!<br>
Why didn't Rust's designers make the generics more like C++ templates, where the
 constraints are left implicit in the code, à la "duck typing"? One advantage of
 Rust's approach is forward compatibility of generic code. If you didn't change
 the signature, you havn't broken any of its users. Perhaps the most important
 advantage of writing out the bounds explicitly is that you can look at the
 signature of a generic function in Rust and see exactly what kind of arguments
 it accepts.

## 12. Operator Overloading

```rust
use std::ops::Add;

impl<L, R, O> Add<Complex<R>> for Complex<L>
    where L: Add<R, Output=O>
{
    type Output = Complex<O>;
    fn add(self, rhs: Complex<R>) -> Self::Output {
        Complex { re: self.re + rhs.re, im: self.im + rhs.im }
    }
}
```

Rust does not permit the left operand of `+` to be a `&str`, to discourage
 building up long strings by repeatedly concatenating small pieces on the left.
 Generally, the `write!` macro is better for building up strings piece by
 piece.<br>
The value of a compound assignment expression is always `()`.

```rust
trait PartialEq<Rhs: ?Sized = Self> {
    // letting us write traits like PartialEq<str> or PartialEq<[T]>
    fn eq(&self, other: &Rhs) -> bool;
    fn ne(&self, other: &Rhs) -> bool { !self.eq(other) }
}
```

The traditional mathematical definition of an *equivalence relation*, of which
 equality is one instance, imposes three requirements: ... It must always be
 true that `x == x`. It clearly doesn't meet the third when used on IEEE
 floating-point values. This is called a *partial equivalence relation*.<br>
In practice, almost every type that implements `PartialEq` should implement `Eq`
 as well; `f32` and `f64` are the only types in the standard library that are
 `PartialEq` but not `Eq`.<br>
With the `#[derive(Clone, Copy, Debug, Eq, PartialEq)]` attribute,
 `Complex<i32>` would implement `Eq`, but `Complex<f32>` would implement only
 `PartialEq`.<br>
`PartialOrd<Rhs>` extends `PartialEq<Rhs>`. The only method of `PartialOrd` you
 must implement yourself is `partial_cmp`.<br>
If you know that values of two types are always ordered with respect to each
 other, you can implement the stricter `trait Ord: Eq + PartialOrd<Self>`.

You can refer to a subslice with an expression like `a[i..j]` because they also
 implement `Index<Range<usize>>`.<br>
One limitation of `IndexMut` is that, by design, it must return a mutable
 reference to some value. This is why you can't use an expression like
 `m["十"] = 10`. The table would need to create an entry for "十" first, with
 some default value, and return a mutable reference to that. But it would be a
 waste to create such a value only to be immediately dropped by the assignment.
 (These are plans to improve this in later versions of the language.)

Not all operators can be overloaded in Rust. As of Rust 1.17, the error-checking
 `?` operator works only with `Result` values. Similarly, the logical operators
 `&&` and `||` are limited to Boolean values only. The `..` operator always
 `Range` values, the `&` operator always borrows references, and the `=`
 operator always moves or copies values. Rust does not support overloading the
 function call operator, `f(x)`. Instead, when you need a callable value, you'll
 typically just write a closure.

## 13. Utility Traits

When a value's owner goes away, we say that Rust *drops* the value.<br>
This implicit invocation of `drop` is the only way to call that method; if you
 try to invoke it explicitly yourself, Rust flags that as an error.<br>
Rust keeps track of the variable's state with an invisible flag indicating
 whether the variable's value need to be dropped or not.<br>
`c_int` is an alias for `i32`. `libc::close` is the Rust name for the C
 library's `close` function. Rust code may call C functions only within `unsafe`
 blocks.<br>
If a type implements `Drop`, it cannot implement the `Copy` trait.<br>
The standard prelude includes a function to drop a value, `drop`, but its
 definition is anything but magical: `fn drop<T>(_x: T) { }`

Because the `str` and `[T]` types denote sets of values of varying sizes, they
 are unsized types. The other common kind of unsized type in Rust is the
 referent of a trait object.<br>
Rust can't store unsized values in variables or pass them as arguments. You can
 only deal with them through pointers like `&str` or `Box<Write>`, which
 themselves are sized.<br>
Trait objects and pointers to slices are nicely symmetrical. In both cases, the
 fat pointer (dynamic information) fills in the information missing from the
 type, carrying a length or a vtable pointer.<br>
Rust implements the `std::marker::Sized` trait automatically for all types to
 which it applies; you can't implement it yourself. The only use for `Sized` is
 a bound for type variables. Traits of this sort are called *marker traits*,
 because the Rust language itself uses them to mark certain types as having
 characteristics of interest.<br>
Since unsized types are so limited, most generic type variables should be
 restricted to `Sized` types. If you do not want to constrain `T` this way, you
 must explicitly opt out, writing `struct S<T: ?Sized> { ... }`. The `?Sized`
 syntax is specific to this case, and means "not necessarily `Sized`." This
 almost always means that the given type is only pointed to, and allows the
 associated code to work with slices and trait objects as well as ordinary
 values. When a type variable has the `?Sized` bound, people often say it is
 *questionably sized*.<br>
A struct type's last field (but only its last) may be unsized, and such a struct
 is itself unsized.

This is why Rust doesn't just clone values automatically, but instead requires
 you to make an explicit method call. The reference-counted pointer types are
 exceptions: cloning one of these simply increments the reference count and
 hands you a new pointer.<br>
These types don't implement `Clone`, since `clone` must be infallible. Instead,
 `std::fs::File` provides a `try_clone` method, which returns a
 `std::io::Result<File>`.

Pointer types like `Box<T>` and `Rc<T>` implement `std::ops::Deref` and
 `std::ops::DerefMut` traits so that they can behave as Rust's built-in pointer
 types do.<br>
Rust uses this to automatically convert references of a `&Self` reference to a
 `&Self::Target` reference. In other words, if inserting a `deref` call would
 prevent a type mismatch, Rust inserts one for you. These are called the *deref
 coercions*: one type is being "coerced" into behaving as another. Rust will
 apply several deref coercions in succession if necessary.<br>
These's no need for `String` to reimplement all of `str`'s methods, since you
 can coerce a `&str` from a `&String`. If you have a vector of bytes `v`, and
 you want to pass it to a function that expects a byte slice `&[u8]`, you can
 simply pass `&v` as the argument, since `Vec<T>` implements
 `Deref<Target=[T]>`.<br>
You should not implement `Deref` and `DerefMut` for a type just to make the
 `Target` type's methods appear on it automatically, the way a C++ base class's
 methods are visible on a subclass. This will not always work as you expect, and
 can be confusing when it goes awry. Rust applies them to resolv type conflicts,
 but not to satisfy bounds on type variables. To work around this problem, you
 can spell out the coercion using the `as` operator:
 `show_it_generic(&s as &str);`

If a type `T` implements `Default`, then the standard library implements
 `Default` automatically for `Rc<T>`, `Arc<T>`, `Box<T>`, `Cell<T>`,
 `RefCell<T>`, `Cow<T>`, `Mutex<T>`, and `RwLock<T>`. If all the element types
 of a tuple type implement `Default`, then the tuple type does too. Rust does
 not implicitly implement `Default` for struct types, but if all of a struct's
 fields implement `Default`, you can implement `Default` for the struct
 automatically using `#[derive(Default)]`. The default value of any `Option<T>`
 is `None`.

`AsRef` is typically used to make functions more flexible in the argument types
 they accept. `open` accepts anything it can borrow a `&Path` from---that is,
 anything that implements `AsRef<Path>`. Such types include `String` and `str`,
 the operating system interface string types `OsString` and `OsStr`, and of
 course `PathBuf` and `Path`.<br>
Fortunately, the standard library includes the blanket implementation (In other
 words, for any types `T` and `U`, if `T: AsRef<U>`, then `&T: AsRef<U>` as
 well):

```rust
impl<'a, T, U> AsRef<U> for &'a T
    where T: AsRef<U>,
          T: ?Sized, U: ?Sized
{
    fn as_ref(&self) -> &U {
        (*self).as_ref()
    }
}
```

`Borrow` imposes more restrictions to `AsRef`: a type should implement
 `Borrow<T>` only when a `&T` hashes and compares the same way as the value it's
 borrowed from. (Rust doesn't enforce this; it's just the documented intent of
 the trait.)<br>
All the standard library's associative collection types use `Borrow` to decide
 which types can be passed to their lookup functions. The standard library
 includes a blanket implementation so that every type `T` can be borrowed from
 itself: `T: Borrow<T>`. This ensures that `&K` is always an acceptable type for
 looking up entries in a `HashMap<K, V>`. As a convenience, every `&mut T` type
 also implements `Borrow<T>`, returning a shared reference `&T` as usual. This
 allows you to pass mutable references to collection lookup functions without
 having to reborrow a shared reference, emulating Rust's usual implicit coercion
 from mutable references to shared references.

```rust
impl HashMap<K, V> where K: Eq + Hash
{
    fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
        where K: Borrow<Q>,
              Q: Eq + Hash
    { ... }
}
```

Whereas the `AsRef` and `AsMut` traits borrow a reference of one type from
 another, `From` and `Into` take ownership of their argument, transform it, and
 then return ownership of the result back to the caller.<br>
The standard library automatically implements the trivial conversion from each
 type to itself: every type `T` implements `From<T>` and `Into<T>`. Given an
 appropriate `From` implementation, the standard library automatically
 implements the corresponding `Into` trait.<br>
Although the traits simply provide two ways to do the same thing, they lend
 themselves to different uses. You generally use `Into` to make your functions
 more flexible in the arguments they accept. The `from` method serves as a
 generic constructor for producing an instance of a type from some other single
 value.<br>
Because the `from` and `into` conversion methods take ownership of their
 arguments, a conversion can reuse the original value's resources to construct
 the converted value. The conversion has no need to allocate or copy the
 text. However, whereas `AsRef` and `AsMut` conversions are expected to be
 cheap, `From` and `Into` conversions may allocate, copy, or otherwise process
 the value's contents.<br>
To provide fallible conversions into or out of your types, it's best to have a
 function or method that returns a `Result` type.

## 14. Closures

The closure is subject to the rules about borrowing and lifetimes. In short,
 Rust ensures safety by using lifetimes instead of garbage collection. Rust's
 way is faster: even a fast GC allocation will be slower than storing `stat` on
 the stack.<br>
The `move` keyword before closures tells Rust that a closure doesn't borrow the
 variable it uses: it steals them.<br>
We get something important by accepting Rust's strict rules: thread safety. It
 is precisely because the vector is moved, rather than being shared across
 threads.<br>
`fn(&City) -> bool    // fn type (functions only)`<br>
`Fn(&City) -> bool    // Fn trait (both functions and closures)`<br>
This special syntax is built into the language. The `->` and return type are
 optional; if omitted, the return type is `()`. A closure is callable, but it's
 not a `fn`. Every closure you write has its own type, because a closure may
 contain data: values either borrowed or stolen from enclosing scopes. No two
 closures have exactly the same type.<br>
Rust's closures are designed to be fast: faster than function pointers, fast
 enough that you can use them even in red-hot, performance-sensitive code. If
 you're familiar with C++ lambdas, you'll find that Rust closures are jsut as
 fast and compact, but safer. In most languages, closures are allocated in the
 heap, dynamically dispatched, and garbage collected. Worse, closures tend to
 rule out inlining.<br>
Closures that `drop` values are not allowed to have `Fn`. They implement a less
 powerful trait, `FnOnce`, the trait of closures that can be called once. For a
 `Fn` closure, `closure(&self)` expands to `closure.call()`. But if the closure
 is only safe to call once, then `closure(self)` expands to
 `closure.call_once()`. Any closure that requires `mut` access to a value, but
 doesn't drop any values, is a `FnMut` closure. `Fn()` is a subtrait of
 `FnMut()`, which is a subtrait of `FnOnce()`.<br>
If you try to use Rust closures to make a "sea of objects," you're going to have
 a hard time.

## 15. Iterators

Rust examines the combined code and recognizes that there're a simpler way to
 sum the numbers from one to `n`.<br>
We call any type that implements `IntoIterator` an *iterable*. All iterators
 automatically implement `IntoIterator`, with an `into_iter` method that simply
 returns the iterator.<br>
If you call an iterator's `next` method again after it has returned `None`, the
 `Iterator` trait doesn't specify what it should do. Most iterators will just
 return `None` again, but not all.<br>
Most collection types provide `iter` and `iter_mut` methods that return the
 natural iterators over the type, producing a shared or mutable reference to
 each item. Slices like `&[T]` and `&str` has `iter` and `iter_mut` methods
 too.<br>
The general principle is that iteration should be efficient and predictable, so
 rather than providing implementations that are expensive or could exhibit
 surprising behavior (for example, rehashing modified `HashSet` entries), Rust
 ommits them entirely.

```rust
// IntoIterator::into_iter
for element in &collection { ... }
for element in &mut collection { ... }
for element in collection { ... }
```

Many collection types provide a `drain` method that takes a mutable reference to
 the collection and returns an iterator that passes ownership of each element to
 the consumer. However, unlike the `into_iter()` method, which takes the
 collection by value and consumes it, `drain` merely borrows a mutable reference
 to the collection, and when the iterator is dropped, it removes any remaining
 elements from the collection and leaves it empty. On types that can be indexed
 by a range, the `drain` method takes a range of elements to remove.

The `Iterator` trait provides a broad selection of *adapter methods,* or simply
 *adapters*, that consume one iterator and build a new one with useful
 behaviors.<br>
Rust RFC 1522 will add syntax to the language very much like our `some Iterator`
 notation. As of Rust 1.17, it is not yet included in the language by
 default.<br>
Iterator adaptors are lazy and do nothing unless consumed.

A `map` iterator passes each item to its closure by value, and in turn, passes
 along ownership of the closure's result to its consumer. A `filter` iterator
 passes each item to its closure by shared reference, retaining ownership in
 case the item is selected to be passed on to its consumer. This is why the
 example must dereferences `s`: the `filter` iterator's item type is `&str`, so
 the type of the closure's argument `s` is `&&str`.<br>
Since `Option` is an iterable behaving like a sequence of zero or one items,
`iterator.filter_map(closure)` is equivalent to `iterator.flat_map(closure)`,
 assuming `closure` returns an `Option<T>`.<br>
The `fuse` adapter takes any iterator and turns into one that will definitely
 continue to return `None` once it has done so the first time.<br>
Adapters take ownership of the underlying iterator, and provide no method to
 give it back. The call `lines.by_ref()` borrows a mutable reference to the
 iterator, and it is this reference that `take_while` iterator takes ownership
 of. That iterator goes out of scope at the end of the first `for` loop,
 meaning that the borrow has ended, so you can use `lines` again in the second
 `for` loop.

```rust
impl<'a, I: Iterator + ?Sized> Iterator for &'a mut I { // in the standard library
    type Item = I::Item;
    fn next(&mut self) -> Option<I::Item> {
        (**self).next()
    }
    // ...
```

The underlying iterator must implement `std::clone::Clone`, so that `cycle` can
 save its initial state and reuse it each time the cycle starts again.

The double reference in `cmp`'s parameters arise because `numbers.iter()`
 produces references to the elements, and then `max_by` and `min_by` pass the
 closure references to the iterator's items.<br>
Although iterators do not support Rust's comparison operators, they do provide
 methods like `eq` and `lt`.<br>
The `chars` iterator on `&str` does not know how many items it will produce in
 advance (UTF-8 is a variable-width encoding), so it can't implement
 `ExactSizeIterator` and you can't use `rposition` on strings.<br>
Calling `.nth(0)` is equivalent to `.next()`. It doesn't take ownership of the
 iterator the way an adapter would, so you can call it many times.

`collect` isn't specific to vectors: in fact, it can build any kind of
 collection from Rust's standard library. When some collection type like `Vec`
 or `HashMap` knows how to construct itself from an iterator, it implements the
 `std::iter::FromIterator` trait, for which `collect` is just a convenient
 veneer.<br>
The standard library provides a blanket implementation of `IntoIterator` for
 every type that implements `Iterator`.

```rust
impl<T> FromIterator<T> for LinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut list = Self::new();
        list.extend(iter);
        list
    }
}
```

## 16. Collections

Invalidation errors are another source of undefined behavior in C++, and they
 cause the occasional `ConcurrentModificationException` even in memory-safe
 languages. This is an invalidation error: the program modifies data while
 iterating over it, *invalidating* the iterator. In Java, the result would be an
 exception; in C++, undefined behavior. In Python, while the behavior is
 well-defined, it's unintuitive: the iterator skips an element. Rust's borrow
 checker rules them out at compile time.<br>
Vector lengths and indices are of type `usize`. Trying to use a `u32`, `u64`,
 or `isize` as a vector index is an error. You can use an `n as usize` cast
 to convert as needed.<br>
`slice.to_vec()` clones a whole slice, returning a new vector. This method is
 available only if the elements are cloneable.<br>
`vec.append(&mut vec2)`, where `vec2` is another vector of type `Vec<T>`, moves
 all elements from `vec2` into `vec`. Afterward, `vec2`is empty. This is like
 `vec.extend(vec2)` except that `vec2` still exists afterward, with its capacity
 unaffected.<br>
Rust has several methods that can borrow `mut` references to two or more parts
 of an array, slice, or vector at once. Unlike the code above, these methods are
 safe, because by design, they split the data into *nonoverlapping* regions.
 Many of these methods are also handy for working with non-`mut` slices.<br>
These sort-key values are not cached during sorting, so the `key` function may
 be called more than *n* times.<br>
For technical reasons, `key(element)` can't return any references borrowed from
 the element. This won't work: `students.sort_by_key(|s| &s.last_name);`<br>
To get similar sort methods that work on floating-point data, use the
 `ord_subset` crate.<br>
like `array.indexOf(value)` in JavaScript, use an iterator:
 `slice.iter().position(|x| *x == value)`<br>
Random numbers are not built into the Rust standard library. The `rand` crate,
 which provides them. It's easy to get a random number generator by calling
 `rand::thread_rng()`.

Because deques don't store their elements contiguously in memory, they don't
 inherit all the methods of slices.<br>
`LinkedList<T>` in memory: front, back, len; (in nodes) next, prev, element<br>
`BinaryHeap` is iterable, and it has an `.iter()` method, but the iterators
 produce the heap's elements in an arbitrary order, not from greatest to
 least.<br>
The Rust standard library uses B-trees rather than balanced binary trees because
 B-trees are faster on modern hardware. A binary tree may use fewer comparisons
 per search than a B-tree, but searching a B-tree has better *locality*.<br>
In general, maps let you have `mut` access to the values stored inside them, but
 not the keys. The values are yours to modify however you like. The keys belong
 to the map itself. Iterating over a set by `mut` reference is not
 supported.<br>
Both `HashMap` and `BTreeMap` have a corresponding `Entry` type. The point of
 entries is to eliminate redundant map lookups.

```rust
let record = student_map.entry(name.to_string()).or_insert_with(Student::new);

// (in std::collections::hash_map)
pub enum Entry<'a, K: 'a, V: 'a> {
    Occupied(OccupiedEntry<'a, K, V>),
    Vacant(VacantEntry<'a, K, V>)
}
```

Rust's two set types, `HashSet<T>` and `BTreeSet<T>`, are implemented as thin
 wrappers around `HashMap<T, ()>` and `BTreeMap<T, ()>`.<br>
Most built-in types that implement `Eq` also implement `Hash`. Tuples, arrays,
 slices, and vectors are all hashable, as long as their elements are hashable.
 Structs and enums don't implement `Hash` by default.<br>
One principle of the standard library is that a value should have the same hash
 code regardless of where you store it or how you point to it. Therefore, a
 reference has the same hash code as the value it refers to, and a `Box` has the
 same hash code as the boxed value. A vector `vec` has the same hash code as the
 slice containing all its data, `&vec[..]`. A `String` has the same hash code as
 a `&str` with the same characters.<br>
If you implement `PartialEq` by hand for a type, you should also implement
 `Hash` by hand.<br>
`HashSet<T>` like all hash tables requires that `hash(a) == hash(b)` if
 `a == b`.<br>
Even when you implement `Hash` by hand, you don't need to know anything about
 hashing algorithms. `.hash()` receives a reference to a `Hasher`, which
 represents the hashing algorithm. You simply feed this `Hasher` all the data
 that's relevant to the `==` operator.<br>
Rust's default hash algorithm is a well-known algorithm called SipHash-1.3.
 SipHash is fast, and it's very good at minimizing hash collisions. In fact,
 it's a cryptographic algorithm: there's no known efficient way to generate
 SipHash-1.3 collisions. As long as a different, unpredictable key is used
 for each hash table, Rust is secure against a kind of denial-of-service
 attack called HashDoS, where attackers deliberately use hash collisions to
 trigger worst-case performance in a setver. If you're storing many small
 keys, such as integers or very short strings, it is possible to implement a
 faster hash function, at the expense of HashDoS security. The `fnv` crate
 implements one such algorithm, the Fowler-Noll-Vo hash.

## 17. Strings and Text

We describe the `String` and `str` types, representing owned and borrowed
 sequences of Unicode characters.<br>
`ch.to_lowercase()` and `ch.to_uppercase()` iterators implement the
 `std::fmt::Display` trait, so you can pass them directly to a `println!` or
 `write!` macro.<br>
The `as` operator will convert any `u8` value to a `char`, and `char` implements
 `From<u8>` as well, but wider integer types can represent invalid code points,
 so for those you must use `std::char::from_u32`, which returns
 `Option<char>`.<br>
These methods index text by byte offsets, and measure its length in bytes,
 rather than characters. In practice, given the nature of Unicode, indexing by
 character is not as useful as it may seem, and byte offsets are faster and
 simpler. If you try to use a byte offset that lands in the midst of some
 character's UTF-8 encoding, the method panics, so you can't introduce
 ill-formed UTF-8 this way. A `String` is implemented as a wrapper around a
 `Vec<u8>` that ensures the vector's contents are always well-formed UTF-8.<br>
`String::new()` has no heap-allocated buffer, but will allocate one as
 needed.<br>
The `&str` type cannot implement `Clone`: the trait requires `clone` on a `&T`
 to return a `T` vsalue, but `str` is unsized. However, `&str` does implement
 `ToOwned`, which lets the implementer specify its owned equivalent, so
 `slice.to_owned()` returns a copy of `slice` as a fresh allocated `String`.<br>
You cannot index a string slice with a single position, like `slice[i]`.
 Fetching a single character at a given byte offset is a bit clumsy: you must
 produce a `chars` iterator over the slice, and ask it to parse one character's
 UTF-8.

The standard library supports four main kinds of patterns:
* A `char`
* A `String` or `&str` or `&&str`
* A `FnMut(char) -> bool`
* A `&[char]` (not a `&str`, but a slice of `char` values). If you write out the
  list as an array literal, you may neeed to use an `as` expression to get the
  type right. Otherwise, Rust will be confused by the fixed-size array type
  `&[char; 2]`, which is unfortunately not a pattern type.

In the library's own code, a pttern is any type that implements the
 `std::str::Pattern` trait. The details of `Pattern` are not yet stable, so you
 can't implement it for your own types in stable Rust.<br>
If a type implements the `std::str::FromStr` trait, then it provides a standard
 way to parse a value from a string slice:
 `fn from_str(s: &str) -> Result<Self, Self::Err>;`<br>
String slices have a `parse` method that parses the slice into whatever type you
 like, assuming it implements `FromStr`.

```rust
let address = IpAddr::from_str("fe80::0000:3ea9:f4ff:fe34:7a50")?;
let address = "fe80::0000:3ea9:f4ff:fe34:7a50".parse::<IpAddr>()?;
```

Types that have a natural human-readable printed form can implement the
 `std::fmt::Display` trait, which lets you use the `{}` format specifier in the
 `format!` macro. All Rust's machine numeric types implement `Display`, as do
 characters, strings and slices. The smart pointer types `Box<T>`, `Rc<T>`, and
 `Arc<T>` implement `Display` if `T` itself does: their displayed form is simply
 that of their referent. Containers like `Vec` and `HashMap` do not implement
 `Display`.<br>
If a type implements `Display`, the standard library automatically implements
 the `std::str::ToString` trait for it, whose sole method `to_string` can be
 more convenient when you don't need the flexivility of `format!`. The
 `ToString` trait predates the introduction of `Display` and is less
 flexible.<br>
Every public type in the standard library implements `std::fmt::Debug`, which
 takes a value and formats it as a string in a way helpful to programmers. The
 easiest way to use `Debug` to produce a string is via the `format!` macro's
 `{:?}` format specifier. All of Rust's collection types have such
 implementations.

The nub of the problem is that sometimes the return value should be an owned
 `String`, sometimes it should be a `&'static str`, and we can't know which
 one it will be until we run the program. This dynamic character is the hint
 to consider using `std::borrow::Cow`, the clone-on-write type that can hold
 either owned or borrowed data. Whether `Owned` or `Borrowed`, a `Cow<'a, T>`
 can always produce a `&T` for you to use. In fact, `Cow<'a, T>` dereferences
 to `&T`, behaving as a kind of smart pointer.<br>
`Cow` is also useful when you may or may not need to modify some text you've
 borrowed. When no changes are necessary, you can continue to borrow it. But
 `Cow`s namesake clone-on-write behavior can give you an owned, mutable copy of
 the value on demand. `Cow`'s `to_mut` method makes sure the `Cow` is
 `Cow::Owned`, applying the value's `ToOwned` implementation if necessary, and
 then returns a mutable reference to the value.<br>
Since `Cow` is frequently used for strings, the standard library has some
 special support for `Cow<'a, str>`. It provides `From` and `Into` conversions
 from both `String` and `&str`. It also implements `std::ops::Add` and
 `std::ops::AddAssign`.

The template string must be a constant, so that Rust can check it against the
 types of the arguments at compile time. Each argument must be used; Rust
 reports a compile-time error otherwise.<br>
You can extend these macros to support your own types by implementing the
 `std::fmt` module's formatting traits. And you can use the `format_args!` macro
 and the `std::fmt::Arguments` type to make your own functions and macros
 support the formatting language.

## 18. Input and Output

## 19. Concurrency

## 20. Macros

## 21. Unsafe Code

