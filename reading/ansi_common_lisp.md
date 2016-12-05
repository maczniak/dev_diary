# [ANSI Common Lisp][homepage], by Paul Graham, Prentice Hall (1995)

[errata][errata]

[homepage]: http://www.paulgraham.com/acl.html
[errata]: http://www.paulgraham.com/ancomliser.html

Programming should be fun. Programs should be beautiful.

## 1. Introduction

## 2. Welcome to Lisp

Not all the operators in Common Lisp are functions, but most are.<br>
`()`, `nil`<br>
Although `t` is the default representation for truth, everything except `nil` (false) also counts as true in a logical context.<br>
The logical operators `and` and `or` resemble conditionals. Both take any number of arguments, but only evaluate as many as they need to in order to decide what to return. These two operators are *macros*.<br>
Parentheses are not an issue, because programmers read and write Lisp by indentation.<br>
When we are writing code without side-effects, there is no point in defining functions with bodies of more than one expression.

`(defparameter *glob* 99)`, `(defconstant limit (+ *glob* 1))`<br>
When the first argiment to `setf` is a symbol that is not the name of a local variable, it is taken to be a global variable.<br>
`(setf (car x) 'n)`, The arguments of "settable" operators can be almost any expression that refers to a particular place.<br>
```
(setf a b
      c d
      e f)
```

*Functional programming* means writing programs that work by returning values, instead of by modifying things.

`do` macro - until the termination test is succeeded
```
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

> (function +)
#<Compiled-Function + 17BA4E>

(apply #'+ 1 2 '(3 4 5)) ; can be given any number of arguments, so long as the last is a list
(funcall #' 1 2 3)
```

`'` = `quote`, `#'` = `function`<br>
The `lambda` in a lambda expression is not an operator. It is just a symbol.
In Common Lisp, `lambda` is no longer really necessary. Common Lisp retained it
for the sake of tradition.<br>
In Common Lisp, values have types, not variables. This approach is called
*manifest typing*. You don't have to declare the types of variables, because
any variable can hold objects of any type.<br>
The built-in Common Lisp types form a hierarchy of subtypes and supertypes.
An object always has more than one type. For example, the number `27` is of
type `fixnum`, `integer`, `rational`, `real`, `number`, `atom`, and `t`, in
order of increasing generality. The type `t` is the supertype of all types, so
everything is of type `t`.

## 3. Lists

`nil` is both an atom and a list.<br>
While `eql` returns true only if its arguments are the same object, `equal`,
essentially, returns true if its arguments would print the same.<br>
The reason Lisp has no pointers is that every value is conceptually a
pointer.<br>
`(load "compress.lisp")`<br>
Common Lisp includes functions for operating on trees not because one needs
trees as such, but because one needs a way to do something to a list and all
the lists within it. Functions that operate on trees usually have this form,
recursing down both the `car` and `cdr`. Such functions are said to be *doubly
recursive*.<br>
Keyword arguments are always optional. If any are included in a call, they come
last, if more than one keyword argument is given, their order doesn't
matter.<br>
The function `adjoin` is like a conditional `cons`. It takes an object and a
list, and conses the object onto the list only if it is not already a member.
The `pushnew` macro is a variant of `push` that uses `adjoin` instead of
`cons`.<br>
In Common Lisp, *sequences* include both lists and vectors.<br>
You have to be careful when using `sort`, because it's *destructive*. For
efficiency reasons, `sort` is allowed to modify the sequence given to it as an
argument. So if you don't want your original sequence modified, pass a copy
(`copy-list`).<br>
`(setf pair (cons 'a 'b)) ; (A . B)` Because this cons is not a proper list, it
is displayed in *dot notation*. A cons that isn't a proper list is called a
*dotted list*. Conses that aren't proper lists are usually not meant to
represent lists at all: `(a . b)` is just a two-part data structure.<br>
`(a . (b . (c . nil))) ; (A B C)`<br>
A list of conses is called an *assoc-list* or *alist*.
`(assoc '+ '((+ . "add") (- . "subtract")))`<br>
Allocating memory from the heap is sometimes generically known as *consing*.
The trouble with consing is, allocating storage and scavenging memory to
reclaim it can be expensive compared to the routine operations of a program.
You can avoid some of this consing by using destructive functions, which try to
reuse most of the structure of the lists passed to them as arguments. But it's
hard to give general advice about consing, because some Lisp implementations
now do memory management so well that it can sometimes be faster to cons than
not to.<br>
Actually, we do have a way of reaching the list, for a bit. The globals `*`,
`**`, and `***` are always set to the last three values returned to the
toplevel. These variables are useful in debugging.

## 4. Specialized Data Structures

```
(make-array '(2 3) :initial-element nil))
(setf (aref arr 0 0) 'b)
#2a((b nil nil) (nil nil nil))
(setf *print-array* t)
(make-array 4 :initial-element nil))
(vector "a" 'b 3) ; #("a" B 3)
(svref vec 0) ; "sv" means "simple vector".
```

A simple array is one that is neither adjustable, nor displaced, nor has a
fill-pointer. Arrays are simple by default.<br>
Some Lisp programmers use multiple semicolons to indicate the level of the
comment: four semicolons in a heading, three in a description of a function or
macro, two to explain the line below, and one when a comment is on the same
line as the code it applies to.<br>
Strings are vectors of characters. We denote a constant string as a series of
characters surrounded by double-quotes, and an individual character *c* as
`#\c`.<br>
`char` (faster than `aref`), `string-equal` (case-insensitive),`(format nil ...)` (returns a string), `(concatenate 'string "not " "to worry")`<br>
In Common Lisp the type `sequence` includes both lists and vectors (and
therefore strings).<br>
See chapter 5 for `do`, `dotimes` and `when`.

```
(defstruct point ; It also implicitly defines the functions make-point,
  x              ; point-p, copy-point, point-x, and point-y. Each point will
  y)             ; be of type point, then structure, then atom, then t.
> (setf p (make-point :x 0 :y 0))
#S(POINT X 0 Y 0)
> (setf (point-y p) 2)

; with default expressions
(defstruct polemic
  (type (progn
          (format t "What kind of polemic was it? ")
          (read)))
  (effect nil))

(defstruct (point (:conc-name p) ; -> "p"x and "p"y
                  (:print-function print-point))
  (x 0)
  (y 0))
(defun print-point (p stream depth)
  (format stream "#<~A,~A>" (px p) (py p)))
; print-object takes the first two arguments.
; A macro print-unreadable-object displays objects in #<...> syntax.

; "and" and "or" usage
(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))
```

```
; hash tables
(setf ht (make-hash-table))
(setf (gethash 'color ht) 'red)
; Most implementations will display all the return values of a call made at the
; toplevel, but code that expects only one return value will get just the first.
```

## 5. Control

Since only the value of the last expression is returned, the use of `progn` (or
any block) implies side-effects.<br>
Many Common Lisp operators that take a body of expressions implicitly enclose
the body in a `block` named `nil`. The body of a function defined with `defun`
is implicitly enclosed in a `block` with the same name as the function.<br>
Thi operator is mainly something that other operators are built upon, not
something you would use yourself. Most iteration operators have an implicit
`tagbody`, so it's possible (through rarely desirable) to use labels and `go`
within their bodies.<br>
Entering a new lexical context is conceptually equivalent to a function
call.<br>
The function `mapc` is like `mapcar` but does not cons up a new list as a
return value, so the only reason to use it is for side-effects. It always
returns its second argument.<br>
The maximum number of return values is implementation-dependent, but it will be
at least 19. The `values` function returns multiple values. It returns exactly
the values you give it as arguments. To receive multiple values, we use
`multiple-value-bind`.<br>
An `unwind-protect` takes any number of arguments and returns the value of the
first. However, the remaining expressions will be evaluated even if the
evaluation of the first is interrupted.

```
> (setf x 1)
1
> (catch 'abort
    (unwind-protect
      (throw 'abort 99)
      (setf x 2)))
99
> x
2
```

## 6. Functions

```
(setf (symbol-function 'add2) ; (defun add2 (x) (+ x 2))
      #'(lambda (x) (+ x 2)))
```

By making the first argument to `defun` a list of the form
<code>(setf *f*)</code>, you define what happens when the first argument to
`setf` is a call to *f*. In the definition of a function whose name is of the
form <code>(setf *f*)</code>, the first parameter represents the new value, and
the remaining parameters represent arguments to *f*. It's not necessary to
define `primo` in order to define `(setf primo)`, but such definitions usually
come in pairs.

```
(defun primo (lst) (car lst))
(defun (setf primo) (val lst)
  (setf (car lst) val))
```

A `do` expression can be similarly explained as a call to a recursive
function.<br>
Local functions can be defined with `labels`, which is a kind of `let` for
functions.<br>
When a function refers to a variable defined outside it, it's called a *free*
variable. A function that refers to a free lexical variable is called a
*closure*. (The name "closure" is left over from earlier Lisp dialects. It
derives from the way closures have to be implemented under dynamic scope.) The
variable must persist as long as the function does. A closure is a combination
of a function and an environment. Closures are created implicitly whenever a
function refers to something from the surrounding lexical environment. We can
even make several closures share variables.<br>
`nconc` = Haskell `concat`<br>
The real distinction here is between lexical variables, which have lexical
scope, and special variables, which have dynamic scope. But it's almost the
same distinction, because local variables are nearly always lexical variables,
and global variables are always special variables. Under lexical scope, a
symbol refers to the variable that has that name in the context where the
symbol appears, regardless of any name that might exist where a function is
called. With dynamic scope, we look for a variable in the environment where the
function is called, not in the environment where it was defined. To cause a
variable to have dynamic scope, we must declare it to be `special` in any
context where it occurs. Global variables established by called `setf` at the
toplevel are implicitly special.<br>
A `declare` can begin any body of code where new variables are created. The
`special` declaration is unique, in that it can change the way a program
behaves. All other declarations are simply advice to the compiler.

## 7. Input and Output

## 8. Symbols

## 9. Numbers

## 10. Macros

## 11. CLOS

## 12. Structure

## 13. Speed

## 14. Advanced Topics

## 15. Example: Inference

## 16. Example: Generating HTML

## 17. Example: Objects

## A. Debugging

## B. Lisp in Lisp

## C. Changes to Common Lisp

## D. Language Reference

