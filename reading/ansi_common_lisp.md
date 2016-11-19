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

## 5. Control

## 6. Functions

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

