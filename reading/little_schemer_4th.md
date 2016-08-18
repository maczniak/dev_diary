# [The Little Schemer, Fourth Edition][homepage], by Daniel P. Friedman and Matthias Felleisen, The MIT Press (1995)

[homepage]: https://mitpress.mit.edu/books/little-schemer

The goal of this book is to teach the reader to think recursively.<br>
The key to learning Scheme is "pattern recognition."

## 1. Toys

`(quote atom)` or `'atom`<br>
all atoms and lists are S-expressions.<br>
the car of a non-empty list is the first S-expression of the list.<br>
`#t`, `#f`<br>
`(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))`<br>

The Five Rules
* The Law of Car - The primitive *car* is defined only for non-empty lists.
* The Law of Cdr /could-er/ - The primitive *cdr* is defined only for non-empty lists. The *cdr* of any non-empty list is always another list.
* The Law of Cons - The primitive *cons* takes two arguments. The second argument to *cons* must be a list. The result is a list.
* The Law of Null? - The primitive *null?* is defined only for lists.
* The Law of Eq? - The primitive *eq?* takes two arguments. Each must be a non-numeric atom.

`(cons 'a 'b)` == `(a . b)`?

## 2. Do It, Do It Again, and Again, and Again ...

lat is a list of atoms.<br>
The First Commandment - (preliminary) Always ask *null?* as the first question in expressing any function.

## 3. Cons the Magnificent

The Second Commandment - Use *cons* to build lists.<br>
The Third Commandment - When building a list, describe the first typical element, and then *cons* it onto the natural recursion.<br>
The Fourth Commandment - (preliminary) Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using *cdr*, test termination with *null?*.

## 4. Numbers Games

typical element, terminal condition, natural recursion<br>
tup(le) is a list of numbers.<br>
The First Commandment - (first revision) When recurring on a list of atoms, *lat*, ask two questions about it: *(null? lat)* and else. / When recurring on a number, *n*, ask two question about it: *(zero? n)* and else.<br>
The Fourth Commandment - (first revision) Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using *cdr*, test termination with *null?* and when using *sub1*, test termination with *zero?*.<br>
The Fifth Commandment - When building a value with ＋, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition. / When building a value with ✕, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication. / When building a value with *cons*, always consider () for the value of the terminating line.<br>
*number?*, like *add1*, *sub1*, *zero?*, *car*, *cdr*, *cons*, *null?*, *eq?*, and *atom?*, is a primitive function.<br>
use = for numbers and *eq?* for all other atoms.

## 5. *Oh My Gawd*: It's Full of Stars

The First Commandment - (final version) When recurring on a list of atoms, *lat*, ask two questions about it: *(null? lat)* and else. / When recurring on a number, *n*, ask two question about it: *(zero? n)* and else. / When recurring on a list of S-expression, *l*, ask three question about it: *(null? l)*, *(atom? (car l))*, and else.<br>
The Fourth Commandment - (final version) Always change at least one argument while recurring. When recurring on a list of atoms, *lat*, use *(cdr lat)*. When recurring on a number, *n*, use *(sub1 n)*. And when recurring on a list of S-expressions, *l*, use *(car l)* and *(cdr l)* if neither *(null? l)* nor *(atom? (car l))* are true. / It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using *cdr*, test termination with *null?* and when using *sub1*, test termination with *zero?*.<br>
An S-expression is either an atom or a (possibly empty) list of S-expressions.<br>
The Sixth Commandment - Simplify only after the function is correct.

## 6. Shadows

## 7. Friends and Relations

## 8. Lambda and Ultimate

## 9. ... and Again, and Again, and Again, ...

## 10. What Is the Value of All of This?

