# [The Little Prover][homepage] by Daniel P. Friedman and Carl Eastlund, The MIT Press (2015)

Daniel P. Friedman (professor of computer science at Indiana university, *The
 Little Schemer* (fourth edition), *The Reasoned Schemer*, *The Seasoned
 Schemer*, *[Essentials of Programming Languages][eopl]* (third edition)<br>
Carl Eastlund (software engineer at Jane Street Capital in New York City)

[homepage]: http://the-little-prover.github.io
[eopl]: https://en.wikipedia.org/wiki/Essentials_of_Programming_Languages

## Preface

SLaTeX, ACL2<br>
nine built-in functions - `cons`, `car`, `cdr`, `atom` (`'nil` for non-empty
 lists, `'t` for everything else), `equal`, `natp` (natural number), `size`
 (counts the `cons`es needed to build a value), `+`, `<`

```scheme
(defun list-length (xs)
  (if (atom xs)
      '0
      (+ '1 (list-length (cdr xs)))))

; Since expressions cannot refer to theorems, theorems are not recursive.
(dethm natp/list-length (xs) ; "de"fine a "th"eore"m"
  (natp (list-length xs)))
```

we include a simple proof assitant, J-Bob, defined in the same language as the
 theorems we prove. J-Bob is a program that can check each step when attempting
 to prove a theorem but does *not* contribute any steps.

## 1. Old Game, New Rules

An axiom is a basic assumption that is presumed to be true. A theorem is an
 expression that is always true. Axioms are theorems that are assumed to be
 true, whereas other theorems must be shown to be true.

```scheme
; The Axioms of Cons (initial)
(dethm atom/cons (x y)
  (equal (atom (cons x y)) 'nil))

(dethm car/cons (x y)
  (equal (car (cons x y)) x))

(dethm cdr/cons (x y)
  (equal (cdr (cons x y)) y))

; The Axioms of Equal (initial)
(dethm equal-same (x)
  (equal (equal x x) 't))

(dethm equal-swap (x y)
  (equal (equal x y) (equal y x)))
```

The Law of Dethm (initial)<br>
For any theorem (dethm *name* (*x<sub>1</sub>* ... *x<sub>n</sub>*)
 *body<sub>x</sub>*), the variables *x<sub>1</sub>* ... *x<sub>n</sub>* in
 *body<sub>x</sub>* can be replaced with any corresponding expressions
 *e<sub>1</sub>* ... *e<sub>n</sub>*. The result, *body<sub>e</sub>*, can be
 used to rewrite a focus *p* to become *q* provided *body<sub>e</sub>* is either
 (equal *p* *q*) or (equal *q* *p*).

J-Bob is a program that helps us rewrite one expression to another. J-Bob
 "knows" about all the axioms and the Law of Dethm and makes sure we get all the
 details right.

## 2. Even Older Games

```scheme
; The Axioms of If (initial)
(dethm if-true (x y)
  (equal (if 't x y) x))

(dethm if-false (x y)
  (equal (if 'nil x y) y))

(dethm if-same (x y)
  (equal (if x y y) y))

; The Axioms of Equal (final)
(dethm equal-same (x)
  (equal (equal x x) 't))

(dethm equal-swap (x y)
  (equal (equal x y) (equal y x)))

(dethm equal-if (x y)
  (if (equal x y) (equal x y) 't))
```

`(if Q A E)`  - question, answer, else

The Law of Dethm (final)<br>
For any theorem (dethm *name* (*x<sub>1</sub>* ... *x<sub>n</sub>*)
 *body<sub>x</sub>*), the variables *x<sub>1</sub>* ... *x<sub>n</sub>* in
 *body<sub>x</sub>* can be replaced with any corresponding expressions
 *e<sub>1</sub>* ... *e<sub>n</sub>*. The result, *body<sub>e</sub>*, can be
 used to rewrite a focus as follows:<br>
1. *body<sub>e</sub>* must contain the *conclusion* (equal *p* *q*) or (equal
 *q* *p*),<br>
2. the conclusion must not be found in the question of any `if` or in the
 argument of any function application,<br>
3. and if the conclusion can be found in an `if` answer (respectively else),
 then the focus must be found in an `if` answer (respectively else) with the
 same question.

A *premise* is an `if` question such that a focus can be found in either the
 `if` answer or the `if` else.

```scheme
; The Axioms of Cons (final)
(dethm atom/cons (x y)
  (equal (atom (cons x y)) 'nil))

(dethm car/cons (x y)
  (equal (car (cons x y)) x))

(dethm cdr/cons (x y)
  (equal (cdr (cons x y)) y))

(dethm cons/car+cdr (x)
  (if (atom x) 't (equal (cons (car x) (cdr x)) x)))

; The Axioms of If (final)
(dethm if-true (x y)
  (equal (if 't x y) x))

(dethm if-false (x y)
  (equal (if 'nil x y) y))

(dethm if-same (x y)
  (equal (if x y y) y))

(dethm if-nest-A (x y z)
  (if x (equal (if x y z) y) 't))

(dethm if-nest-E (x y z)
  (if x 't (equal (if x y z) z)))
```

visit A. and B. now.

## 3. What's in a Name?

```scheme
(defun pair (x y)
  (cons x (cons y '())))

(defun first-of (x)
  (car x))

(defun second-of (x)
  (car (cdr x)))

(dethm first-of-pair (a b) ; prove this claim
  (equal (first-of (pair a b)) a))

(dethm second-of-pair (a b) ; prove this claim
  (equal (second-of (pair a b)) b))

(defun in-pair? (xs)
  (if (equal (first-of xs) '?)
      't
      (equal (second-of xs) '?)))

(dethm in-first-of-pair (b) ; prove this claim
  (equal (in-pair? (pair '? b)) 't))

(dethm in-second-of-pair (b) ; prove this claim
  (equal (in-pair? (pair a '?)) 't))
```

A *claim* is an as-yet unproven theorem. We *prove* a claim by writing a proof.
 A *proof* is a sequence of rewriting steps that ends in `'t`. then that claim
 is a theorem.

The Law of Defun (initial)<br>
Given the non-recursive function (defun *name* (*x<sub>1</sub>* ...
 *x<sub>n</sub>*) *body*),<br>
(*name* *e<sub>1</sub>* ... *e<sub>n</sub>*) = *body* where *x<sub>1</sub>* is
 *e<sub>1</sub>*, ..., *x<sub>n</sub>* is *e<sub>n</sub>*.

Insight: Skip Irrelevant Expressions<br>
Rewriting a claim to `'t` does not have to go in any particular order. Some
 parts of the expression might be skipped entirely. For example, `if-same` can
 simplify many `if` expressions to `'t` regardless of the `if` question.

## 4. Part of This Total Breakfast

```scheme
(defun list0? (x)
  (equal x '()))

(defun list1? (x)
  (if (atom x)
      'nil
      (list0? (cdr x))))

(defun list2? (x)
  (if (atom x)
      'nil
      (list1? (cdr x))))
```

all built-in operators are total. We are only concerned with the result of `cdr`
 on `cons`es; `(cdr 'grapefruit)` must have a value, but that is all we need to
 know. `(cdr '())` and `(car '())` are the same. An `if` expression produces
 either its answer or its else no matter what *value* its question has. In other
 words, as long as an `if`'s question, answer, and else have values, the `if`
 expression does too.<br>
`list1?` is total because `atom`, `cdr`, and `list0?` are total, and the `if`'s
 question, answer, and else all have values.

The Law of Defun (final)<br>
Given the total function (defun *name* (*x<sub>1</sub>* ...
 *x<sub>n</sub>*) *body*),<br>
(*name* *e<sub>1</sub>* ... *e<sub>n</sub>*) = *body* where *x<sub>1</sub>* is
 *e<sub>1</sub>*, ..., *x<sub>n</sub>* is *e<sub>n</sub>*.

we can use the Law of Defun for any total function, including all non-recursive
 functions and many recursive functions. Our axioms and laws tell us which
 expressions have equal values. Functions that are not total are called *partial
 functions*. If an expression does not have a value, our axioms and laws do not
 apply.

```scheme
(defun partial (x)
  (if (partial x)
      'nil
      't))

(dethm contradiction ()
  'nil)
```

```scheme
(defun list? (x)
  (if (atom x)
      (equal x '())
      (list? (cdr x))))
```

A *measure* is an expression that is included with a function definition. It may
 only refer to previously defined, total functions and to the function
 definition's formal arguments. The measure must produce a natural number that
 decreases for every recursive call to the function. `list?`'s measure is
 `(size x)`.<br>
the claim that `list?` is total: "if `x` is not an atom, then `(size (cdr x))`
 must be smaller than `(size x)`."<br>
Here is the totality claim:

```scheme
(if (natp (size x))
    (if (atom x)
        't
        (< (size (cdr x)) (size x))
    'nil)

; The Axioms of Size
(dethm natp/size (x)
  (equal (natp (size x)) 't)) ; states that the measure cannot decrease forever

(dethm size/car (x)
  (if (atom x) 't (equal (< (size (car x)) (size x)) 't)))

(dethm size/cdr (x)
  (if (atom x) 't (equal (< (size (cdr x)) (size x)) 't)))
```

Here is the claim that a non-recursive function is total. `'t` (The proof
 completes itself!)<br>
Now we know that many functions that use recursion on lists can be total.

```scheme
(defun sub (x y)
  (if (atom y)
      (if (equal y '?)
          x
          y)
      (cons (sub x (car y))
        (sub x (cdr y)))))
; measure: (size y)

; the claim that sub is total
(if (natp (size y))
    (if (atom y)
        't
        (if (< (size (car y)) (size y))
            (< (size (cdr y)) (size y))
            'nil))
    'nil)
```

## 5. Think It Over, and Over, and Over

```scheme
(defun memb? (xs)
  (if (atom xs)
      'nil
      (if (equal (car xs) '?)
          't
          (memb? (cdr xs)))))

(defun remb (xs)
  (if (atom xs)
      '()
      (if (equal (car xs) '?)
          (remb (cdr xs))
          (cons (car xs)
            (remb (cdr xs))))))

(dethm memb?/remb0 ()
  (equal (memb? (remb '())) 'nil))
(dethm memb?/remb1 (x1)
  (equal (memb?
           (remb (cons x1 '())))
         'nil))
(dethm memb?/remb2 (x1 x2)
  (equal (memb?
           (remb
             (cons x2
               (cons x1 '()))))
         'nil))
; (dethm memb?/remb3 (x1 x2 x3) ...
```

Insight: Rewrite from the Inside Out<br>
Rewrite an expression from the "inside" out, starting inside `if` answers, `if`
 elses, and function arguments. Simplify the arguments of a function application
 as much as possible, then use the Law of Defun to replace the application with
 the function's body. Rewrite `if` questions as necessary to use theorems that
 require premises. Proceed to outer expressions when inner expressions cannot be
 simplified.

```scheme
; If Lifting
(original-context
  (original-focus
    (if Q A E)))
;  = by if-same
(original-context
  (if Q
      (original-focus
        (if Q A E))
      (original-focus
        (if Q A E))))
;  = by if-nest-A and if-nest-E
(original-context
  (if Q
      (original-focus A)
      (original-focus E)))
```

Insight: Pull Ifs Outward<br>
Use If Lifting when an `if` is found in an argument of a function application or
 in an `if` question. Lift the `if` outside any function applications and `if`
 questions.

Insight: Keep Theorems in Mind<br>
Bear existing theorems in mind, especially axioms. When the current claim
 contains an expression that some theorem can rewrite, try using that theorem.
 When the current claim contains *part* of an expression that some theorem can
 rewrite, leave that part alone and try to rewrite the current claim in order to
 use the theorem.

## 6. Think It Through

recursion in proofs is called *induction*. One case for the empty list and
 handle the rest via *natural recursion*. For a list `xs`, the natural recursion
 is the same function call but with `(cdr xs)`. The (natural) recursion for a
 claim is called the *inductive premise*.

```scheme
(dethm memb?/remb (xs)
  (equal (memb? (remb xs)) 'nil))

; inductive claim
(if (atom xs)
    (equal (memb? (remb xs)) 'nil)
    (if (equal (memb? (remb (cdr xs))) 'nil)
        (equal (memb? (remb xs)) 'nil)
        't))
```

Insight: Don't Touch Inductive Premises<br>
Do not try to simplify an inductive premise in an inductive proof directly.
 Instead, rewrite the expression around it until the inductive premise can be
 applied. Often, after applying the inductive premise, an inductive proof is
 nearly done.

Insight: Build Up to Induction Gradually<br>
Build up to a proof by induction over lits by proving theorems about the empty
 list, lists with one element, lists with two element, and so on. Once the
 pattern of these proofs is clear, the proof by induction should be similar.

Proof by List Induction (why???)<br>
To prove a claim *C* by induction over a list named `x`, prove
 (if (atom x) *C* (if *C<sub>cdr</sub>* *C* 't))

## 7. Oh My, Stars!

```scheme
(defun ctx? (x)
  (if (atom x)
      (equal x '?)
      (if (ctx? (car x))
          't
          (ctx? (cdr x)))))
; measure: (size x)

; the claim that if x and y contain '?, then so does (sub x y)
(dethm ctx?/sub (x y)
  (if (ctx? x)
      (if (ctx? y)
          (equal (ctx? (sub x y)) 't)
          't)
      't))

; (helper for induction) the claim that if (ctx? x) is true, it is equal to 't
(dethm ctx?/t (x)
  (if (ctx? x)
      (equal (ctx? x) 't)
      't))
```

Proof by Star Induction (why???)<br>
To prove a claim *C* by induction over `car`s and `cdr`s of a variable named
 `x`, prove
 (if (atom x) *C* (if *C<sub>car</sub>* (if *C<sub>cdr</sub>* *C* 't) 't))

Insight: Combine Ifs<br>
When there are multiple `if`s with the same question, combine them into one `if`
 with If Lifting. Lift the `if`s outside any function applications and `if`
 questions.

Insight: Create Helpers for Induction<br>
To rewrite the application of a recursive function, prove a separate theorem
 about the recursive function using induction. Do this if the current proof
 either does not use induction, or uses induction for a different kind of
 recursion from the function, or uses induction on different arguments from the
 application.

## 8. Learning the Rules

```scheme
(defun member? (x ys)
  (if (atom ys)
      'nil
      (if (equal x (car ys))
          't
          (member? x (cdr ys)))))
; measure: (size ys)
; totality claim
(if (natp (size ys))
    (if (atom ys)
        't
        (if (equal x (car ys))
            't
            (< (size (cdr ys)) (size ys))))
    'nil)

(defun set? (xs)
  (if (atom xs)
      't
      (if (member? (car xs) (cdr xs))
          'nil
          (set? (cdr xs)))))
; measure: (size xs)
; totality claim
(if (natp (size xs))
    (if (atom xs)
        't
        (if (member? (car xs) (cdr xs))
            't
            (< (size (cdr xs)) (size xs))))
    'nil)

(defun add-atoms (x ys)
  (if (atom x)
      (if (member? x ys)
          ys
          (cons x ys))
      (add-atoms (car x)
        (add-atoms (cdr x) ys))))
; measure: (size x)
; totality claim
(if (natp (size x))
    (if (atom x)
        't
        (if (< (size (car x)) (size x))
            (< (size (cdr x)) (size x))
            'nil))
    'nil)
```

A totality claim tells us that the function's measure decreases on every
 recursive call.

Conjunction<br>
The *conjunction* of expressions *e<sub>1</sub>* ... *e<sub>n</sub>* states that
 each of *e<sub>1</sub>* ... *e<sub>n</sub>* must be true.<br>
The conjunction of zero expressions is `'t`.<br>
The conjunction of one expression *e<sub>1</sub>* is *e<sub>1</sub>*.<br>
The conjunction of *e<sub>1</sub>* and *e<sub>2</sub>* is *e<sub>1</sub>* if
 *e<sub>2</sub>* is `'t`, it is *e<sub>2</sub>* if *e<sub>1</sub>* is `'t`, and
 otherwise it is (if *e<sub>1</sub>* *e<sub>2</sub>* 'nil).<br>
The conjunction of three or more expressions *e<sub>1</sub>* *e<sub>2</sub>* ...
 *e<sub>n</sub>* is the conjunction of *e<sub>1</sub>* and the conjunction of
 *e<sub>2</sub>* ... *e<sub>n</sub>*.

Constructing Totality Claims<br>
Given a function (defun *name* (*x<sub>1</sub>* .. *x<sub>n</sub>*) body) and a
 measure *m*, construct a claim for subexpressions in *body*:<br>
For variables and quoted literals, use `'t`.<br>
For (if *Q* *A* *E*) where the claims for *Q*, *A*, and *E* are *c<sub>q</sub>*,
 *c<sub>a</sub>*, and *c<sub>e</sub>*, if *c<sub>a</sub>* and *c<sub>e</sub>*
 are the same use the conjunction of *c<sub>q</sub>* and *c<sub>a</sub>*,
 otherwise use the conjunction of *c<sub>q</sub>* and (if *Q* *c<sub>a</sub>*
 *c<sub>e</sub>*).<br>
For any other expression *E*, consider each recursive application (*name*
 *e<sub>1</sub>* ... *e<sub>n</sub>*) in *E*. Construct the measure
 *m<sub>r</sub>* of the recursive application by substituting *e<sub>1</sub>*
 for *x<sub>1</sub>*, ..., *e<sub>n</sub>* for *x<sub>n</sub>* in *m*. The claim
 for *E* is the conjunction of (< *m<sub>r</sub>* *m*) for every recursive
 application in *E*.<br>
The totality claim for *name* us the conjunction of (natp *m*) and the claim for
 *body*.

## 9. Changing the Rules

## 10. The Stars Are Aligned

## A. Recess

(J-Bob usage)<br>
The first argument to `J-Bob/step` is a list of representations of definitions,
 in this case `(prelude)` representing J-Bob's axioms and initial functions. The
 second argument represents an expression to rewrite. The third argument is a
 list of steps, processed first to last, to rewrite the expression. That list is
 the *path* to the focus. The path is a list of directions from the current
 expression to the subexpression representing the focus in the pending rewrite.

```scheme
(J-Bob/step (prelude)
  '(if a c c)
  '((() (if-same a c))
    (()
     (if-same
       (if (equal a 't)
           (if (equal 'nil 'nil)
               a
               b)
           (equal 'or
                  (cons 'black '(coffee))))
       c))
    ((Q E 2) (cons 'black '(coffee)))))
```

The first argument to `J-Bob/prove` is a list of definitions, the second
 argument is a list of *proof attempts*. Each proof attempt starts with a
 definition and a *seed*. A seed is extra information outside of the definition
 used to generate the claim that must be proved.

```scheme
(J-Bob/prove (prelude)
  '()) ; ''t

(J-Bob/prove (prelude)
  '(((defun pair (x y)
       (cons x (cons y '())))
     nil))) ; ''t

(J-Bob/prove (prelude)
; ''nil if there is no definitions or it is recursive,
; expression from the last unfinished proof attempt
  '(((defun pair (x y)
       (cons x (cons y '())))
     nil)
    ((defun first-of (x)
       (car x))
     nil)
    ((defun second-of (x)
       (car (cdr x)))
     nil)
; up to here, 't (proof attempts succeed) because given defuns are non-recursive
    ((defun first-of-pair (a b)
       (equal (first-of (pair a b)) a))
     nil
; up to here, '(equal ...) because we have not proved the therom
     ((1 1) (pair a b))
     ((1) (first-of (cons a (cons b '()))))
     ((1) (car/cons a (cons b '())))
     (() (equal-same a)))))
; ''t

(defun prelude+first-of-pair ()
  (J-Bob/define (prelude)
; produces a list of all the theorems we know or have proved so far
    '(((defun pair (x y)
         (cons x (cons y '())))
       nil)
      ((defun first-of (x)
         (car x))
       nil)
      ((defun second-of (x)
         (car (cdr x)))
       nil)
      ((defun first-of-pair (a b)
         (equal (first-of (pair a b)) a))
       nil
       ((1 1) (pair a b))
       ((1) (first-of (cons a (cons b '()))))
       ((1) (car/cons a (cons b '())))
       (() (equal-same a)))))

(J-Bob/prove (prelude) ; or (prelude+first-of-pair)
  '(((defun list? (x)
       (if (atom x)
           (equal x '())
           (list? (cdr x))))
     (size x) ; seed like (list-induction xs) and (star-induction y)
; up to here
; '(if (natp (size x))
;      (if (atom x)
;          't
;          (< (size (cdr x)) (size x))
;      'nil))
     ((Q) (natp/size x))
     (()
      (if-true
        (if (atom x)
            't
            (< (size (cdr x)) (size x)))
        'nil))
     ((E) (size/cdr x))
     (() (if-same (atom x) 't)))))
; ''t
```

## B. The Proof of the Pudding

(J-Bob applications to examples in each chapter)

## C. The Little Assistant

(J-Bob implementation)<br>
ACL2 and Scheme implementations<br>
179

## D. Restless for More?

Nqthm (by J Moore and Bob Boyer) -> [ACL2][acl2] ([Dracula][dracula] user interface, [ACL2 Sedan][acl2_sedan] Eclipse plug-in)<br>
[Agda][agda], [Coq][coq], [Isabelle/HOL][isabelle_hol], [PVS][pvs], [Twelf][twelf]<br>
recommended books

[acl2]: http://www.cs.utexas.edu/users/moore/acl2/
[dracula]: http://dracula-lang.github.io
[acl2_sedan]: http://acl2s.ccs.neu.edu/acl2s/doc/
[agda]: http://wiki.portal.chalmers.se/agda/
[coq]: https://coq.inria.fr/
[isabelle_hol]: http://www.cl.cam.ac.uk/research/hvg/Isabelle/
[pvs]: http://pvs.csl.sri.com/
[twelf]: http://twelf.org/

