# [Logic for Computer Science: Foundations of Automatic Theorem Proving, Second Edition][homepage] by Jean H. Gallier, Dover (2015)

proof theory and procedures for algorithmic construction of formal proofs,
 formalization of proofs, automatic theorem proving<br>
... who convinced me that there is no better way to really know a topic than
 writing about it.

[homepage]: http://www.cis.upenn.edu/~jean/gbooks/logic.html

## Chapter 1: INTRODUCTION

Roughly speaking, Gödel's completeness theorem asserts that there are logical
 calculi in which every true formula is provable, and Church's theorem (Church's
 undecidability of validity) asserts that there is no decision procedure
 (procedure which always terminates) for deciding whether a formula is true
 (valid). Hench, any algorithmic procedure for testing whether a formula is true
 (or equivalently, by Gödel's completeness theorem, provable in a complete
 system) must run forever when given certain non-true formulae as input.<br>
In spite of the theoretical limitation imposed by Church's result, the goal of
 automatic theorem proving (for short, atp) is to find *efficient* algorithmic
 methods for finding proofs of those formulae that are true. A fairly intuitive
 method for finding such algorithms is the completeness proof for Gentzen-like
 sequent calculi. This approach yields a complete procedure (the *search*
 procedure) for proving valid formulae of first-order logic. Because the
 *search* procedure is not practical, we will try improve it or find more
 efficient proof procedures. For this, we will analyze the structure of proofs
 carefully. Fundamental results of Gentzen and Herbrand show that if a formula
 is provable, then it has a proof having a certain form, called a *normal* form.
 The existence of such normal forms can be exploited to reduce the size of the
 search space that needs to be explored in trying to find a proof. The existence
 of normal forms is also fundamental because it reduces the problem of finding a
 proof of a first-order formula to the problem of finding a proof of a simpler
 type of formula, called a proposition. Indeed, there are algorithms for
 deciding truth. One of the methods based on this reduction technique is the
 *resolution method*. Besides looking for general methods applying to the class
 of all true (first-order) formulae, it is interesting to consider subclasses
 for which simpler or more efficient proof procedures exist.

## Chapter 2: MATHEMATICAL PRELIMINARIES

2.1 relations, functions, partial orders, induction<br>
2.2 tree domains and trees<br>
2.3 inductive definitions

## Chapter 3: PROPOSITIONAL LOGIC

use of a logical language - proof theory, model theory<br>
A statement true under all interpretations of the parmeters is said to be valid.

## Chapter 4: RESOLUTION IN PROPOSITIONAL LOGIC

## Chapter 5: FIRST-ORDER LOGIC

## Chapter 6: GENTZEN'S CUT ELIMINATION THEOREM AND APPLICATIONS

## Chapter 7: GENTZEN'S SHARPENED HAUPTSATZ; HERBRAND'S THEOREM

## Chapter 8: RESOLUTION IN FIRST-ORDER LOGIC

## Chapter 9: SLD-RESOLUTION AND LOGIC PROGRAMMING (PROLOG)

## Chapter 10: MANY-SORTED FIRST-ORDER LOGIC

## APPENDIX

