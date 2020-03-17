# [Complexity Theory: Exploring the Limits of Efficient Algorithms][homepage] by Ingo Wegener, translated by Randall Pruim, Springer (2005)

[errata][errata]

[homepage]: https://www.springer.com/gp/book/9783540210450
[errata]: http://ls2-www.cs.tu-dortmund.de/grav/en/monographs/ct

## 1 Introduction

Since this text is also especially directed toward those who do not wish to make
 complexity theory their specialty, results that do not (yet) have a connection
 to algorithmic applications will be omitted.<br>
Complexity theory must provide *lower bounds* for the minimally necessary
 resource requirements that *every* algorithm that solves the problem must use.
 For the proof of an upper bound, it is sufficient to design and analyze a
 *single* algorithm. Every lower bound, on the other hand, is a statement about
 *all* algorithms that solve a particular problem.<br>
None of the most important problems in complexity theory have been solved, but
 along the way to answering the central questions many notable results have been
 achieved.<br>
Complexity theory has in the NP ≠ P-problem a central challenge. Many results in
 complexity theory assume solidly based but unproven hypotheses, such as NP ≠ P.

## 2 Algorithmic Problems & Their Complexity

By an algorithmic problem, we mean a problem that is suitable for processing by
 computers and for which the set of correct results is unambiguous.<br>
Since formally in each case we are looking for one of potentially many correct
 answers, we refer to such a problem as a *search problem*. If, as in the case
 of the search for a shortest path, we are looking for a solution that is in
 some way optimal, we will refer to the problem as an *optimization problem*.
 Often, it is sufficient to compute the value of an optimal solution (e.g., the
 length of a shortest path). These variants are called *evaluation problems*. In
 the special case when the only possible answers are 0 (no) and 1 (yes), and we
 must decide which of these two possibilities is correct, we speak of a
 *decision problem*.

* traveling salesperson problems;
* knapsack problems (the best selection of objects);
* partitioning problems (bin packing problems, scheduling problems);
* surveillance (or covering) problems;
* clique problems;
* team building problems;
* optimization of flows in networks;
* championship problems in sports leagues;
* verfication problems; and (→ satisfiability problem)
* number theoretic problems (primality testing, factoring).

In general, we shall see that, historically, new subareas of complexity theory
 have always begun with the investigation of new satisfiability problems.<br>
The classical Church-Turing Thesis says that all models of computation can
 simulate one another, so that the set of algorithmically solvable problems is
 independent of the model of computation. The Extended Church-Turing Thesis goes
 one step further: For any two models of computation R<sub>1</sub> and
 R<sub>2</sub> there is a polynomial p such that t computation steps of
 R<sub>1</sub> on an input of length n can be simulated by p(t, n) computation
 steps of R<sub>2</sub>.

## 3 Fundamental Complexity Classes

## 4 Reductions --- Algorithmic Relationships Between Problems

## 5 The Theory of NP-Completeness

## 6 NP-complete and NP-equivalent Problems

## 7 The Complexity Analysis of Problems

## 8 The Complexity of Approximation Problems --- Classical Results

## 9 The Complexity of Black Box Problems

## 10 Additional Complexity Classes

## 11 Interactive Proofs

## 12 The PCP Theorem and the Complexity of Approximation Problems

## 13 Further Topics From Classical Complexity Theory

## 14 The Complexity of Non-uniform Problems

## 15 Communication Complexity

## 16 The Complexity of Boolean Functions

## A Appendix

For probabilities *p*(*n*), it often matters how fast these converge to 0 or 1.
 (*polynomially small*, *exponentially small*, ...)

