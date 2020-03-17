# [Computational Complexity: A Modern Approach][homepage] by Sanjeev Arora and Boaz Barak, Cambridge University Press (2009)

It quickly became clear that computation can happen in diverse physical and
 mathematical systems---Turing machines, lambda calculus, cellular automata,
 pointer machines, bouncing biliards balls, Conway's *Game of life*, and so on.
 Surprisingly, all these forms of computation are equivalent. / Generalizing
 from physical models such as cellular automata, scientists now view many
 natural phenomena as akin to computational processes. The understanding of
 reproduction in living things was triggered by the discovery of
 self-reproduction in computational machines. Today, computational models
 underlie many research areas in biology and neuroscience. Several physics
 theories such as QED give a description of nature that is very reminiscent of
 computation, motivating some scientists to even suggest that the entire
 universe may be viewed as a giant computer.<br>
We do have important nonexistence results in some concrete computational models
 that are not as powerful as general computers.<br>
Chapters 19 and 20 show a surprising recent result giving strong evidence that
 randomness does *not* help speed up computation too much, in the sense that any
 probabilistic algorithm can be replaced with a *deterministic* algorithm
 (tossing no coins) that is almost as efficient.<br>
Do interactive proofs, involving a dialog between prover and verifier, have more
 power than standard "static" mathematical proofs? The notion of *proof*,
 central to mathematics, turns out to be central to computational complexity as
 well, and complexity has shed new light on the meaning of mathematical proofs.
 Whether mathematical proofs can be generated automatically turns out to depend
 on the P vs. NP question. Chapter 11 describes *probabilistically checkable
 proofs*. These are surprisingly robust mathematical proofs that can be checked
 simply by reading them in very few probabilistically chosen locations, in
 contrast to the traditional proofs that require line-by-line verification.
 Finally, *proof complexity*, a subfield of complexity studying the minimal
 proof length of various statements, is studied in Chapter 15. In a surprising
 twist, computational complexity has also been used to prove some
 metamathematical theorems.

[homepage]: https://theory.cs.princeton.edu/complexity/

### 0 Notational conventions

*h*(*n*) = *n*<sup>*O*(1)</sup> = poly(*n*)

## PART ONE: BASIC COMPLEXITY CLASSES

### 1 The computational model---and why it doesn't matter

The class **P** will be a starting point for definitions of many other models,
 including nondeterministic, probabilistic, and quantum Turing machines, Boolean
 circuits, parallel computers, decision trees, and communication games.<br>
The most basic version of the model can *simulate* the most complicated version
 with at most polynomial (actually quadratic) slowdown. Thus *t* steps on the
 complicated model can be simulated in *O*(*t*<sup>*c*</sup>) steps on the
 weaker model where *c* is a constant depending only on the two models. /
 (efficient universal Turing machine) If *M*<sub>*α*</sub> halts on input *x*
 within *T* steps then 𝒰(*x*,*α*) halts within *CT*log*T* steps, where *C* is a
 number independent of |*x*| and depending only on *M*<sub>*α*</sub>'s alphabet
 size, number of tapes, and number of states.<br>
An algorithm (i.e., a machine) can be represented as a bit string once we decide
 on some canonical encoding. Thus an algorithm/machine can be viewed as a
 possible *input* to another algorithm---this makes the boundary between
 *input*, *software*, and *hardware* very fluid.<br>
There is a *universal* Turing machine 𝒰 that can *simulate* any other Turing
 machine given its bit representation. This simulation is very efficient: If the
 running time of *M*<sub>*α*</sub> was *T*(|*x*|), then the running time of 𝒰 is
 *O*(*T*(|*x*|)log*T*(|*x*|)).<br>
The previous two facts can be used to easily prove the existence of functions
 that are not computable by any Turing machine. There exists a function `UC`:
 {0, 1}<sup>*</sup> → {0, 1} that is not computable by any TM.<br>
We showed that computing `UC` is *reducible* to computing `HALT`---we showed
 that if there were a hypothetical algorithm for `HALT` then there would be one
 for `UC`.

Given a set of polynomial equations with integer coefficients, find out whether
 these equations have an integer solution. This is known as the problem of
 solving Diophantine equations, and in 1900 Hilbert mentioned finding an
 algorithm for solving it as one of the top 23 open problems in mathematics.<br>
In the year 1900, David Hilbert, the preeminent mathematician of his time,
 proposed an ambitious agenda to base all of mathematics on solid axiomatic
 foundations, so that eventually all true statements would be rigorously proven.
 Mathematicians such as Russell, Whitehead, Zermelo, and Fraenkel, proposed
 axiomatic systems in the ensuing decades, but nobody was able to prove that
 their systems are simultaneously *complete* (i.e., prove all true mathematical
 statements) and *sound* (i.e., prove no false statements). In 1931, Kurt Gödel
 shocked the mathematical world by showing that this ongoing effort is doomed to
 fail---for every sound system 𝒮 of axioms and rules of inference, there exist
 true number theoretic statements that cannot be proven in 𝒮.<br>
Now if the system is complete, it must prove at least one of
 *Φ*<sub>⟨*α*,*x*⟩</sub> or ¬*Φ*<sub>⟨*α*,*x*⟩</sub>, and if it is sound, it
 cannot prove both. So if the system is both complete and sound, the following
 algorithm for the Halting problem is guaranteed to terminate in finite time for
 all inputs.<br>
Note that this procedure implicitly uses the simple fact that proofs in
 axiomatic systems can be easily verified by a Turing machine, since each step
 in the proof has to follow mechanically from previous steps by applying the
 axioms.

The class **P** contains only decision problems.<br>
Most scientists believe the **Church-Turing (CT) thesis**, which states that
 every physically realizable computation device---whether it's based on silicon,
 DNA, neurons or some other alien technology---can be simulated by a Turing
 machine. When it comes to *efficiently* computable problems, the situation is
 less clear. The **strong form of the CT thesis** says that every physically
 realizable computation model can be simulated by a TM *with polynomial
 overhead*. However, this strong form is somewhat controversial, in particular
 because of models such as *quantum computers*, which do not appear to be
 efficiently simulatable on TMs.<br>
Also quantum computers currently seem able to efficiently solve only very few
 problems that are not known to be in **P**.

### 2 NP amd NP completeness

A complexity class **NP** aims to capture the set of problems whose solutions
 can be efficiently *verified*. **NP** problems seem to capture some aspects of
 "creativity" in problem solving, and such creativity could become accessible to
 computers if **P = NP**.<br>
Some texts use the term *witness* instead of *certificate*.<br>
The class **NP** can also be defined using a variant of Turing machine called
 *nondeterministic* Turing machines (abbreviated NDTM). In fact, this was the
 original definition, and the reason for the name **NP**, which stands for
 *nondeterministic polynomial time*. The only difference between an NDTM and a
 standard TM is that an NDTM has *two* transition functions δ<sub>0</sub> and
 δ<sub>1</sub>, and a special state denoted by *q*<sub>`accept`</sub>. If
 *every* sequence of choices makes *M* halt without reaching
 *q*<sub>`accept`</sub>, then we say that *M*(*x*) = 0.<br>
In fact, using nondeterminism, we can even make the simulation by a universal TM
 slightly more efficient.<br>
One should not that, unlike standard TMs, NDTMs are not intended to model any
 physically realizable computation device.

It turns out that the independent set problem is at least as "hard" as any other
 language in **NP**. This fascinating property is called **NP**-*hardness*.<br>
*L′* is **NP**-*complete* if *L′* is **NP**-hard and *L′* ∈ **NP**. To study the
 **P** versus **NP** question it suffices to study whether any **NP**-complete
 problem can be decided in polynomial time.<br>
Some texts use the names "many-to-one reducibility" or "polynomial-time mapping
 reducibility" instead of "polynomial-time Karp reducibility."<br>

### 3 Diagonalization

### 4 Space complexity

### 5 The polynomial hierarchy and alternations

### 6 Boolean circuits

### 7 Randomized computation

### 8 Interactive proofs

### 9 Cryptography

### 10 Quantum computation

### 11 PCP theorem and hardness of approximation: An introduction

## PART TWO: LOWER BOUNDS FOR CONCRETE COMPUTATIONAL MODELS

### 12 Decision trees

### 13 Communication complexity

### 14 Circuit lower bounds: Complexity theory's Waterloo

### 15 Proof complexity

### 16 Algebraic computation models

## PART THREE: ADVANCED TOPICS

### 17 Complexity of counting

### 18 Average case complexity: Levin's theory

### 19 Hardness amplification and error-correcting codes

### 20 Derandomization

### 21 Pseudorandom constructions: Expanders and extractors

### 22 Proofs of PCP theorems and the Fourier transform technique

### 23 Why are circuit lower bounds so difficult?

### Appendix: Mathematical background

