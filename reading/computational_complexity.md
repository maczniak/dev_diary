# [Computational Complexity: A Conceptual Perspective][homepage] by [Oded Goldreich][author], Cambridge University Press (2008)

The P versus NP Question can be phrased as asking whether or not finding
 solutions is harder than checking the correctness of solutions. An alternative
 formulation asks whether or not discovering proofs is harder than verifying
 their correctness, that is, is proving harder than verifying (membership). A
 problem (in NP) is NP-complete if any problem in NP is reducible to it.

[homepage]: http://www.wisdom.weizmann.ac.il/~oded/cc-book.html
[author]: http://www.wisdom.weizmann.ac.il/~oded/

## 1 Introduction and Preliminaries

Is this experience merely a coincidence or does it represent a fundamental fact
 of life? The denial of the plausibility of such a hypothetical world is what "P
 different from NP" actually means, where P represents tasks that are
 efficiently solvable and NP represents tasks for which solutions can be
 efficiently checked.<br>
This means that the notion of a "proof" is meaningful; that is, proofs do help
 when one is seeking to be convinced of the correctness of assertions. Here NP
 represents sets of assertions that can be efficiently verified with the help of
 adequate proofs, and P represents sets of assertions that can be efficiently
 verified from scratch.<br>
NP-completeness indicates not only the conjectured intractability of a problem
 but also its "richness" in the sense that the problem is rich enough to
 "encode" any other problem in NP.<br>
The foregoing discussion of NP-completeness hints at *the importance of
 representation*, since it referred to different problems that encode one
 another. In general, Complexity Theory is concerned with problems for which the
 solutions are implicit in the problem's statement. Indeed, a solution to
 computational problem is merely a different representation of the informaiton
 given, that is, a representation in which the answer is explicit rather than
 implicit.<br>
In general, Complexity Theory provides new viewpoints on various phenomena that
 were considered also by past thinkers. Examples include the aforementioned
 concepts of solutions, proofs, and representation as well as concepts like
 randomness, knowledge, interaction, secrecy, and learning.<br>
A complexity assumption implies the existence of deterministic programs (called
 *pseudorandom generators*) that stretch short random seeds into long
 pseudorandom sequences. In fact, it turns out that the existence of
 pseudorandom generators is equivalent to the existence of one-way
 functions.<br>
Complexity Theory offers its own perspective on the concept of *knowledge* (and
 distinguishes it from information). Specifically, Complexity Theory views
 knowledge as the result of a hard computation. Thus, whatever can be
 efficiently done by anyone is not considered knowledge.<br>
The foregoing paragraph has explicitly referred to *interaction*, viewing it as
 a vehicle for gaining knowledge and/or gaining confidence. Let us highlight the
 latter application by noting that it may be easier to verify an assertion when
 allowed to interact with a prover rather than when reading a proof. The added
 power of such *interactive proofs* is rooted in their being randomized (i.e.,
 the verification procedure is randomized), because if the verifier's questions
 can be determined beforehand then the prover may just provide the transcript of
 the interaction as a traditional written proof.<br>
In some sense knowledge is a secret. Much of cryptography is based on complexity
 theoretic assumptions and its results are typically transformations of
 relatively simple computational primitives (e.g., one-way functions) into more
 complex cryptographic applications (e.g., secure encryption schemes).<br>
We have already mentioned two fundamental types of tasks: *searching for
 solutions* (or rather "finding solutions") and *making decisions* (e.g.,
 regarding the validity of assertions). Now we consider two additional types of
 tasks: *counting the number of solutions* and *generating random solutions*.
 Under some natural conditions on the problem, approximately counting the number
 of solutions and generating an approximately random solution is not
 significantly harder than finding an arbitrary solution.<br>
The study of the complexity of finding "approximate solutions" is also of
 natural importance. Rather than finding a solution that attains the optimal
 value, the approximation task consists of finding a solution that attains an
 "almost optimal" value. Interestingly, in many cases, even a very relaxed level
 of approximation is as difficult to obtain as solving the original (exact)
 search problem. Surprisingly, these hardness-of-approximation results are
 related to the study of *probabilistically checkable proofs*, which are proofs
 that allow for ultra-fast probabilistic verification. Amazingly, every proof
 can be efficiently transformed into one that allows for probabilistic
 verification based on probing a *constant* number of bits (in the alleged
 proof).<br>
In view of the central role of randomness in Complexity Theory, one specific
 question, which received a lot of attention, is the possibility of "purifying"
 randomness (or "extracting good randomness from bad sources"). This study
 turned out to be related to Complexity Theory, where the most tight connection
 is between some type of *randomness extraxtors* and some type of pseudorandom
 generators.<br>
The study of space complexity has uncovered several fascinating phenomena, which
 seem to indicate a fundamental difference between space complexity and time
 complexity. For example, in the context of space complexity, verifying proofs
 of validity of assertions (of any specific type) has the same complexity as
 verifying proofs of invalidity for the same type of assertions.<br>
One aspect that makes Complexity Theory unique is its perspective on the most
 basic question of the theory of computation, that is, the way it studies the
 question of *what can be efficiently computed*. In most cases, complexity
 theoretic studies do not refer to any specific computational problems.<br>
Indeed, the most conceptually appealing phrasing of the P-vs-NP Question
 consists of asking whether every search problem in \mathcal PC can be solved
 efficiently.

In a sense, a computation merely transforms the problem instance to its
 solution. Indeed, the answer to any fully specified question is implicit in the
 question itself, and computation is employed to make this answer explicit.<br>
Complexity Theory is focused at what computers cannot do, or rather with drawing
 the line between what can be done and what cannot be done.<br>
A `computation` is a process that modifies an environment via repeated
 applications of a predetermined ("simple") `rule`. Our starting point is a
 desired functionality, and our aim is to design computation rules (`algorithm`)
 that effect it.<br>
The model of Turing machines is not meant to provide an accurate (or "tight")
 model of real-life computers, but rather to capture their inherent limitations
 and abilities. (Church-Turing "Thesis", due to *reasonable and general* model
 of computation)<br>
The extension of the model to multi-tape Turing machine is crucial to the
 definition of space complexity. A less fundamental advantage of the model of
 multi-tape Turing machines is that it facilitates the design of machnies that
 compute functions of interest.<br>
(undecidability of the halting problem): The halting function is not
 computable.<br>
... algorithm that solves one problem (`d'`) by using as a subroutine an
 algorhtum that solves another provlem (`d` or `h`). The "natural" ("positive")
 meaning of a Turing-reduction (such an algorithmic scheme) of *f'* to *f* is
 that, when given an algorithm for computing *f*, we obtain an algorithm for
 computing *f'*. ... "unnatural" ("negative") counter-positive: If (as we know)
 there exists no algorithm for computing *f'* = `d'` then there exists no
 algorithm for computing *f* = `d` (which is what we wanted to prove).
 Resource-bounded Turing-reductions (e.g., polynomial-time reductions) play a
 central role in Complexity Theory itself, and again they are used mostly in a
 "negative" way.<br>
Rice's Theorem: The undecidability of the halting problem (or rather the fact
 that the function `d` is uncomputable) is a special case of a more general
 phenomenon: Every non-trivial decision problem *regarding the function computed
 by a given Turing machine* has no algorithmic solution. The relevance of this
 assertion to the project of program verification is obvious.<br>
A `universal Turing machine` *is a Turing machine that on input a description of
 a machine M and an input x returns the value of M(x) if M halts on x and
 otherwise does not halt.* The existence of a universal machine asserts that it
 is enough to build one physical device, that is, a general purpose computer.
 Thus, (fixed) universal machines correspond to general-purpose computers, and
 provide the basis for separating hardware from software (can be viewed as
 input).<br>
Kolmogorov Complexity is concerned with the length of (efective, succinct)
 descriptions of objects, and views the minimum such length as the inherent
 "complexity" of the object. The complexity of *s* with respect to *M*, denoted
 *K*<sub>*M*</sub>(*s*), is the length of the shortest description of *s* with
 respect to *M*, and furthermore such that this description is not
 "significantly" longer than the description with respect to a different machine
 *M'*.<br>
The Cobham-Edmonds Thesis asserts that a problem has time complexity t in some
 "reasonable and general" models of computation if and only if it has time
 complexity poly(t) in the model of (single-tape) Turing machines.<br>
An oracle machine is a machine that is augmented such that it may pose questions
 (queries) to the outside (oracle). An oracle machine is a Turing machine with a
 special additional tape, called oracle tape, and two special states, called
 oracle invocation and oracle spoke.<br>
Restricted Model is the model of finite automata, which in some variant
 coincides with Turing machnies that have space-complexity zero (equiv.,
 constant). We reject the common coupling of computability theory with the
 theory of automata and formal languages.<br>
Non-uniform Models (Circuits and Advice)<br>
Each Boolean function has at most exponential (O(n2<sup>n</sup>) look-up table)
 circuit complexity. Families of circuits can compute functions that are
 uncomputable by algorithms.<br>
A graph with *v* vertices and *e* edges can be described by a string of length
 2*e*log<sub>2</sub>*v*.<br>
One often says that a set *S* ⊆ {0, 1}<sup>\*</sup> is in the class \mathcal C
 rather than saying that *the problem of deciding membership in S* is in the
 class \methcal C. Likewise, one talks of classes of relations rather than
 classes of the corresponding search problems (i.e., saying that *R* ⊆ {0,
 1}<sup>\*</sup> × {0, 1}<sup>\*</sup> is in the class \mathcal C means that
 *the search problem of R* is in the class \mathcal C).

It is quite remarkable that the theories of uniform and non-uniform
 computational devices have emerged in two single papers. We refer to Turing's
 paper, which introduced the model of Turing machines, and to Shannon's paper,
 which introduced Boolean circuits.

## 2 P, NP, and NP-Completeness

The best we can expect is a conditional proof that the said problem is not in P,
 based on the assumption that NP is different from P.<br>
We stress that NP-complete problems are not the only hard problems in NP (i.e.,
 if P is different than NP than NP contains problems that are neither
 NP-complete nor in P). We also note that the P-vs-NP Question is not about
 inventing sophisticated algorithms or ruling out their existence, but rather
 boils down to the analysis of a single known algorthm.

Another natural question captured by the P versus NP Question is whether proving
 theorems is harder than verifying the validity of these proofs. In other words,
 the question is whether deciding membership in a set is harder than being
 convinced of this membershiup by an adequate proof.<br>
The standard technical definition of NP refers to the fictitious and confusing
 device called a non-deterministric polynomial-time machine.<br>
\mathcal PF (Polynomial-time Find), \mathcal PC (Polynomial-time Check)<br>
We warn that \mathcal PF contains (unnatural) problems that are not in \mathcal
 PC.<br>
(P versus NP Question) Is it the case that every problem in \mathcal PC is in
 \mathcal PF?<br>
We say that *V* is a verification procedure for membership in *S* (or a witness
 relation of *S*) if it satisfies the following two conditions: completeness
 (for every *x* ∈ *S* there exists a string *y* such that *V*(x, y) = 1) and
 soundness (for every *x* ∉ *S* and every string *y* it holds that *V*(x, y) =
 0) / Such a string *y* is called an NP-witness for *x* ∈ *S* and we say that
 *S* has an NP-proof system.<br>
We note that the soundness condition captures the "security" of the verification
 procedure, that is, its ability not to be fooled into proclaiming a wrong
 assertion. The completeness condition captures the "viability" of the
 verification procedure, that is, its ability to be convinced of any valid
 assertion, when presented with an adequate proof.

## 3 Variations on P and NP

## 4 More Resoueces, More Power?

## 5 Space Complexity

## 6 Randomness and Counting

## 7 The Bright Side of Hardness

## 8 Pseudorandom Generators

## 9 Probabilistic Proof Systems

## 10 Relaxing the Requirements

## Appendix A: Glossary of Complexity Classes

## Appendix B: On the Quest for Lower Bounds

## Appendix C: On the Foundations of Modern Cryptography

## Appendix D: Probabilistic Preliminaries and Advanced Topics in Randomization

## Appendix E: Explicit Constructions

## Appendix F: Some Omitted Proofs

## Appendix G: Some Computational Problems

