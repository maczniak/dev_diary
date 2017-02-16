# [Certified Programming with Dependent Types][homepage], by Adam Chlipala, The MIT Press (2013)

latest draft available under [Creative Commons License][cc_by_nc_nd_3_0], [book source][book_source]<br>
Coq versions 8.4pl5, 8.5beta2

[homepage]: http://adam.chlipala.net/cpdt/
[cc_by_nc_nd_3_0]: http://creativecommons.org/licenses/by-nc-nd/3.0/
[book_source]: http://adam.chlipala.net/cpdt/cpdt.tgz

### 1. Introduction

mechanized theorem proving, machine-checked proof, certified program<br>
[ACL2][acl2], [Coq][coq], [Isabelle/HOL][isabelle_hol], [PVS][pvs], [Twelf][twelf], ([Agda][agda], [Epigram][epigram])<br>
Coq - higher-order functional programming language, dependent types, easy-to-check kernel proof language (that satisfies the "de Bruijn criterion"), convenient programmable proof automation, proof by reflection<br>
A language with *dependent types* may include references to programs inside of
 types. For instance, the type of an array might include a program expression
 giving the size of the array, making it possible to verify absence of
 out-of-bounds accesses statically. Dependent types can go even further than
 this, effectively capturing any correctness property in a type.<br>
Dependent types are useful not only because they help you express correctness
 properties in types. Dependent types also often let you write certified
 programs without writing anything that looks like a proof. Writing formal
 proofs is hard, so we want to avoid it as far as possible.<br>
Coq includes programs and proof terms in the same syntactic class. This makes
 it easy to write programs that compute proofs. With rich enough dependent
 types, such programs are *certified decision procedures*. In such cases, these
 certified procedures can be put to good use *without ever running them*! The
 critical ingredient for this technique, many of whose instances are referred
 to as proof by reflection, is a way of inducing non-trivial computation inside
 of logical propositions during proof checking.<br>
made by coqdoc, [Proof General][proof_general] mode for Emacs, CoqIDE by the Coq team<br>
Coq manual, *Interactive Theorem Proving and Program Development*, [*Software Foundations*][software_foundations]<br>
[exercises][coq_exercises], `CpdtTactics.v`, `coqtop -R DIR/src Cpdt`

[acl2]: http://www.cs.utexas.edu/users/moore/acl2/
[coq]: http://coq.inria.fr/
[isabelle_hol]: http://isabelle.in.tum.de/
[pvs]: http://pvs.csl.sri.com/
[twelf]: http://www.twelf.org/
[agda]: http://wiki.portal.chalmers.se/agda/agda.php
[epigram]: https://code.google.com/p/epigram/
[proof_general]: https://proofgeneral.github.io/
[software_foundations]: http://www.cis.upenn.edu/~bcpierce/sf/
[coq_exercises]: http://adam.chlipala.net/cpdt/ex/

### 2. Some Quick Examples

`StackMachine.v`<br>
Every Coq source file is a series of vernacular commands, where many command
 forms take arguments that are Gallina or Ltac programs.<br>
in Coq we are free to choose between these and many other orders of evaluation,
 because all Coq programs terminate.<br>
The second restriction is not lifted by GADTs. In ML and Haskell, indices of
 types must be types and may not be *expressions*. In Coq, types may be indexed
 by arbitrary Gallina terms. Type indices can live in the same universe as
 programs, and we can compute with them just like regular programs. Haskell
 supports a hobbled form of computation in type indices based on multi-parameter
 type classes, and recent extensions like type functions bring Haskell
 programming even closer to “real” functional programming with types, but,
 without dependent typing, there must always be a gap between how one programs
 with types and how one programs normally.<br>
`Set`, the type of types of programs, is itself a first-class type, and we can
 write functions that return `Set`s.<br>
type `unit`, which has just a single value, `tt`

## I. Basic Programming and Proving

### 3. Introducing Inductive Types

`InductiveTypes.v`

### 4. Inductive Predicates

`Predicates.v`

### 5. Infinite Data and Proofs

`Coinductive.v`

## II. Programming with Dependent Types

### 6. Subset Types and Variations

`Subset.v`

### 7. General Recursion

`GeneralRec.v`

### 8. More Dependent Types

`MoreDep.v`

### 9. Dependent Data Structures

`DataStruct.v`

### 10. Reasoning About Equality Proofs

`Equality.v`

### 11. Generic Programming

`Generic.v`

### 12. Universes and Axioms

`Universes.v`

## III. Proof Engineering

### 13. Proof Search by Logic Programming

`LogicProg.v`

### 14. Proof Search in Ltac

`Match.v`

### 15. Proof by Reflection

`Reflection.v`

## IV. The Big Picture

### 16. Proving in the Large

`Large.v`

### 17. A Taste of Reasoning About Programming Language Syntax

`ProgLang.v`

