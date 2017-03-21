# [A Beginner's Guide to Mathematical Logic][homepage] by Raymond M. Smullyan, Dover (2014)

[Raymond Merrill Smullyan][raymond_smullyan_wikipedia]
 [[KO][raymond_smullyan_wikipedia_ko]] (May 25, 1919 -- February 6, 2017). He is
 one of many logicians to have studied under Alonzo Church.

[homepage]: http://store.doverpublications.com/0486492370.html
[raymond_smullyan_wikipedia]: https://en.wikipedia.org/wiki/Raymond_Smullyan
[raymond_smullyan_wikipedia_ko]: https://ko.wikipedia.org/wiki/%EB%A0%88%EC%9D%B4%EB%A8%BC%EB%93%9C_%EC%8A%A4%EB%A9%80%EB%A6%AC%EC%96%B8

## Part I. General Background

### Chapter 1. Genesis

However, there is one purpose (of mathematical logic) that I can tell you right
 now, and that is to make precise the notion of a *proof*.<br>
Aristotle introduced the notion of the syllogism. It is important to understand
 the difference between a syllogism being *valid* and a syllogism being *sound*.
 A valid syllogism is one in which the conclusion is a logical consequence of
 the premises, regardless of whether the premises are true or not. A *sound*
 syllogism is a syllogism which is not only valid, but in which, in addition,
 the premises are true.<br>
we shall henceforth regard it as true that for *any* property P, all elements of
 the empty set have property P.<br>
In other words, "*p* implies *q*" is synonymous with "it is not the case that
 *p* is true and *q* is false". This type of implication is more specifically
 called *material implication*, and it does have the strange property that a
 false proposition implies any proposition!
One well-known way is by use of Venn diagrams, but I have found another way
 [Smullyan, 2007], the method of *indexing*. In general, for any *n* equal or
 greater than 2, the sets *A*<sub>1</sub>, *A*<sub>2</sub>, ..., *A*<sub>n</sub>
 divide *I* into 2<sup>2</sup> basic regions.<br>
*A* → *B*, which is *A'* ∪ *B*<br>
A ≡ B, which is (*A* ∩ *B*) ∪ (*A'* ∩ *B'*)

### Chapter 2. Infinite Sets

A curious thing about infinite sets is that each of them can be put into a 1-1
 correspondence with a *proper* subset of itself!<br>
A set is called *denumerable*, or *denumerably infinite*, if it can be put into
 a 1-1 correspondence with the set of positive integers, or what is the same
 thing, when it can be put into a 1-1 correspondence with the set of natural
 numbers.<br>
Cantor's great discovery is that the set of all sets of positive integers is
 *not* denumerable! (but the set of finite sets of positive integers is
 denumerable.) We now turn to the proof of this crucial fact, which started the
 whole mathematical field known as *set theory*.<br>
For any set *A*,the set of all subsets of *A* is called the *power set* of *A*,
 and is denoted 𝒫(*A*).<br>
**Cantor's Theorem.** For any set *A*, the power set 𝒫(*A*) is numerically
 larger than *A*.<br>
It is known that the set 𝒫(*N*) is the same size as the set of all points on a
 straight line, and accordingly the set 𝒫(*N*) is known as *the continuum*.
 Cantor conjectured that there are no intermediate size, and this conjecture is
 known as *the continuum hypothesis*. More generally, Cantor conjectured that
 for every infinite set *A*, there is no set whose size is intermediate between
 that of *A* and that of 𝒫(*A*) and this conjecture is known as the *generalized
 continuum hypothesis*. To this day, no one knows whether the continuum
 hypothesis, in either form, is true or false. In the late 1930's Kurt Gödel
 showes that in the most powerful mathematical system known today the continuum
 hypothesis is not disprovable. Then, in the 1960's, Paul Cohen showed that the
 continuum hypothesis is not provable in the same system. Both the continuum
 hypothesis and its negation --- each is consistent with the axioms of
 present-day set theory.<br>
**The Bernstein Schroeder Theorem.** For any pair of infinite sets *A* and *B*,
 if *A* can be put into a 1-1 correspondence with a part of *B* (i.e. a subset
 of *B*) and *B* can be put into a 1-1 correspondence with a part of *A*, then
 the whole of *A* can be put into a 1-1 correspondence with the whole of *B*.

### Chapter 3. Some Problems Arise!

Shortly after the start of Cantor's Set Theory, some paradoxes arose that
 threatened the validity of the whole theory of infinite sets! One such paradox
 is this: Consider the set *S* of all sets. S is the same size as 𝒫(*S*),
 contrary to Cantor's Theorem.<br>
Next there is the famous Russell Paradox (discovered indepedently by Zermelo).
 Call a set *ordinary* if it is not a member of itself, and *extraordinary* if
 it is a member of itself. Let *M* be the set of all ordinary sets. Is *M*
 ordinary or not? Either way we get a contradiction. Russell subsequently made a
 popular version of this paradox. A male barber of a certain town shaved all men
 of the town who did not shave themselves, and only such men. The answer to the
 paradox is that there was no such barber.<br>
Then there is the *Berry* paradox. THE SMALLEST NUMBER NOT DESCRIBABLE IN LESS
 THAN ELEVEN WORDS. The above description uses only ten words! The answer is
 that the notion of *describable* is not well-defined.<br>

### Chapter 4. Further Background

## Part II. Propositional Logic

### Chapter 5. Beginning Propositional Logic

### Chapter 6. Propositional Tableaux

### Chapter 7. Axiomatic Propositional Logic

## Part III. First-Order Logic

### Chapter 8. Beginning First-Order Logic

### Chapter 9. First-Order Logic: Main Topics

## Part IV. The Incompleteness Phenomenon

### Chapter 10. Incompleteness in a General Setting

### Chapter 11. Elementary Arithmetic

### Chapter 12. Formal Systems

### Chapter 13. Peano Arithmetic

### Chapter 14. Further Topics

