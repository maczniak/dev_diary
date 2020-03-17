# [A Programmer's Introduction to Mathematics][homepage] by [Jeremy Kun][jeremy_kun], CreateSpace (2018)

[homepage]: https://pimbook.org/
[jeremy_kun]: https://jeremykun.com/

Except for a few informal cameos, we ignore complex numbers, probability and
 statistics, differential equations, and formal logic. In my humble opinion,
 none of these topics is as fundamental for mathematical computer science as
 those I've chosen to cover.<br>
The first step on your journey is to confirm that mathematics has a culture
 worth becoming acquainted with.

## 1 Like Programming, Mathematics has a Culture

The problem is that the culture of mathematics and the culture of mathematics
 education---elementary through lower-lovel college courses---are completely
 different.<br>
I worked at a "blockchain" company that implemented a Bitcoin wallet, which is
 based on elliptic curve cryptography.<br>
Advanced books also lean toward terseness, despite being titled as "elementary"
 or an "introduction." They opt not to redefine what they think the reader must
 already know. The purest fields of mathematics take a sort of pretentious pride
 in how abstract and compact their work is (to the point where many students
 spend weeks or months understanding a single chapter!).<br>
Leibniz, one of the inventors of calculus, dreamed of a machine that could
 automatically solve mathematical problems. Ada Lovelace designed the first
 program for computing Bernoulli numbers. In the early 1900's Hilbert posed his
 Tenth Problem on algorithms for computing solutions to Diophantine equations,
 and later his Entscheidungsproblem, which was solved concurrently by Church and
 Turing and directly led to Turing's code-breaking computer.

## 2 Polynomials

learn about how to "share secrets" using something called *polynomial
 interpolation*.<br>
Let's analyze the content of this definition in three ways: syntactically,
 semantically, and culturally (which includes the unspoken expectations of the
 reader upon encountering a definition in the wild).<br>
If you're truly interested in how real numbers are defined from scratch, Chapter
 29 of Spivak's text *Calculus* is devoted to a gold-standard treatment.<br>
Polynomials are expressive enough to capture all of boolean logic.<br>
The most important cultural expectation, one every mathematician knows, is that
 the second you see a definition in a text **you must immediately write down
 examples.**<br>
To avoid this, we amend Definition 2.1 so that the last coefficient
 *a*<sub>*n*</sub> is required to be nonzero. But then the function *f*(*x*) = 0
 is not allowed to be a polynomial! So, by convention, we define a special
 exception, the function *f*(*x*) = 0, as the *zero polynomial*. By convention,
 the zero polynomial is defined to have degree -1.<br>
Every time a definition includes the phrase "by convention," it becomes a
 special edge-case in the resulting program.<br>
The Z stands for Zahlen, the German word for "numbers."<br>
Functional programmers will know this (product) pattern well, because both are a
 "fold" (or "reduce") function with a particular choice of binary operation and
 initial value.<br>
The best default assumption is that the author is far smarter than we are, and
 if we the reader don't understand something, it's likely a user error and not a
 bug. In the occasional event that the author is wrong, it's more often than not
 a simple mistake or typo, to which an experienced reader would say, "The author
 obviously meant 'foo' because otherwise none of this makes sense," and continue
 unscathed.<br>
The square □ is called a *tombstone* and marks the end of a proof.<br>
We've shown the existence and uniqueness of a degree *n* polynomial passing
 through a given list of *n* + 1 points. It's called the *interpolating
 polynomial* of the given points. The verb *interpolate* means to take a list of
 points and find the unique minimum-degree polynomial passing through them.<br>
If one of the daughters sees the shares of two others before revealing her own,
 she could compute a share that produces whatever "decoded message" she wants,
 such as a will giving her the entire inheritance! This property of being able
 to decode any possible plaintext given an encrypted text is called *perfect
 secrecy*, and it's an early topic on a long journey through mathematical
 cryptography.

## 3 On Pace and Patience

## 4 Sets

## 5 Variable Names, Overloading, and Your Brain

## 6 Graphs

## 7 The Many Subcultures of Mathematics

## 8 Calculus with One Variable

## 9 On Types and Tail Calls

## 10 Linear Algebra

## 11 Live and Learn Linear Algebra (Again)

## 12 Eigenvectors and Eigenvalues

## 13 Rigor and Formality

## 14 Multivariable Calculus and Optimization

## 15 The Argument for Big-O Notation

## 16 Groups

## 17 A New Interface

