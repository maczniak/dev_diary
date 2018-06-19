# [Modern Cryptography and Elliptic Curves: A Beginner's Guide][homepage] by [Thomas R. Shemanske][author], American Mathematical Society (2017)

[homepage]: https://bookstore.ams.org/stml-83/
[author]: https://www.math.dartmouth.edu/~trs/

## Chapter 1. Three Motivating Problems

This goal of this book is to explain how the set of points on an elliptic curve
 can be given the structure of an abelian group, and how the arithmetic of
 elliptic curves over finite fields can be used as a powerful tool in
 cryptography and cryptoanalysis.<br>
Ken Ribet proved the epsilon conjecture in 1986 reducing the Fermat theorem to a
 proof of the Taniyama-Shimura conjecture for a special class of elliptic
 curves. In 1994, Andrew Wiles succeeded in proving the required case of the
 Taniyama-Shimura conjecture, and hence proving Fermat's Last Theorem.<br>
There is a clear relationship between congruent numbers and Pythagorean triples,
 which means if we had a way to lsit all Pythagorean triples, we would know
 which numbers were congruent numbers.<br>
Elliptic curve cryptography turns out to be among the best public-key
 cryptosystems currently in use.

## Chapter 2. Back to the Beginning

It turns out that the key to listing all the triples is to characterize all the
 rational points on the unit circle, *x*<sup>2</sup> + *y*<sup>2</sup> = 1. This
 represents an important first example of how the algebra and geometry of a
 curve can combine to answer questions that are seemingly quite unrelated.<br>
Conversely, if any of the pairs {*A*, *B*}, {*A*, *C*}, or {*B*, *C*} are
 relatively prime, then *A*, *B*, *C* must obviously divide any two.

Let's see how some of these ideas play out with the *Fermat curve*,
 *x*<sup>*n*</sup> + *y*<sup>*n*</sup> = *z*<sup>*n*</sup>, *n* > 2, for which,
 in the context of Fermat's Last Theorem, we are seeking integer solutions to
 this equation.<br>
The set of complex points on an elliptic curve is naturally associated to a
 torus (doughnut); other curves, a sphere or a geometric structure which looks
 like several tori glued together, sort of like fat links in a chain. There is a
 geometric invariant associated to such surfaces called the *genus* of the
 surface, and the genus refers to the number of holes in the surface.<br>
In 1922 Mordell made a conjecture that, for any curve of genus at least 2, the
 set of rational points on the curve is finite, and in 1983 Gerd Faltings proved
 Mordell's conjecture. So given our knowledge that there are infinitely many
 rational points on *x*<sup>2</sup> + *y*<sup>2</sup> = 1, we know for sure it
 has genus 0 or 1. But the real import of Falting's theorem is that for *n* > 4,
 the curve *u*<sup>*n*</sup> + *v*<sup>*n*</sup> = 1 has genus ≥ 2.

## Chapter 3. Some Elementary Number Theory

## Chapter 4. A Second View of Modular Arithmetic: Z<sub>*n*</sub> and *U*<sub>*n*</sub>

## Chapter 5. Public-Key Cryptography and RSA

## Chapter 6. A Little More Algebra

## Chapter 7. Curves in Affine and Projective Space

## Chapter 8. Applications of Elliptic Curves

## Chapter A. Deeper Results and Concluding Thoughts

## Chapter B. Answers to Selected Exercises

