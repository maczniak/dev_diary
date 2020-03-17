# [Elliptic Curves: Number Theory and Cryptography (Seond edition)][homepage] by Lawrence C. Washington, Chapman and Hall/CRC (2008)

[homepage]: http://www.math.umd.edu/~lcw/ellipticcurves.html

## 1 Introduction

The question of which integers *n* can occur as areas of right triangles with
 rational sides is known as the **congruent number problem**. Another formation
 is whether there are three rational squares in arithmetic progression with
 difference *n*.<br>
An integer *n* is called squarefree if *n* is not a multiple of any perfect
 square other than 1.

## 2 The Basic Theory

**Weierstrass equation** for an elliptic curve, *E*(*L*) = {∞} ∪ {(*x*,*y*) ∈
 *L* × *L*|*y*<sup>2</sup> = *x*<sup>3</sup> + *Ax* + *B*}<br>
4*A*<sup>3</sup> + 27*B*<sup>2</sup> ≠ 0 (the discriminant is zero if and only
 if at least two roots are equal)<br>
**generalized Weierstrass equation**, *y*<sup>2</sup> + *a*<sub>1</sub>*xy* +
 *a*<sub>3</sub>*y* = *x*<sup>3</sup> + *a*<sub>2</sub>*x*<sup>2</sup> +
 *a*<sub>4</sub>*x* + *a*<sub>6</sub>, -*P* = (*x*, -*a*<sub>1</sub>*x* -
 *a*<sub>3</sub> - *y*)<br>
For technical reasons, it is useful to add a **point at infinity** to an
 elliptic curve. It not only is at the top of the *y*-axis, it is also at the
 bottom of the *y*-axis. It is best to regard ∞ as a formal symbol satisfying
 certain properties. Also, we have arranged that two vertical lines meet at ∞.
 Two lines should intersect in only one point, so the "top ∞" and the "bottom ∞"
 need to be the same.

*P* + ∞ = *P*<br>
The points on *E* form an additive abelian group with ∞ as the identity
 element.<br>
In fact, it is rather surprising that the law of composition that we have
 defined is associative.<br>
The usual addition of complex numbers induces a group law on **C**/\mathcal L
 that corresponds to the group law on the elliptic curve under the isomorphism
 between the torus and the elliptic curve. The real point *E*(**C**) are
 obtained by intersecting the torus with a plane.

A polynomial is **homogeneous** of degree *n* if it is a sum of terms of the
 form *ax*<sup>*i*</sup>*y*<sup>*j*</sup>*z*<sup>*k*</sup> with *a* ∈ *K* and
 *i* + *j* + *k* = *n*.

If *P* is a nonsingular point of a curve *F*(*x*,*y*,*z*) = 0, then the tangent
 line at *P* is, *F*<sub>*x*</sub>(*P*)*x* + *F*<sub>*y*</sub>(*P*)*y* +
 *F*<sub>*z*</sub>(*P*)*z* = 0.

## 3 Torsion Points

## 4 Elliptic Curves over Finite Fields

## 5 The Discrete Logarithm Problem

## 6 Elliptic Curve Cryptography

## 7 Other Applications

## 8 Elliptic Curves over Q

## 9 Elliptic Curves over C

## 10 Complex Multiplication

## 11 Divisors

## 12 Isogenies

## 13 Hyperelliptic Curves

## 14 Zeta Functions

## 15 Fermat's Last Theorem

