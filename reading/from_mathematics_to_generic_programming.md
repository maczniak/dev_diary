# [From Mathematics to Generic Programming][homepage], by Alexander A. Stepanov and Daniel E. Rose, Addison-Wesley (2015)

[errata][errata], see also "Elements of Programming" (Korean translation, "프로그래밍의 이해")

[homepage]: http://www.fm2gp.com/
[errata]: http://www.fm2gp.com/errata.html

based on notes from an "Algorithmic Journeys" course taught by Alex Stepanov at
[A9.com][a9_com] durig 2012

[a9_com]: https://a9.com/

## 1 What This Book Is About

**Generic programming** is an approach to programming that focuses on designing
algorithms and data structures so that they work in the most general setting
without loss of efficiency.<br>
"Wait a minute, that's all there is to generic programming? What about all that
stuff about templates and iterator traits?" Those are tools that enable the
language to support generic programming, and it's important to know how to use
them effectively. But generic programming itself is more of an *attitude*
toward programming than a particular set of tools. The components of a
well-written generic program are easier to use and modify than those of a
program whose data structures, algorithms, and interfaces hardcode unnecessary
assumptions about a specific application.<br>
This generic programming attitude comes from mathematics, and especially from a
branch of mathematics called *abstract algebra*. To help you understand the
approach, this book will introduce you to a little bit of abstract algebra,
which focuses on how to reason about objects in terms of abstract properties of
operations on them. The abstractions that appear in abstract algebra largely
come from concrete results in one of the oldest branches of mathematics, called
*number theory*. For this reason, we will also introduce some key ideas from
number theory, which deals with properties of integers, especially
divisibility.<br>
To be a good programmer, you need to understand the principles of generic
programming. To understand the principles of generic programming, you need to
understand abstraction. To understand abstraction, you need to understand the
mathematics on which it's based.

## 2 The First Algorithm

multiply-accumulate<br>
A **strictly tail-recursive** procedure is one in which all the tail-recursive
calls are done with the formal parameters of the procedure being the
corresponding arguments.

## 3 Ancient Greek Number Theory

## 4 Euclid's Algorithm

> The whole structure of number theory rests on a single foundation,
> namely the algorithm for finding the greatest common divisor.
> -- Dirichlet, *Lectures on Number Theory*

## 5 The Emergence of Modern Number Theory

## 6 Abstraction in Mathematics

## 7 Deriving a Generic Algorithm

## 8 More Algebraic Structures

## 9 Organizing Mathematical Knowledge

## 10 Fundamental Programming Concepts

## 11 Permutation Algorithms

## 12 Extensions of GCD

## 13 A Real-World Application

## 14 Conclusions

## A Notation

## B Common Proof Techniques

## C C++ for Non-C++ Programmers

"A Tour of C++" by Bjarne Stroustrup (Addison-Wesley, 2013) as an introduction to C++11<br>
as of this writing, C++ does not support concepts (template type restriction) as a built-in part of the language.

```C++
template <InputIterator I>
using IteratorCategory = // instead of typedef in C++11
          typename std::iterator_traits<I>::iterator_category;

std::pair<int, double> = {42, 3.1415}; // like array initializations in C++11

int a = some_other_function([=](int x) { return x * x * x; });
```

