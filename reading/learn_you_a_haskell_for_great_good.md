# [Learn You a Haskell for Great Good!][learnyouahaskell] by Miran Lipovača, No Starch Press (2011)

[learnyouahaskell]: http://learnyouahaskell.com/

## 1. Introduction

Haskell is 1) a purely functional programmig language, 2) lazy,
 3) statically typed, 4) elegant and typed, 5) made by really smart guys.

install `haskell-platform` pacakge<br>
in interactive `ghci`, `:r` (reload) or `:l myfunctions` after loading scripts<br>
Haskell committee started in 1987, and Haskell Report was published in 2003.

## 2. Starting Out

`:set prompt "ghci> "<br>
`True == 5` -- error<br>
`infix` form, `succ` <-> `pred`, `min` <-> `max` functions<br>
function application has the highest precedence.<br>
`let a = 1` ghci-specific syntax

`doubleSmallNumber' x = (if x > 100 then x else x*2) + 1`
* "else" part is mandatory.
* watch out precedence!
* function names start with a lowercase letter. type names start with an uppercase letter.
* "'" means a strict (no lazy) or modified version.
* definition (or name) - function with no parameters

list - homogenous data structure, comparable<br>
list `++` list, element `:` list (cons operator), 1:2:3:[]'s syntax sugars<br>
list `!!` 0-based index, `head` : `tail` = `last` : `init`, `length`, `null`, `reverse`, `take` #elements list <-> `drop`, `maximum`, `minimum`, `sum`, `product`, `elem` (whether the element exists in the list)<br>
range `[20,19..1]`, `take 24 [13,26..]`, `cycle` list, `repeat` element, `replicate` count element<br>
list comprehension `[ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]`

tuple<br>
no singleton (one element) tuples<br>
`fst` pair, `snd` pair, `zip` list list<br>
"We'll go over extracting data from tuples in different ways a bit later." ?

## 3. Types and Typeclasses

## 4. Syntax in Functions

## 5. Recursion

## 6. Higher Order Functions

## 7. Modules

## 8. Making Our Own Types and Typeclasses

## 9. Input and Output

## 10. Functionally Solving Problems

## 11. Functors, Applicative Functors and Monoids

## 12. A Fistful of Monads

## 13. For a Few Monads More

## 14. Zippers

