# [Haskell Programming from first principles][homepage], by Christopher Allen and Julie Moronuki

[homepage]: http://haskellbook.com/

## 1. Lambda Calculus

alpha equivalence and beta reduction<br>
A combinator is a lambda term with no free variables.<br>
the semantics of the core language are the same as the lambda calculus. That is, the meaning of Haskell programs is centered around evaluating expressions rather than executing instructions.<br>
Normal order isn't how Haskell code is evaluated -- it's call-by-need instead.

* [A Tutorial Introduction to the Lambda Calculus][tut_lambda_calculus] by Raul Rojas
* [Introduction to Lambda Calculus][intro_lambda_calculus] by Henk Barendregt and Erik Barendsen
* [Proofs and Types][proofs_and_types] by Jean-Yves Girard, P. Taylor and Yves Lafon

[tut_lambda_calculus]: http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf
[intro_lambda_calculus]: http://www.cse.chalmers.se/research/group/logic/TypesSS05/Extra/geuvers.pdf
[proofs_and_types]: http://www.paultaylor.eu/stable/prot.pdf

## 2. Hello Haskell

`:r` (recompile and load)<br>
(quot x y)*y + (rem x y) == x, (div x y)*y + (mod x y) == x

* [How to desugar Haskell code][desugar_haskell]

[desugar_haskell]: http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

## 3. Strings

Global or top level bindings in Haskell mean bindings visible to all code within a module and, if made available, can be imported by other modules or programs. Global bindings in the sense that a variable is unconditionally visible throughout an entire program do not exist in Haskell.

## 4. Basic Datatypes

Num - Int, (Data.Int) Int8, Int16, Integer, Float, Double, Rational, (Data.Scientific) Scientific<br>
"Julie" == 8 -- error<br>
length :: [a] -> Int<br>
there is no singleton tuple, but there is a zero tuple also called unit or ().

## 5. Types

funcIgnoresArgs :: a -> a -> a -> String<br>
funcIgnoresArgs x y z = "Blah"<br>
funcIgnoresArgs undefined undefined :: a -> String<br>
curry f a b = f (a, b)<br>
uncurry f (a, b) = f a b<br>
Prelude> let f :: a -> a -> a -> a; f = undefined<br>
[How to make *ad-hoc* polymorphism less *ad hoc*][ad_hoc_polymorphism]<br>
if a variable could be anything, then there's little that can be done to it because it has no methods. If it is a concrete type, you lose the type flexibility but, due to the additive nature of typeclass inheritance, gain more potential methods.<br>
Haskell's [type inference][type_inference] is built on an extended version of the [Damas-Hindley-Milner type system][damas_hindley_milner_type_system].<br>
With respect to Haskell, the *principal type* is the most generic type which still typechecks.<br>
[@parametricity][twitter_parametricity]

* [Principal type-schemes for functional programs][principal_type_schemes]
* [Fundamental Concepts in Programming Languages][fundamental_concepts_in_programming_languages]

[ad_hoc_polymorphism]: http://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf
[type_inference]: https://en.wikipedia.org/wiki/Type_inference
[damas_hindley_milner_type_system]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
[twitter_parametricity]: https://twitter.com/parametricity
[principal_type_schemes]: http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf
[fundamental_concepts_in_programming_languages]: http://www.cs.cmu.edu/~crary/819-f09/Strachey67.pdf

## 6. Typeclasses

...

partial function vs total function

...

## 7. Functions

...

*Bottom* (⊥) is a non-value used to denote that the program cannot return a value or result. The two main varieties of bottom are computations that failed with an error or those that failed to terminate. (for example, `error`, failed pattern matches of partial functions, `undefined` and infinite loops)<br>
*Higher-order* functions are functions which themselves take functions as arguments or return functions as results.<br>
`enumFrom`<br>
*[Pointfree][pointfree]* refers to a style of composing functions without specifying their arguments. The "point" in "pointfree" refers to the arguments.

* [Case Expressions and Pattern Matching chapter of A Gentle Introduction to Haskell, Version 98][gentle_intro] by Paul Hudak, John Peterson and Joseph Fasel
* [The Implementation of Functional Programming Languages][implementation_fp_lang] by Simon Peyton Jones
* [An introduction to pointfree programming][pointfree_intro] by J.N. Oliveira
* [Point-free Program Calculation][pointfree_calc] by Manuel Alcino Pereira da Cunha

[pointfree]: https://wiki.haskell.org/Pointfree
[gentle_intro]: https://www.haskell.org/tutorial/patterns.html
[implementation_fp_lang]: http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm
[pointfree_intro]: http://www.di.uminho.pt/~jno/ps/iscalc_1.ps.gz
[pointfree_calc]: http://www4.di.uminho.pt/~mac/Publications/phd.pdf

## 8. Recursion

Being able to write recursive functions, though, is essential to Turing completeness. We use a combinator `.` known as the [Y combinator][y_combinator] or fixed-point combinator `.` to write recursive functions in the lambda calculus. Haskell has native recursion ability based on the same principle as the Y combinator.

[y_combinator]: http://mvanier.livejournal.com/2897.html

## 9. Lists

Ranges use `enumFromTo` and `enumFromThenTo` of `Enum` type class.<br>
`:sprint` - see what has been evaluated, caution: opportunistic optimizations and polymorphism<br>
NF (normal form) implies WHNF (weak head normal form). An expression cannot be in normal form or weak head normal form if the outermost part of the expression isn't a data constructor. It can't be in normal form if any part of the expression is unevaluated.<br>
`length` is strict in the spine but not the values.<br>
A common mantra for performance sensitive code in Haskell is, "lazy in the spine, strict in the leaves."

* [Data.List][data_list]
* [Ninety-nine Haskell problems][ninety_nine_problems]

`bool` function in Data.Bool module?

[data_list]: http://hackage.haskell.org/package/base/docs/Data-List.html 
[ninety_nine_problems]: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems 

## 10. Folds

Catamorphisms ("cata-" means "down" or "against") are a means of deconstructing data (breaking down structure). A catamorphism for a non-collection datatype, `data Maybe a = Nothing | Just a`, `maybe :: b -> (a -> b) -> Maybe a -> b`<br>
`foldr :: (a -> b -> b) -> b -> [a] -> b`<br>
`foldr f acc (x:xs) = f **x** (foldr f acc xs)` can be used with infinite lists. Everything is find unless the first piece (cons cell) of the spine is bottom.<br>
`[1, 2, 3, 4, undefined] -- undefined value`<br>
`[1, 2, 3, 4] ++ undefined -- undefined spine`<br>
`length $ take 2 $ take 4 ([1, 2]++undefined) -- no error`<br>
`foldl :: (b -> a -> b) -> b -> [a] -> b`<br>
`foldl f acc (x::xs) = foldl f (f acc x) xs -- left associativity` must evaluate its whole spine before it starts evaluating values in each cell. Use `foldl'` almost always (for long lists).<br>
`scanr (+) 0 [1..5] -- [15,14,12,9,5,0]`<br>
`scanl (+) 0 [1..5] -- [0,1,3,6,10,15]`<br>
`foldr f z xs = foldl (flip f) z (reverse xs) -- only for finite lists` or `reverse $ foldl (flip f) z xs`<br>
[Twitter waterflow problem and loeb][waterflow_problem]<br>
`fibs = 1 : scanl (+) 1 fibs`<br>
`map' f = foldr (\x xs -> f x : xs) []`<br>
Tail recursion is a function whose *tail calls* are recursive invocations of itself.

* [Fold, Haskell Wiki][fold_wiki]
* [Introduction to Haskell][intro_haskell] by Antoni Diller
* [A tutorial on the universality and expressiveness of fold][tut_uni_exp_fold]

[waterflow_problem]: http://chrisdone.com/posts/twitter-problem-loeb
[fold_wiki]: https://wiki.haskell.org/Fold
[intro_haskell]: http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html
[tut_uni_exp_fold]: http://www.cs.nott.ac.uk/~gmh/fold.pdf

## 11. Algebraic datatypes

[Typed type-level programming in Haskell, part I: functional dependencies][type_level_prog]<br>
:kind, :k<br>
phantom type arguments<br>
In standard Haskell, we cannot quotient out or choose specific inhabitants (values) of types as arguments.<br>
arity - nullary, unary, products (data constructors that take more than one argument), tuple (anonymous products)<br>
Algebraic datatypes in Haskell are algebraic because we can describe the patterns of argument structures using two basic operations: sum and product. (algebraic rules such as the distributive property)<br>
A `newtype` cannot be a product type, sum type, or contain nullary constructors. One advantage is that it has no runtime overhead, as it reuses the representation of the type it contains. The difference between newtype and the type it contains is gone by the time the compiler generates the code. The newtype declaration will allow you to define a custom typeclass instance. We can't do this if it's just a type synonym (`type`). `{-# LANGUAGE **X**GeneralizedNewtypeDeriving #-}` pragma will tell the compiler to allow our newtype (`newtype Goats = Goats Int deriving (Eq, Show, TooMany)`) to rely on a typeclass instance for the type it contains.<br>
(not now) `:set -XNegativeLiterals` extension prevents an warning when using not `(negate 128)` but `(-128)`. `(-128)` desugars into `(negate 128)`.<br>
Type aliases create type constructors, not data constructors.<br>
record - `data Person = Person { name :: String, age :: Int } deriving (Eq, Show)`, `Person "Papu" 5`<br>
normal form - sum of products, like `data Expr = Number Int | Add Expr Expr | Minus Expr`<br>
The nullary data constructor is equivalent to the () unit type.<br>
You can construct values of products that use record syntax in a manner identical to that of non-record products (or `myRecord = RecordProduct { pfirst = 42, , psecond = 0.00001 }`). Records are just syntax to create field references.<br>
`nub` (means essence) of Data.List keeps the first occurrence of each element and removes duplicate elements (O(n^2)).<br>
Partial applications of the record data constructor make exceptions. Keep sum types apart to prevent accidental bottoms.<br>
cardinality of function a -> b = b^a, a -> b -> c = (c ^ b) ^ a = c ^ (b * a)<br>
higher-kinded type (vs polymorphism)<br>
`data Silly a b c d = MkSilly a b c d deriving Show` is the same kind as `(,,,)`.<br>
[bloodhound][bloudhound] ElasticSearch package by the auther<br>
All infix data constructors must start with a colon. Exceptions: (->), type constructor of functions and :: that is reserved for type (and kind) assertions.<br>
type constructors cannot exist at runtime (static type).<br>
t@(a, _) as-patterns<br>
[1HaskellADay][one_haskell_a_day]<br>
[Graham Hutton: Publications][graham_hutton_publications]

[FlexibleInstances](https://prime.haskell.org/wiki/FlexibleInstances)?

```
instance (FromJSON a) => FromJSON (EsResultFound a) where -- ?
  parseJSON (Object v) = EsResultFound <$>
                         v .: "_version" <*>
                         v .: "_source"
  parseJSON _          = empty
```

[type_level_prog]: https://byorgey.wordpress.com/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies/
[bloodhoud]: http://hackage.haskell.org/package/bloodhound
[one_haskell_a_day]: https://twitter.com/1haskelladay
[graham_hutton_publications]: http://www.cs.nott.ac.uk/~pszgmh/bib.html#semantics

## 12. Signaling Adversity

Case expressions and pattern matching will work without an Eq instance, but guards using (==) will not.<br>
A lifted type (*), which includes any datatype you could define yourself, is any that can be inhabited by bottom. Lifted types are represented by a pointer and include most of the datatypes we've seen and most that you're likely to encounter and use. / Unlifted types (#) are any type which cannot be inhabited by bottom. Types of kind # are often native machine types and raw pointers. / Newtypes are a special case in that they are kind *, but are unlifted because their representation is identical to that of the type they contain, so the newtype itself is not creating any new pointer beyond that of the type it contains.<br>
:k [] Int == [Int]

...

anamorphisms?, animorphs?

```
mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
```

p. 504, flipMaybe :: [Maybe a] -> Maybe [a], flipMaybe [Just 1, Nothing, Just 3], Nothing -- called sequence?

## 13. Building projects in Haskell

Haskell Cabal (Common Architecture for Building Applications and Libraries) package manager, Stack (cross-platform program for developing Haskell projects, replies on an LTS (long term support) snapshot of Haskell packages from Stackage)<br>
sample project - https://github.com/fightingtanukis/hello<br>
hello.cabal, stack.yaml<br>
stack setup, stack build, stack ghci, stack exec (--) hello (`Linking .stack-work/dist/{...noise...}/hello`, executable stanza in `hello.cabal`), stack new hangman simple<br>
library stanza uses exposed-modules foo, bar instead of main-is Main.<br>
test-suite stanza (example p. 573)<br>
module Hello where ..., module Hello ( sayHello ) where ...<br>
The ordering of import declarations is irrelevant.<br>
stack ghci --ghci-options -XNoImplicitPrelude<br>
import Data.Bool, import Data.Bool (bool), import qualified Data.Bool, import qualified Data.Bool as B<br>
import System.IO, hSetBuffering stdout NoBuffering<br>
import Control.Monad (forever), import Data.Char (toLower), import Data.Maybe (isJust), import Data.List (intersperse), import System.Exit (exitSuccess), import System.Random (randomRIO)<br>
Foldable instance - [], Maybe, Either

* [Stack][stack] - [Commercial Haskell][commercialhaskell], [FP Complete][fpcomplete]
* [How I Start: Haskell][how_i_start_haskell]
* [Cabal FAQ][cabal_faq]
* [Cabal User's Guide][cabal_user_guide]

[stackage]: https://www.stackage.org/
[stack]: https://github.com/commercialhaskell/stack
[commercialhaskell]: http://commercialhaskell.com/
[fpcomplete]: https://www.fpcomplete.com
[how_i_start_haskell]: http://bitemyapp.com/posts/2014-11-18-how-i-start-haskell.html
[cabal_faq]: https://www.haskell.org/cabal/FAQ.html
[cabal_user_guide]: https://www.haskell.org/cabal/users-guide/

## 14. Testing

unit testing, spec testing ([Hspec][hspec] [manual][hspec_manual], HUnit), property testing (QuickCheck)<br>
stack init

```haskell
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

-- shouldBe :: (Eq a, Show a) => a -> a -> Expectation -- augmented == with Show

-- in Test.QuickCheck
arbitrary :: Arbitrary a => Gen a
sample :: Show a => Gen a -> IO ()
sample' :: Gen a -> IO [a]
choose :: System.Random.Random a => (a, a) -> Gen a
elements :: [a] -> Gen a
frequency :: [(Int, Gen a)] -> Gen a
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b
-- in Test.QuickCheck.Gen
oneof :: [Gen a] -> Gen a

runQc :: IO ()
runQc = quickCheck prop_additionGreater -- prop_additionGreater :: Int -> Bool
```

If you leave the types unspecified, the extended defaulting behavior of GHCi will (helpfully?) pick unit for you. Outside of GHCi, you'll get an error about an ambiguous type.<br>
import - Control.Monad (forever, when), Data.List(intercalate), Data.Traversable (traverse), System.Environment (getArgs), System.Exit (exitFailure, exitSuccess), System.IO (hGetLine, hIsEOF, stdin)<br>
stack install (into $HOME/.local/bin, [XDG][xdg] guideline), stack ghci morse:tests

```haskell
morseToLetter = M.foldWithKey (flip M.insert) M.empty letterToMorse
M.lookup :: Ord k => k -> Map k a -> Maybe a
-- , M.keys and M.elems

-- in Data.Traversable
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
sequence :: Monad m => t (m a) -> m (t a)
```

* [An introduction to QuickCheck testing][intro_to_quickcheck] by Pedro Vasconcelos
* QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs by Koen Claessen and John Hughes
* [Verifying a Simple Compiler Using Property-based Random Testing][verify_compiler] by Pedro Vasconcelos

```
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
-- vs
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing),
                         (3, liftM Just arbitrary)]
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r

import GHC.Generics
data Bool' = True' | False' deriving (Generic)
instance CoArbitrary Bool' -- randomly generate a function?
```

isomorphism?

[hspec]: http://hackage.haskell.org/package/hspec
[hspec_manual]: http://hspec.github.io/
[xdg]: https://wiki.archlinux.org/index.php/XDG_user_directories
[intro_to_quickcheck]: https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing
[verify_compiler]: http://www.dcc.fc.up.pt/dcc/Pubs/TReports/TR13/dcc-2013-06.pdf

## 15. Monoid, Semigroup

A monoid is a binary associative operation with an identity.<br>
mempty, mappend (<>)<br>
mappend (Sum 1) (Sum 5), mappend (Product 5) (Product 5)<br>
A variant of monoid that provides even stronger guarantees is the Abelian or commutative monoid.<br>
Among (many) other things, laws provide us guarantees that let us build on solid foundations for the predictable composition (or combination) of programs.<br>
The monoidal operation is less about combining the values and more about finding a (condensing or reducing) summary value for the set.<br>
Bool - All/Any, Maybe - First/Last/...(Monoid a)...<br>
An orphan instance is when an instance is defined for a datatype and typeclass, but not in the same module as either the declaration of the typeclass or the datatype. Writing orphan instances should be avoided at all costs. If you don't "own" the typeclass or the datatype, newtype it!<br>
verboseCheck - quickCheck while printing test values<br>
semigroup - binary, associative, no identity<br>
magma (or groupoid in abstract algebra, not groupoids in category theory) - binary, no associative, no identity<br>
[semigroups][semigroups_package] will be added to GHC's base librrary.<br>
`data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)`, head and length in Data.List.NonEmpty<br>
A symbolic data constructor can't be used as a prefix. An alphanumeric data constructor can't be used as an infix.<br>
The strength of an algebra is the number of operations it provides.<br>
(a -> b) is the instance of Semigroup and Monoid.

* [Algebraic structure][algebraic_structure], Simple English Wikipedia
* [Haskell Monoids and their Uses][monoids_uses]

semiring?

[semigroups_package]: http://hackage.haskell.org/package/semigroups
[algebraic_structure]: https://simple.wikipedia.org/wiki/Algebraic_structure
[monoids_uses]: http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html

## 16. Functors

Lifting is the "cheat mode" of type tetris. -- Michael Neale<br>
Rudolf Carnap, in the 1930s<br>
Functors are combinators: they take a sentence or phrase as input and produce a sentence or phrase as an output, with some logical operation applied to the whole. It lifts the concept of ... over the entire sentence or phrase structure without changing the internal structure. Functor is in some sense just a special sort of function application.<br>
`(<$>) == fmap`<br>
[A system of constructor classes: overloading and implicit higher-order polymorphism][gofer_generalizes_typeclasses]<br>
Functor laws - identity (`fmap id == id`), composition (`fmap (f . g) == fmap f . fmap g`)<br>
fmap == (.) for function<br>

[gofer_generalizes_typeclasses]: http://www.cs.tufts.edu/~nr/cs257/archive/mark-jones/fpca93.pdf

## 17. Applicative

## 18. Monad (SPOOKY? No.)

## 19. Abstract structure applied

## 20. Foldable

## 21. Traversable

## 22. Reader

## 23. State

## 24. Parsers

## 25. Composing Types

## 26. Monad Transformers

## 27. Non-strictness

## 28. Commonly used data structures

## 29. Demystifying IO

## 30. Exceptions

## 31. Final Project

