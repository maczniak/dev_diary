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
A function is lifted over some structure f.

```haskell
{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

Prelude> type IntToInt = Fun Int Int
Prelude> type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
Prelude> quickCheck (functorCompose' :: IntFC)

Prelude> type Nat f g = f a -> g a -- Not in scope: type variable 'a'
Prelude> :set -XRank2Types -- or XRankNTypes
Prelude> type Nat f g = forall a . f a -> g a
Prelude> type Nat f g a = f a -> g a
```

Functor instances will be unique for a given datatype.

```haskell
{-# LANGUAGE FlexibleInstances #-}

data Tuple a b = Tuple a b deriving (Eq, Show)
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b
```

*Functor* is a mapping between categories. In Haskell, this manifests as a typeclass which lifts a function between two types over two new types. This conventionally implies some notion of a function which can be applied to a value with more structure than the unlifted function was originally designed for.

* [Haskell Wikibook; The Functor class][haskell_wikibook_functor]
* [The functor design pattern][functor_design_pattern] by Gabriel Gonzalez

https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns ?<br>
https://wiki.haskell.org/Rank-N_types ?

[gofer_generalizes_typeclasses]: http://www.cs.tufts.edu/~nr/cs257/archive/mark-jones/fpca93.pdf
[haskell_wikibook_functor]: https://en.wikibooks.org/wiki/Haskell/The_Functor_class
[functor_design_pattern]: http://www.haskellforall.com/2012/09/the-functor-design-pattern.html

## 17. Applicative

Applicative is a monoidal functor. The `Applicative` typeclass allows for function application lifted over structure (like Functor). But with Applicative the function we're applying is also embedded in some structure. Because the function *and* the value it's being applied to both have structure, we have to smash those structures together. So, Applicative involves monoids and functors. (Monoid for a structure and function application)

```haskell
-- in Control.Applicative
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b -- called apply, ap or tie-figher

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

fmap f x = pure f <*> x -- f <?> x

instance Monoid a => Applicative ((,) a) -- Defined in 'GHC.Base'
instance (Monoid a, Monoid b) => Moniod (a, b)
("Woo", (+1)) <*> (" Hoo!", 0)

-- Cartesian product
(,) <?> [1, 2] <*> [3, 4] == liftA2 (,) [1, 2] [3, 4]

instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)
```

Applicative laws
* Identity - `pure id <*> v = v`
* Composition - `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
* Homomorphism - `pure f <*> pure x = pure (f x)`
 * A homomorphism is a structure-preserving map between two categories. The effect of applying a function that is embedded in some structure to a value that is embedded in some structure should be the same as applying a function to a value without affecting any outside structure.
* Interchange - u <*> pure y = pure ($ y) <*> u

[checkers][checkers_module] example

```haskell
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance EqProp Bull where (=-=) = eq

main = quickBatch (monoid Twoo) -- A value does not matter. It uses a type only.
Prelude> quickBatch $ applicative [("b", "w", 1)]
Prelude> let trigger = undefined :: [(String, String, Int)]
Prelude> quickBatch (applicative trigger)
```

* [Validation library][validation_library] by Tony Morris and Nick Partridge
* [Applicative Programming with Effects][applicative_prog_effects] by Conor McBride and Ross Paterson
* Essence of the Iterator Pattern by Jeremy Gibbons and Bruno C. d. S. Oliveira
* [Constructing Applicative Functors][construct_applicative_functors] by Ross Paterson
* Idioms are oblivious, arrows are meticulous, monads are promiscuous by Sam Lindley, Philip Wadler and Jeremy Yallop
 * Idiom means applicative functor and is a useful search term for published work on applicative functors.

Success (+1) <*> (Success 1 :: Validation [Int] Int) p.749?

[checkers_module]: https://github.com/conal/checkers
[validation_library]: http://hackage.haskell.org/package/validation
[applicative_prog_effects]: http://staff.city.ac.uk/~ross/papers/Applicative.html
[construct_applicative_functors]: http://staff.city.ac.uk/~ross/papers/Constructors.html

## 18. Monad (SPOOKY? No.)

```haskell
class Applicative m => Monad m where -- GHC 7.10+
  (>>=) :: m a -> (a -> m b) -> m b -- bind, like (join . fmap)
    -- (=<<) :: (a -> f b) -> f a -> f b
  (>>) :: m a -> m b -> m b         -- sequencing operator
    -- (*>) :: Applicative f => f a -> f b -> f b
  return :: a -> m a                -- the same as pure

-- Monad is stronger than Applicative and Applicative is stronger than Functor.
-- You can derive Applicative and Functor in terms of Monad,
-- just as you can derive Functor in terms of Applicative
fmap f xs = xs >>= return . f

join :: Monad m => m (m a) -> m a
-- concat :: Foldable t => t [a] -> [a]

-- before applicatives were discovered
liftA :: Applicative f => (a -> b)  -> f a  -> f b
liftM :: Monad m =>       (a1 -> r) -> m a1 -> m r
liftA2 :: Applicative f => (a -> b -> c)   -> f a  -> f b  -> f c
liftM2 :: Monad m =>       (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- zipWith is liftA2 or liftM2 specialized to lists, but uses a different list monoid.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith (+) [3, 4] [5, 6] -- [8,10]
liftA2 (+) [3, 4] [5, 6] -- [8,9,9,10]
-- liftA3, liftM3, zipWith3
ap == (<*>)

binding :: do
  name <- getLine
  putStrLn name
binding' =
  getLine >>= putStrLn
  -- getLine >>= \name -> putStrLn name
```

Monads are applicative functors with some unique features. Monads are not, strictly speaking, necessary to Haskell. Older implementations of Haskell did not use monad for constructing and transforming IO actions. Monads are powerful and fun, but they do not define Haskell. Rather, monads are defined in terms of Haskell.<br>
["Monads are like burritos."][monads_like_burritos]<br>
Monad is not 1) Impure, 2) An embedded language for imperative programming, 3) A value, 4) About strictness. Using monads also doesn't require knowing math. Or category theory.<br>
"Haskell is the world's finest imperative programming language." -- Simon Peyton-Jones<br>
There is no `Monad` for `Validation` because `Either` always short-circuits on the first thing to have failed.

Monad laws
* Identity laws - (right identity) `m >>= return = m`, (left identity) `return x >>= f = f x`
* Associativity - `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

```haskell
instance Monad CountMe where -- example
  return = pure

  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
mapM = sequence . fmap f
forM = flip mapM
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
sequence = mapM id
```

Kleisli composition<br>
`(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c`

* [What a Monad is not][what_monad_is_not]
* [What I wish I knew when Learning Haskell][what_i_wish_i_knew] by Stephen Diehl
* [Monads Made Difficult][monads_made_difficult] by Stephen Diehl
* [Typeclassopedia][typeclassopedia] by Brent Yorgey

[monads_like_burritos]: http://blog.plover.com/prog/burritos.html
[what_monad_is_not]: https://wiki.haskell.org/What_a_Monad_is_not
[what_i_wish_i_knew]: http://dev.stephendiehl.com/hask/#monads
[monads_made_difficult]: http://www.stephendiehl.com/posts/monads.html
[typeclassopedia]: https://wiki.haskell.org/Typeclassopedia

## 19. Abstract structure applied

(<*) :: Applicative f => f a -> f b -> f a<br>
[haproxy-haskell][haproxy-haskell] (Haskell binding to the HAProxy socket API)<br>
[URL shortener example][url_shortener_example]<br>
[Stack video tutorial][stack_video_tutorial]<br>
`OverloadedStrings` make `String` literals polymorphic. (`Text`, `ByteString`, `fromString` of `IsString` typeclass)

* [The case of the mysterious explosion in space][mysterious_explosion_in_space] (how GHC handles string literals)  by Bryan O'Sullivan

understand code samples?

[haproxy-haskell]: https://github.com/MichaelXavier/haproxy-haskell
[url_shortener_example]: https://github.com/bitemyapp/shawty-prime/blob/master/app/Main.hs
[stack_video_tutorial]: https://www.youtube.com/watch?v=sRonIB8ZStw
[mysterious_explosion_in_space]: http://www.serpentine.com/blog/2012/09/12/the-case-of-the-mysterious-explosion-in-space/

## 20. Foldable

```haskell
-- all of Foldable is in the Prelude from GHC 7.10.
class Foldable (t :: * -> *) where
  {-# MINIMAL foldMap | foldr #-}
  fold :: Data.Monoid.Monoid m => t m -> m
  foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m

toList :: t a -> [a]
null :: t a -> Bool -- null (Left 3) == null Nothing
length :: t a -> Int
elem :: Eq a => a -> t a -> Bool
maximum :: forall a . Ord a => t a -> a -- the largest element of a non-empty structure
minimum :: forall a . Ord a => t a -> a
sum :: (Foldable t, Num a) => t a -> a
product :: (Foldable t, Num a) => t a -> a

-- Data.Monoid
newtype First a = First { getFirst :: Maybe a }
newtype Last a = Last { getLast :: Maybe a }
```

* [Foldable and Traversable][foldable_and_traversable] by Jakub Arnold

forall ?

[foldable_and_traversable]: http://blog.jakubarnold.cz/2014/07/30/foldable-and-traversable.html

## 21. Traversable

Traversable was introduced in the same paper as Applicative and its introduction to Prelude didn't come until the release of GHC 7.10. However, it was available as part of the `base` library for much longer than that. Traversable depends on Applicative, and thus Functor, and is also superclassed by Foldable.<br>
[vector package][vector_package], [wreq package][wreq_package]

```haskell
-- in Data.Traversable
class (Functor t, Foldable t) => Traversable t where
  {-# MINIMAL traverse | sequenceA #-}
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
         -- mapM :: Monad m => (a -> m b) -> [a] -> m [b] -- GHC prior to 7.10
                   -- (=<<) :: (a -> m b) -> m a -> m b
  traverse f = sequenceA . fmap f
         -- (sequence .) . fmap = \ f xs -> sequence (fmap f xs)
    -- implementation pattern: traverse f (Right y) = Right <$> f y
  sequenceA :: Applicative f => t (f a) -> f (t a)
      -- sequence :: Monad m => [m a] -> m [a] -- GHC prior to 7.10
  sequenceA = traverse id

-- in Data.Maybe
catMaybes [Just 1, Just 2, Nothing] == [1, 2]
-- cf. sequenceA [Just 1, Just2, Nothing] == Nothing
fromMaybe :: a -> Maybe a -> a

-- Traversable is stronger than Functor and Foldable. Because of this, we can
-- recover the Functor and Foldable instance for a type from the Traversable,
-- just as we can recover the Functor and Applicative from the Monad.
fmap f t = runIdentity $ traverse (Identity . f) t
foldMap f t = getConstant $ traverse (Constant . f) t
```

Traversable Laws
* Naturality - `t . traverse f = traverse (t . f)`
* Identity - `traverse Identity = Identity`
* Composition - `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

sequenceA laws
* Naturality - `t . sequenceA = sequenceA . fmap t`
* Identity - `sequenceA . fmap Identity = Identity`
* Composition - `sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA`

```haskell
type TI = []

main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
```

[vector_package]: http://hackage.haskell.org/package/vector
[wreq_package]: http://hackage.haskell.org/package/wreq

## 22. Reader

We don't want to simply pass this information as arguments because it would be present in the type of almost every function. To address this, we use the Reader Monad. It is a way of stringing functions together when all those functions are awaiting one input from a shared environment.

```haskell
-- Functor, Applicative, and Monad for partially-applied functions
fmap f1 f2 = f1 . f2
(+) <$> f1 <*> f2 = liftA2 (+) f1 f2
f1f2 = do
    a <- f1
    b <- f2
    return (a + b)
```

...

## 23. State

## 24. Parsers

## 25. Composing Types

## 26. Monad Transformers

## 27. Non-strictness

## 28. Commonly used data structures

## 29. Demystifying IO

## 30. Exceptions

## 31. Final Project

