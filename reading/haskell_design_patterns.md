# [Haskell Design Patterns][homepage] by Ryan Lemmer, Packt Publishing (2015)

[homepage]: https://www.packtpub.com/application-development/haskell-design-patterns

## Chapter 1: Functional Patterns - the Building Blocks

Programming with functions in the style, free of arguments, is called **tacit
 programming**. It can become difficult to infer types (and, therefore,
 meaning). Use this style when ease of reading is not overly compromised.<br>
Curried functions are composable, whereas uncurried functions are not.<br>
Recursion can be viewed as a pattern for avoiding a mutable state. Tail
 recursion addresses the exorbitant use of space we have with non-tail-recursive
 processes. Even though `sumTail` is a recursive function, it expresses an
 iterative process. `sumNonTail` is a recursive function that expresses a
 recursive process. The `foldl` function expands in exactly the same way as
 `sumTail`. In contrast, `foldr` expands in the same way as `sumNonTail`.<br>
alternation-based ad-hoc polymorphism vs class-based polymorphism<br>
We have parametric polymorphism, where a single generic function acts on a
 variety of types. This is in contrast to ad-hoc polymorphism, where we have an
 overloaded function that is resolved to a particular function by the compiler.
 Haskell blurs the distinction between ad hoc and parametric polymorphism. We
 can see this clearly in the definition of the type class for equality.<br>
Lazy lists decouple consumers from producers. From another perspective, we have
 a decoupling between iteration and termination.<br>
Streams provide an antidote to mutation, but as with all powerful medicine,
 streams create new problems. Because streams pretend to express a complete list
 while only incrementally materializing the list, we cannot know exactly when
 evaluation of list elements happens. Working on I/O in the face of lazy
 evaluation is a minefield.<br>
In 1989, Eugenio Moggi used monads, from Category theory, to describe
 programming language features. Phillip Wadler, a then member of the Haskell
 Committee, recognized that it was possible to express Moggi's ideas in Haskell
 code.

## Chapter 2: Patterns for I/O

We start this chapter by establishing I/O as a first-class citizen of Haskell.
 We start with the most naïve style: imperative style. From there, we move on to
 the elegant and concise "lazy I/O", only to run into its severe limitations.
 The way out is the third and last style we explore: iteratee I/O.<br>
[Iteratees][iteratees]<br>
`sequence_ = foldr (>>) (return ())`<br>
While the `hGetLine` function returns a strict string, the `hGetContents`
 function returns a lazy string.<br>
`mapM_ f` is equal to `sequence_ (map f)`.
 `mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()`<br>
The `forM_` function is just a `mapM_` function with flipped arguments, which is
 useful when you want to pass a "trailing lambda".<br>
Lazy I/O is expressed at a relatively high level of abstraction, and very
 composable, enabling the decoupling of producers from consumers. But it has
 poor control over when something is evaluated, and has a poor control of
 resources.<br>
When we use `print $ words contents`, all the file contents will be held in
 memory at once. We could have used `hGetContents h >>= return . lines` to print
 the lines incrementally, thereby using only a constant space.

> "Extensive experience in Haskell has, however, exposed severe drawbacks of
> lazy evaluation, which are especially grievous for stream processing of large
> amounts of data.
>
> Lazy evaluation is fundamentally incompatible with computational effects, can
> cause fatal memory leaks, and greatly inhibits modular reasoning, especially
> about termination and space consumption.
>
> Seemingly innocuous and justified changes to the code or code compositions may
> lead to divergence, or explosion in memory consumption."
>
> [Lazy v. Yield: Incremental, Linear Pretty-printing][lazy_v_yield] -
> Kiselyov et al

`withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r`<br>
[Bracket pattern][bracket_pattern]<br>
*Why Functional Programming Matters* (1990, John Hughes) has been described as
 the manifesto for lazy programming, written at a time when enthusiasm for this
 style was running very high. Less than 20 years later, Oleg Kiselyov published
 the obituary of Lazy I/O in
 [a series of writings][streams_and_incremental_processing]. In the late 2000s,
 Kiselyov championed a new way of doing I/O that combines the best of
 Handle-based I/O (precise control over resources and predictable space
 requirements) with the best of lazy I/O (decoupling of producers and consumers,
 high level of abstraction).
[Haskell lecture notes][lecture_notes],
 [Monad.Reader Issue 16][monad_reader_issue_16]<br>
Instead of asking us to simply call the `chunkIter` function again, the iteratee
 provides us with the next iteratee to run (which has embedded knowledge of
 accumulated chunks).<br>
An enumerator feeds data to an iteratee to get a result. Enumerators work like
 folds, in the sense that they apply a function (with an accumulator) over an
 input stream element by element.<br>
We want to be able to compose iteratees and enumerators by the monad type class.
 Iteratees produce data, enumeratees serve as pipeline transformers of data, and
 enumerators consume data and drive the whole pipeline process.<br>
[iteratee][iteratee_package] by John Lato and Oleg Kiselyov,
 [enumerator][enumerator_package] and [iterIO][iterio_package] by John Millikin,
 the most "modern" Iteratee I/O libraries are "[pipes][pipes_package]" (Gabriel
 Gonzales), which focus on preserving equational reasoning, and
 "[conduit][conduit_package]" (Michael Snoyman), which focusses on deterministic
 resource management.<br>
We looked at the means of decoupling producers and consumers of data and also
 the extent to which producers and consumers can communicate in the midst of
 data processing.

```haskell
newtype Iter = Iter {runIter :: B8.ByteString -> IterResult}

data IterResult
  = HaveLine {line :: String, residual :: String}
  | NeedChunk Iter

type Enumerator = Iter -> IO IterResult
```
[iteratees]: http://okmij.org/ftp/Haskell/Iteratee/describe.pdf
[lazy_v_yield]: https://www.cs.indiana.edu/~sabry/papers/yield-pp.pdf
[bracket_pattern]: https://wiki.haskell.org/Bracket_pattern
[streams_and_incremental_processing]: http://okmij.org/ftp/Streams.html
[lecture_notes]: http://www.scs.stanford.edu/11au-cs240h/notes/iteratee.html
[monad_reader_issue_16]: https://themonadreader.files.wordpress.com/2010/05/issue16.pdf
[iteratee_package]: https://hackage.haskell.org/package/iteratee 
[enumerator_package]: https://hackage.haskell.org/package/enumerator
[iterio_package]: https://hackage.haskell.org/package/iterIO
[pipes_package]: https://hackage.haskell.org/package/pipes
[conduit_package]: https://hackage.haskell.org/package/conduit 

## Chapter 3: Patterns of Composition

Functor embeds into applicative functor, which embeds into arrow, which embeds
 into monad.<br>
`Applicative` provides us with a multiparameter `fmap` function. When we combine
 two `Applicative`'s with the `<*>` operator, we always get another
 `Applicative`. In other words, `Applicative`'s are "closed under
 composition".<br>
The "applicative pattern" was recognized and extracted as `Applicative` only in
 2008, more than ten years after `Monad` became an established part of Haskell
 (and 20 years after Eugenio Moggi's `Monad` paper). These discrepancies have
 been resolved by the
 "[Functor-Applicative-Monad Proposal][functor_applicative_monad_proposal]",
 implemented in base 4.8.0.0 used by GHC 7.10 and above.<br>
The shared context of the Reader Monad can be used as a place to do "out of
 band" processing. This explains why we can use monads to approximate imperative
 programming (where out of band processing is so prevalent). Monads bind in a
 way that includes an "accumulator". This contrasts with applicatives, which
 have no accumulator and hence no communication between arguments.<br>
Monads are not "closed under composition", as is the case with applicatives,
 because monads generally don't compose into monads.<br>

```haskell
gM <=< fM = \x -> (fM x) >>= gM

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype AppIO a = App {runApp :: ReaderT Config (WriterT String IO) a}
  deriving (Monad, MonadReader Config, MonadWriter String, MonadIO)

data IOArrow a b = IOArrow {runIOArrow :: a -> IO b}

instance Category IOArrow where
  id = IOArrow return
  IOArrow f . IOArrow g = IOArrow $ f <=< g

instance Arrow IOArrow where
  arr f = IOArrow $ return . f
  first (IOArrow f) = IOArrow $ \(a, c) -> do
      x <- f a
      return (x, c)

main = do
  let f = IOArrow print .
            arr length .
            arr words .
            IOArrow readFile
  runIOArrow f "jabberwocky.txt"

(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(>>>) = flip (.)
(***) -- `first` and `second` at once

data Kleisli m a b = K {runKleisli :: a -> m b}
instance Monad m => Arrow (Kleisli m) where
  arr f = K (\x -> return (f x))
  K f >>> K g = K (x -> f x >>= g)

main = do
  let f = Kleisli print . arr length . arr words . Kleisli readFile
  runKleisli f "jabberwocky.txt"
```

There are more recent [extensible-effects][extensible_effects] and
 [layers][layers] packages that attempt to solve this problem to define your own
 monad transformers tediously.<br>
The order of `ReaderT` and `WriterT` is inconsequential. This is not true for
 all combinations of monads, however. If one monad relies on the work of a
 previous monad being done, the order indeed matters. Moreover, `IO` is a
 special case and must remain at the bottom of the stack.<br>
[Programming with Arrows][programming_with_arrows],
 [Arrows are simpler than they appear][arrows_are_simpler_than_they_appear]<br>
Our `IOArrow` type is unnecessary because there is already the Kleisli arrow,
 which generalizes `IOArrow` to all `Monads`. All monads have a corresponding
 Kleisli arrow, but there are more arrows than monads. In the same way, there
 are more applicative functors than arrows, and more functors than applicative
 functors.<br>
"idioms (applicative functors) embed into arrows and arrows embed into
 monads."<br>
We can combine different arrows into stacks using transformer arrows. As the
 types get more powerful the means of composition (generality, flexibility)
 becomes poorer!<br>
[Deterministic, Error-Correcting Combinator Parsers][deterministic_error_correcting_combinator_parsers]
 → [Generalising Monads to Arrows][generalising_monads_to_arrows], Arrows have
 been useful in parsers, streaming applications, and user interfaces, and in
 recent years they have featured prominently as an approach to functional
 reactive programming. (See [Yampa][yampa], [Netwire][netwire])

[functor_applicative_monad_proposal]: https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
[extensible_effects]: https://hackage.haskell.org/package/extensible-effects
[layers]: https://hackage.haskell.org/package/layers
[programming_with_arrows]: http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
[arrows_are_simpler_than_they_appear]: http://www.newartisans.com/2012/10/arrows-are-simpler-than-they-appear/
[deterministic_error_correcting_combinator_parsers]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.80.9967&rep=rep1&type=pdf
[generalising_monads_to_arrows]: http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf
[yampa]: https://wiki.haskell.org/Yampa
[netwire]: https://wiki.haskell.org/Netwire

## Chapter 4: Patterns of Folding and Traversing

With the introduction of Applicative came more powerful mapping (traversal),
 which opened the door to type-level folding and mapping in Haskell. The Edward
 Kmett's Lens library (2012) raises Foldable and Traversable to an even higher
 level of abstraction. It has been described as a "query language for data
 structures".<br>
[Foldl as foldr][foldl_as_foldr]<br>
*"We introduce the type class Traversable, capturing functional data structures
 through which we can thread an Applicative computation."*<br>
[The Essence of the Iterator Pattern][essence_of_iterator_pattern]<br>
[Foldable Traversable In Prelude][foldable_traversable_in_prelude]<br>
[Lens Tutorial - Introduction (part 1)][lens_tutorial_intro1]<br>
We compose Lenses using the regular function composition `(.)`! The order of
 composition is reversed. To focus on more than one part of a structure, we
 need a `Traversal`, the Lens generalization of `Traversable`. As Lens relies on
 `Functor`, `Traversal` relies on Applicative. Traversals compose seamlessly
 with `Lens`es and Traversals. (`.mapped`) We get `Traversal`s by simply
 swapping out functor for Applicative in the Lens type definition.<br>
The `Lens.Traversal` function lifts `Traversable` into the realm of lenses,
 while `Lens.Fold` does the same for `Foldable`.<br>
Lens library function names do their best not to class with existing names; for
 example, `sumOf` or `droppingWhile` instead of `dropWhile`.

```haskell
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b -- in Control.Monad

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

type Lens' s a = Functor f' => (a -> f' a) -> s -> f' s
-- view|set|over|mapMOf lens v|f s

type Lens s t a b = Functor f' => (a -> f' b) -> s -> f' t
type Lens' s a = Lens s s a a

mapMOf :: Profunctor p => Over p (WrappedMonad m) s t a b -> p a (m b) -> s -> m t
foldMapOf :: Profunctor p => Accessing p r s a -> p a r -> s -> r
```

[foldl_as_foldr]: https://wiki.haskell.org/Foldl_as_foldr
[essence_of_iterator_pattern]: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
[foldable_traversable_in_prelude]: https://wiki.haskell.org/Foldable_Traversable_In_Prelude
[lens_tutorial_intro1]: https://blog.jakuba.net/2014/07/14/Lens-Tutorial---Introduction-part-1/

## Chapter 5: Patterns of Type Abstraction

* abstracting function types: RankNTypes
* abstracting datatypes: existential quantification, phantom types, generalized
  algebraic datatypes (GADTs), type case pattern, dynamic types, heterogeneous
  lists
* abstracting type-classes: multiparameter type-classes, functional dependencies

The rank of a type refers to the nesting depth at which polymorphism occurs.
 `Rank 0` describes absence of polymorphism. `Rank 1` refers to regular
 parametric polymorhpism.<br>
[Practical type inference for arbitrary-rank types][practical_arbitrary_rank_type_inference]

`ExistentialQuantification` - The existentially qualified object hides the type
 parameter instead of the universally qualified object. We can access an
 encapsulated object property only with the functions packaged with that
 property. Existential quantification provides us with the means to implement
 abstract datatypes, thus providing functions over a type while hiding the
 representation of the type.<br>
Phantom types were introduced in 1999 as a solution to the challenges that arise
 when embedding a type-safe domain specific language (DSL) in Haskell. (See
 [Fun with phantom types][fun_with_phantom_types]) For example, the `Lens`
 library uses the const phantom type to great effect (See
 [Derivation][derivation]).<br>
`GADTs` - phantom types + smart constructors + refined pattern matching. GADTs
 emerged independently from both ML and Haskell camps in 2003 and were a part of
 the GHC by 2005. This solves the problem of type inference that we had with
 phantom types. This is why GADTs are also known as
 [First-Class Phantom Types][first_class_phantom_types]. Generic programming is
 another important use-case for GADTs.<br>
[TypeCase: A Design Pattern for Type-Indexed Functions][typecase_design_pattern_for_type_indexed_functions]<br>
Dynamic types carry enough type information about themselves to enable safe type
 casting. (See [Generalized Algebraic Data Types in Haskell][gadt_in_haskell])

```haskell
data Rep t where
  RInt  :: Rep Int
  RChar :: Rep Char
  RList :: Show a => Rep a -> Rep [a]
  RDyn  :: Rep Dynamic

data Dynamic where
  Dyn :: Show t => Rep t -> t -> Dynamic

instance Show Dynamic where
  show (Dyn rep v) = showT rep v

{-# LANGUAGE FunctionalDependencies #-}

class Coerce2 a b | b -> a where
  coerce2 :: a -> b
```

The extension to multiparameter type-classes demands that we specify relations
 between type parameters by way of functional dependencies. The functional
 dependency `b -> a` tells the compiler that `b` determines `a` uniquely.
 Moreover, the compiler can now prevent us from adding conflicting instance
 declarations.
 [Type Classes with Functional Dependencies][type_classes_with_functional_dependencies]

[practical_arbitrary_rank_type_inference]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf
[fun_with_phantom_types]: http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf
[derivation]: https://github.com/ekmett/lens/wiki/Derivation
[first_class_phantom_types]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.596.7907&rep=rep1&type=pdf
[typecase_design_pattern_for_type_indexed_functions]: http://www.cs.ox.ac.uk/jeremy.gibbons/publications/typecase.pdf
[gadt_in_haskell]: https://themonadreader.files.wordpress.com/2013/08/issue221.pdf
[type_classes_with_functional_dependencies]: https://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf

## Chapter 6: Patterns of Generic Programming

[Datatype-Generic Programming][datatype_generic_programming]<br>
[Template meta-programming for Haskell][template_meta_programming_for_haskell]<br>
The Derivable type-classes enable code generation for type-class instances.
 Haskell 98 included autoderivation for the `Eq`, `Ord`, `Enum`, `Bounded`,
 `Show`, and `Read` type classes. Haskell 98 Derivable type-classes were
 achieved through compiler analysis of the structure of the derivable datatypes,
 that is, a form of metaprogramming. As there was no unifying formalism
 underlying the different types, the early Derivable type-classes represent only
 a rudimentary form of generic programming. A second generation of Derivable
 type-classes was added to the Glasgow Haskell Compiler (GHC) in stages for
 `Data`, `Typeable`, `Functor`, `Foldable`, `Traversable`, and `Generic`. The
 `GeneralizedNewtypeDeriving` language extension allows a `newtype` declaration
 to inherit some or all of the type-class instances of an inner type.<br>
Since the Haskell type system is not strong enough to express type laws in
 general, they are not enforceable by the compiler. This limitation in Haskell
 is a strong motivator for Derivable type classes.<br>
Instead of defining functions for ad hoc types, we deconstruct our types into a
 more fundamental type representation and then write generic functions against
 the lower-level type representation instead. This is datatype-generic
 programming---writing generic functions parameterized by the shape of the
 datatype. It explains why datatype-generic programming is also said to exhibit
 shape polymorpism, structure polymorphism, or polytypism.

First, we define a type representation. We follow the generic programming style
 of
 [A Lightweight Implementation of Generics and Dynamics][lightweight_generics_dynamics]
 as revised in
 [Libraries for Generic Programming in Haskell][libraries_generic_programming_haskell].<br>
In 2010, a more generic approach was introduced to synthesize (and surpass) the
 Derivable type-classes of Haskell 98 (refer to
 [A Generic Deriving Mechanism for Haskell][generic_deriving_mechanism_for_haskell]).
 This work has found its way into the Haskell ecosystem in the form of the
 GHC.Generics library.

```haskell
data U = U
data Choice a b = L a | R b
data Combo a b = Combo a b

type RList a = Choice U (Combo a (List' a)) -- shallow type representation
type RTree a = Choice (Combo U a) (Combo a (Combo (Tree a) (TRee a)))

fromL :: List' a -> RList a
fromL Nil'         = L U
fromL (Cons' x xs) = R (Combo x xs)

toL :: RList a -> List' a
toL (L U)            = Nil'
toL (R (Combo x xs)) = (Cons' x xs)

data EP d r = EP {from :: (d -> r),
                  to :: (r -> d)}

data TypeRep t where
  RUnit   :: TypeRep U
  RChoice :: TypeRep a -> TypeRep b -> TypeRep (Choice a b)
  RCombo  :: TypeRep a -> TypeRep b -> TypeRep (Combo a b)
  RInt    :: TypeRep Int
  RType   :: EP d r -> TypeRep r -> TypeRep d

rList :: TypeRep a -> TypeRep (List' a)
rList tr = RType (EP fromL toL) -- rList RInt
                 (RChoice RUnit (RCombo tr (rList tr))) -- RType ep listRep

gSize :: TypeRep a -> a -> Int
gSize (RChoice trA trB) (L a) = gSize trA a
gSize (RType ep tr)     t     = gSize tr (from ep t)
```

origami programming, a style of generic programming that focuses on the core
 patterns of recursion: `map`, `fold` and `unfold`.
 [Origami Programming (The Fun of Programming)][origami_programming]

```haskell
data Fix s a = FixT {getFix :: s a (Fix s a)} -- shape instance-of-the-type
-- fixed point of a function: f (fix f) = fix f
```

[datatype_generic_programming]: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/dgp.pdf
[template_meta_programming_for_haskell]: https://www.microsoft.com/en-us/research/publication/template-meta-programming-for-haskell/
[lightweight_generics_dynamics]: https://www.cs.ox.ac.uk/people/ralf.hinze/publications/HW02.pdf
[libraries_generic_programming_haskell]: http://www.dreixel.net/research/pdf/lgph.pdf
[generic_deriving_mechanism_for_haskell]: http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf
[origami_programming]: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/origami.pdf

## Chapter 7: Patterns of Kind Abstraction

