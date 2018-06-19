# [Practical Concurrent Haskell: With Big Data Applications][homepage] by Stefania Loredana Nita and Marius Mihailescu, Apress (2017)

unnatural sentences, [Concurrency at HaskellWiki][concurrency_haskellwiki]

8.0.1

[homepage]: https://www.apress.com/gp/book/9781484227800
[concurrency_haskellwiki]: https://wiki.haskell.org/Concurrency

## Part I: Haskell Foudations. General Introductory Notions

### Chapter 1: Introduction

[evaluation strategy][evaluation_strategy]<br>
There are two standards: [*The Haskell 98 Report*][haskell_98] and
 [*Haskell 2010 Language Report*][haskell_2010]<br>
[design for a `distributed-process` (Cloud Haskell) implementation][cloud_haskell_design]

[evaluation_strategy]: https://en.wikipedia.org/wiki/Evaluation_strategy
[haskell_98]: https://www.haskell.org/onlinereport/
[haskell_2010]: https://www.haskell.org/onlinereport/haskell2010/
[cloud_haskell_design]: http://haskell-distributed.github.io/wiki/newdesign.html

### Chapter 2: Programming with Hasekell

In May 2016, the Haskell community started working on the next version, Haskell
 2010.<br>
The recursion shows us what something is, rather than how it is computed.<br>
Variables represent names for expressions.<br>
In Haskell, the overloading is extended to values.<br>
Haskell assures that an `Int` is larger than 28 bits.<br>
A characteristic of the monadic bind is that it is strict.<br>
The implementation could be divided into multiple lines, using the `:{` and `:}`
 commands (vs multi-line comment, `{- ... -}`). There is another possibility
 that GHCi identifies when a statement is not terminated, and permits writing on
 multiple lines. It is enabled by the `:set +m` command.<br>
`:show bindings`, `:set +t` (find every variable's type)<br>
The star that accompanies the name of the module shows that the module
 represents the full top-level scope of the expressions from the prompt. In the
 absence of the asterisk, only the module's exports are observable.<br>
If the module keyword is accompanied by `+`, then the modules are added; if it
 is accompanied by `-`, then the modules are removed. If there is no `+` or `-`,
 the actual scope is substituted by the modules after the `:module` command.<br>
`:show imports`<br>
`-XTypeSynonymInstances`, `-XFlexibleInstances`, `-XUndecidableInstances`
 (?)<br>
The immutable arrays belong to the `Data.Array.IArray` module. Mutable arrays
 belong to the `Data.Array.IO` module. Another mutable array belongs to the
 `Data.Array.ST` module, which permits the use of mutable array from the `ST`
 monad (`ST s (STArray s Int Int)`, `main = print $ runST oldNewValue`). In
 addition to these two types of arrays, there are other types that belong to the
 following modules: `Data.Array.Diff`, `Data.Array.Unboxed`, and
 `Data.Array.Storable`.<br>
[wxHaskell applications][wxhaskell_applications]<br>
`data Data = Int Int Int deriving (Show)` (?)

[wxhaskell_applications]: http://wxhaskell.sourceforge.net/applications.html

### Chapter 3: Parallelism and Concurrency with Hasekll

Concurrent Hasekell appears as a simple library, `Control.Concurrent`. The
 following are the functions within this library: forking threads, killing
 threads, sleeping, synchronized mutable variables known as `MVars`, and bound
 threads<br>
The following are the main functionalities supported by this library: atomic
 blocks, transactional variables, operations for composing transactions (`retry`
 and `orElse`), and data invariants<br>
For running the programs on multiple processors and in parallel, you have to
 link your program with the `-threaded`, and run it with the `+RTS -Nnum`
 option.<br>
Glasgow Parallel Haskell (GPH) supports running Parallel Haskell programs using
 clusters of machines and single multiprocessors.<br>
There is a simple method for creating parallelism from pure code by using the
 `par` combinator (evaluate the first argument in parallel with the second),
 which is related to `seq`. The expression ``(x `par` y)`` sparks the evaluation
 of `x` (to weak head normal form) and return `y`. Sparks are queues used to
 execute in FIFO order, but they are not executed immediately. If there is any
 idle state in the CPU detected at run-time, a spark is converted into a real
 thread. `pseq` is used to force the parent thread to evaluate the first
 argument before going on the second. `pseq` is used often than `seq`. The
 arguments of `seq` function are evaluated in any order, but `pseq` function
 evaluates firstly its first argument, then the second one.<br>
More sophisticated combinators for expressing parallelism are avaliable from the
 `Control.Parallel.Strategies` module in the `parallel` package. This module
 builds functionality around `par`, expressing elaborate partterns of parallel
 computation, such as a parallel map.

### Chapter 4: Strategies Used in the Evaluation Process

### Chapter 5: Exceptions

### Chapter 6: Cancellation

### Chapter 7: Transactional Memory Case Studies

### Chapter 8: Debugging Techniques Used in Big Data

## Part II: Haskell for Big Data and Cloud Computing

### Chapter 9: Haskell in the Cloud

### Chapter 10: Haskell in Big Data

### Chapter 11: Concurrency Design Patterns

### Chapter 12: Large-Scale Design in Haskell

### Chapter 13: Designing a Shared Memory Approach for Hadoop Streaming Performance

### Chapter 14: Interactive Debugger for Development and Portability Applications Based on Big Data

### Chapter 15: Iterative Data Processing on Big Data

### Chapter 16: MapReduce

### Chapter 17: Big Data and Large Clusters

