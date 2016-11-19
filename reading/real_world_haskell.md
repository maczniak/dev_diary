# [Real World Haskell][homepage], by Bryan O'Sullivan, Don Stewart, and John Goerzen, O'Reilly (2008)

[homepage]: http://book.realworldhaskell.org/

## Why functional programming? Why Haskell?

> In Haskell, the sort-then-take approach actually performs well: laziness
> ensures that the list will only be sorted enough to find the *k* minimal
> elements.

Haskell 1.0 specification (1990), fifth revision (1998)

* [Haskell Hierarchical Libraries reference][haskell_library_ref]
* [Haskell 98 Report][haskell_98]
* [GHC User's Guide][ghc_user_guide]
* API search - [Hoogle][hoogle], [Hayoo][hayoo]
* libraries - [Hackage][hackage], [Haskell Wiki][libraries_wiki]
* [mailing lists][mailing_list], [Haskell Communities and Activities Report][community_report]

[haskell_library_ref]: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html
[haskell_98]: https://www.haskell.org/onlinereport/
[ghc_user_guide]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html
[hoogle]: https://www.haskell.org/hoogle/
[hayoo]: http://hayoo.fh-wedel.de/
[hackage]: http://hackage.haskell.org/
[libraries_wiki]: https://wiki.haskell.org/Applications_and_libraries
[mailing_list]: https://wiki.haskell.org/Mailing_lists
[community_report]: https://wiki.haskell.org/Haskell_Communities_and_Activities_Report

## 1. Getting started

GHC (Glasgow Haskell Compiler, ghc + ghci + runghc, version 6.8.2 in 2007) vs Hugs<br>
`Prelude>` prompt means the standard prelude (a standard library of useful functions).<br>
`:m[odule] + Data.Ratio` (module load), numerator `%` denominator<br>
`(+) 2 2`<br>
`:info (+)` -- show fixity rules, infixl/r 1(lowest)-9(highest)<br>
`^` integer power, `**` floating point number exponent<br>
`putStrLn` print string<br>
`:set/unset +t` (show the type of an expression also), `:t[ype] 'a'`<br>
ghci's `it` stores the result of the last expression we evaluated.

```haskell
-- file: ch01/WC.hs
main = interact wordCount
	where wordCount input = show (length (lines input)) ++ "\n"
```

## 2. Types and functions

`compare a b` returns one of `LT`, `EQ` and `GT`.<br>
type variable (not type name) starts with a lowercase letter.<br>
both the type and the value of `()` are "unit" (like C `void`).<br>
the result type of a function that has side effects begin with `IO`.<br>
`:cd` in ghci<br>
indentation matters. the exact amount is not important.<br>
The record that we use to track an unevaluated expression (and result) is referred to as a thunk.

## 3. Defining types, streamlining functions

## 4. Functional programming

## 5. Writing a library: working with JSON data

## 6. Using typeclasses

## 7. Input and output

`System.IO` module and `System.Directory` module

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a -- in not System.IO.Error but Control.Exception
finally :: IO a -> IO b -> IO a -- in Control.Exception
interact :: (String -> String) -> IO () -- in System.IO
```

lazy "input" and "output" on demand<br>
`readFile` uses `hGetContents` internally, and the underlying `Handle` will be closed when the returned `String` is garbage-collected or all the input has been consumed. `writeFile` will close its underlying `Handle` when the entire `String` supplied to it has been written.<br>
In some cases, you may need more control over exactly when your I/O occurs. In those cases, `hGetContents` will probably not be appropriate.<br>
`System.Console.GetOpt` module of `base` package

## 8. Efficient file processing, regular expressions, and file name matching

```haskell
handle :: (Exception -> IO a) -> IO a - > IO a -- like catch(), in Control.Exception
```

The `error` throws an exception. Pure Haskell code cannot deal with exceptions, so control is going to rocket out of our pure code into the nearest caller that lives in `IO` and has an appropriate exception handler installed.

## 9. I/O case study: a library for searching the filesystem

```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
  -- first -> last -> in-between, in Control.Exception
maybe :: b -> (a -> b) -> Maybe a -> b

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\_ -> return Nothing) (Just `liftM` act)
  -- instead of "case of"
```

A Haskell implementation will automatically close the file handle when it notices that the handle is no longer being used. That will not occur until the garbage collector runs.<br>
camel case

## 10. Code case study: parsing a binary data format

## 11. Testing and quality assurance

## 12. Barcode recognition

## 13. Data structures

## 14. Monads

## 15. Programming with monads

## 16. The Parsec parsing library

## 17. The foreign function interface

## 18. Monad transformers

## 19. Error handling

## 20. Systems programming

## 21. Working with databases

## 22. Web client programming

## 23. GUI programming

## 24. Basic concurrent and parallel programming

## 25. Profiling and tuning for performance

## 26. Advanced library design: building a Bloom filter

## 27. Network programming

## 28. Software transactional memory

## A. Installing GH and Haskell libraries

## B. Characters, strings, and escaping rules

