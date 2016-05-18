# [Learn You a Haskell for Great Good!][learnyouahaskell] by Miran Lipovača, No Starch Press (2011)

[learnyouahaskell]: http://learnyouahaskell.com/

## 1. Introduction

Haskell is 1) a purely functional programmig language, 2) lazy,
 3) statically typed, 4) elegant and typed, 5) made by really smart guys.

install `haskell-platform` pacakge<br>
in interactive `ghci`, `:r` (reload) or `:l myfunctions` after loading scripts<br>
Haskell committee started in 1987, and Haskell Report was published in 2003.

## 2. Starting Out

`:set prompt "ghci> "`<br>
`True == 5` -- error<br>
`` `infix` `` form, `succ` <-> `pred`, `min` <-> `max` functions<br>
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

If a function is comprised only of special characters, it's considered an infix function by default. If we want to examine its type, pass it to another function or call it as a prefix function, we have to surround it in parentheses.<br>
``5 `compare` 3`` infix function<br>
`fromIntegral :: (Num b, Integral a) => a -> b` -- class constraint =><br>
typeclass - interface that defines some behaviors
* `Eq` - `==`, `/=`, all standard Haskell types except for IO and functions
* `Ord` - ordering
* `Show` - can be presented as strings, `show`
* `Read` - `read`, may need a type annotation, `read "['a','b']" :: [Char]`
* `Enum` - `succ`, `pred`, (), Bool, Char, Ordering ([LT .. GT]), Int, Integer, Float, Double
* `Bounded` - `minBound :: Int`, `maxBound :: (Bounded a) => a`
* `Num`
 * `Integral` - Int, Integer
 * `Floating` - Float, Double

## 4. Syntax in Functions

```haskell
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- "patterns" ...@...
-- (x:xs) cannot be represented by syntactic sugar form [x, blah].
-- you cannot pattern match against (xs ++ ys).
```

```haskell
a `myCompare` b -- can define functions with backticks, too.
	| a > b     = GT
	| a == b    = EQ
	| otherwise = LT -- "otherwise" is defined as "True".
-- "guard"
```

```haskell
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0) -- need to align within the same block ???
-- "where" bindings
--   are just syntactic constructs
--   are applied across guards
--   appear after functions
--   can be nested
```

```haskell
-- "let" bindings are expressions and can bind variables anywhere.
let a = 100; b = 200; c = 300 [;] in a*b*c
let (a,b,c) = (1,2,3) in a+b+c
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
-- if you omit the "in" part in list comprehensions, defined names are visible
--  to the output function (before "|") and sections after the binding.
-- if you omit the "in" part in GHCi, defined names are visible in the entire session.
```

```haskell
-- pattern matching in function definitions is syntactic sugar form of "case" expressions.
head' [] = error "No head for empty lists!"
head' (x:_) = x
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
```

## 5. Recursion

`Num` is not a subclass of `Ord`.<br>
often the edge case value turns out to be an identity.

## 6. Higher Order Functions

curried function - partially applied function<br>
`max :: (Ord a) => a -> (a -> a)` right-associative<br>
`(argument infix)` or `(infix argument)` form, `(subtract 4)` instead of `(-4)`<br>
`zipWith` map with zip, `map`, `filter`, `takeWhile` for infinite lists<br>
`length :: [a] -> Int`, `fromIntegral :: (Integral a, Num b) => a -> b`<br>
lambda - can have only one pattern, runtime error when pattern matching fails
```haskell
addThree x y z = x + y + z
addThree = \x y z -> x + y + z
addThree = \x -> \y -> \z -> x + y + z
```
`foldl` \acc x -> ... from the left, `foldr` \x acc -> ... from the right<br>
`foldl1`, `foldr1` uses the first (or last) list element as the starting value<br>
`reverse' = foldl (flip (:)) []`<br>
`scan[l|r][1]` reports all the intermediate accumulators and shows the progress of fold functions<br>
`scanr (+) 0 [3,5,2,1] -- [11,8,3,1,0]`<br>
function application with `$` has the lowest precedence and is right-associative while function application with a space has the highest precedence and is left-associative.
```haskell
sqrt (3 + 4 + 9)
sqrt $ 3 + 4 + 9
map ($ 3) [(4+), (10*), (^2), sqrt]
```
function composition `.` (right-associative)
```haskell
f . g = \x -> f (g x)
map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
-- functions that take several parameters end with "$ last_param".
fn x = ceiling (negate (tan (cos (max 50 x)))) -- can't get rid of the x
fn = ceiling . negate . tan . cos . max 50

oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares -- use another variable of "let" part
    in  sum belowLimit
```

## 7. Modules

```haskell
ghci> :m + Data.List Data.Map Data.Set -- put functions into the global namespace
import Data.List (nub, sort) -- "import" must be located before any function definitions
import Data.List hiding (nub)
import qualified Data.Map [as M]
```

Data.List
* intersperse, intercalate, transpose
* foldl', foldl1' (strict/non-lazy version to avoid stack overflow errors)
* concat, concatMap
* and/or list - any/all func list, iterate, splitAt
* takeWhile/dropWhile - span/break, sort, group, inits/tails (scan version)
* isInfixOf/isPrefixOf/isSuffixOf, elem/notElem, partition
* find :: (a -> Bool) -> [a] -> Maybe a, data Maybe a = Nothing | Just a
* elemIndex/elemIndices, findIndex/findIndices, zip3-7, zipWith3-7
* lines/unlines, words/unwords, nub (unique element list while preserving the ordering)
* delete (only first occurrence)
* \\ (list difference), union, intersect, insert (into a sorted list)
* **generic**Length/Take/Drop/SplitAt/Index(!!)/Replicate returns Integral of Num instead of Int
* nub/delete/union/intersect/group**By** takes an equality function
* Data.Function.on f \`on\` g = \x y -> f (g x) (g y), for example, (==) \`on\` (> 0), compare \`on\` length
* sort/insert/maximum/minimum**By**, sort == sortBy compare

Data.Char
* is\*, generalCategory :: Char -> GeneralCategory
* toUpper, toLower, toTitle, digitToInt, intToDigit, ord, chr

Data.Map (Map k v, tree implementation require Ord typeclass, import qualified)
* lookup, fromList/toList, empty, singleton, insert, null, size, member
* map/filter (for values), keys, elems
* fromList/insert**With** (with a duplication handling function)

Data.Set (tree implementation, import qualified)
* fromList/toList, intersection, difference, union, isSubsetOf, isProperSubsetOf
* null, size, member, empty, singleton, insert, delete, map, filter

```haskell
module Geometry -- module files in a hierarchical structure
( sphereVolume -- or "*"
, sphereArea
) where

-- function definitions here
```

## 8. Making Our Own Types and Typeclasses

```haskell
module Shapes
( Point(..)
, Shape(Rectangle, Circle)
, surface
) where

-- type constructor = value constructors (functions)
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Eq, Show, Read)

-- record (Haskell defines functions automatically)
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     }
mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}

data (Ord k) => Map k v = ... -- recommend not to add typeclass constraints in data declarations
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- from Bounded, minBound :: Day, maxBound :: Day
-- from Enum, succ, pred, [Thursday .. Sunday], [minBound .. maxBound] :: [Day]

-- type synonyms
type String = [Char]
type IntMap v = Map Int v
type IntMap = Map Int

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- "a" is a possible failure reason, and "b" is a successful result.
infixl 7 *
infixl 6 +
```

```haskell
class Eq a where
    (==) :: a -> a -> Bool -- minimal complete definition for the typeclass
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class (Eq a) => Num a where
    ...

instance (Eq m) => Eq (Maybe m) where -- Maybe alone is not a concrete type.
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

ghci> :info type|type_constructor|function

-- for type constructors that are mappable and act like a box
--   for example, list, Maybe, custom Tree, Either a, Map k
class Functor f where
    fmap :: (a -> b) -> f a -> f b
instance Functor [] where
    fmap = map
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

:k (kind of a type)<br>
Int :: * (concrete type)<br>
Maybe :: * -> *

## 9. Input and Output

## 10. Functionally Solving Problems

## 11. Functors, Applicative Functors and Monoids

## 12. A Fistful of Monads

## 13. For a Few Monads More

## 14. Zippers

