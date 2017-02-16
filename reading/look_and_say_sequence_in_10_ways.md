# [개미 수열을 푸는 10가지 방법][homepage], 한주영 저, Leanpub (2017)

[source code][source_code]

[homepage]: https://leanpub.com/programming-look-and-say/
[source_code]: https://github.com/jooyunghan/programming-look-and-say-src

## 1장 개미 수열 시작하기

```haskell
ant = iterate (group >=> sequence [length, head]) [1]
```

[wishful thinking][wishful_thinking] in SICP

[wishful_thinking]: https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_idx_1306

## 2장 정규표현식

[Regular expressions in lexing and parsing][regex_in_lexing_and_parsing] by Rob Pike
> Standard lexing and parsing techniques are so easy to write, so general, and so adaptable there's no reason to use regular expressions. They also result in much faster, safer, and compact implementations.
> regular expressions' strength is that they provide a way to express patterns dynamically.
> When I do code reviews involving regular expressions, I fix up a far higher fraction of the regular expressions in the code than I do regular statements.

이 책은 앞으로도 개미 수열이라는 특정 문제를 풀겠지만 `replace()` 처럼 특정
 애플리케이션에 국한되지 않는 API 나 패턴을 찾아내려 노력할 것이다.

[regex_in_lexing_and_parsing]: https://commandcenter.blogspot.com/2011/08/regular-expressions-in-lexing-and.html

## 3장 리스트 처리

[TotallyLazy][totallylazy] (Java), [Ramda][ramda] (JavaScript)

[totallylazy]: https://totallylazy.com/
[ramda]: http://ramdajs.com/

## 4장 이터레이터

[마이클 잭슨의 구조 불일치 (Structure Clash)][jackson_structured_programming]

[jackson_structured_programming]: https://en.wikipedia.org/wiki/Jackson_structured_programming

## 5장 제너레이터

JavaScript [function*][js_function_star] declaration<br>
[java-generator-functions][java_generator_functions]<br>
[generator][generator] (Wikipedia)

[js_function_star]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function*
[java_generator_functions]: https://github.com/mherrmann/java-generator-functions/
[generator]: https://en.wikipedia.org/wiki/Generator_(computer_programming)

## 6장 코루틴

[coroutine][coroutine] (Wikipedia)<br>
[Go Concurrency Patterns][go_concurrency_patterns]<br>
[Coroutines in C][coroutines_in_c]<br>
[js-csp][js_csp]

[coroutine]: https://en.wikipedia.org/wiki/Coroutine
[go_concurrency_patterns]: https://talks.golang.org/2012/concurrency.slide#25
[coroutines_in_c]: http://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
[js_csp]: https://github.com/ubolonton/js-csp

## 7장 CPS

## 8장 CSP와 인터프리터 패턴

[CSP (communicating_sequential_processes)][csp]<br>
[Advanced Go Concurrency Patterns][advanced_go_concurrency_paterns]

[csp]: https://en.wikipedia.org/wiki/Communicating_sequential_processes
[advanced_go_concurrency_paterns]: https://talks.golang.org/2013/advconc.slide#6

## 9장 지연리스트

[Why Functional Programming Matters][why_functional_programming_matters] (with summary and videos) by John Hughes

[why_functional_programming_matters]: http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf

## 10장 Reactive

