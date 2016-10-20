# [Masterminds of Programming][homepage] by Federico Biancuzzi and Chromatic (Shane), O'Reilly (2009)

[homepage]: http://shop.oreilly.com/product/9780596515171.do

## 1. C++ (Bjarne Stroustrup)

### Design Decisions

### Using the Language

[Learning Standard C++ as a New Language][learning_standard_cplusplus_as_a_new_language], [Technical Report on C++ Performance][cplusplus_performance_technical_report], [Why C++ is not just an Object-Oriented Programming Language][why_cplsplus_is_not_just_an_oop_language]<br>
I have never seen a program that could be written better in C than in C++. I don’t think such a program could exist.<br>
I don’t want to represent C++ as “an OO language” or “a GP language”

[learning_standard_cplusplus_as_a_new_language]: http://www.stroustrup.com/new_learning.pdf
[cplusplus_performance_technical_report]: http://www.open-std.org/JTC1/sc22/wg21/docs/TR18015.pdf
[why_cplsplus_is_not_just_an_oop_language]: http://www.stroustrup.com/oopsla.pdf

### OOP and Concurrency

I have the feeling that the desire to make reusable objects makes things more complicated and, in the end, it doubles the workload. First, you have to design a reusable tool.<br>
"Refactoring” is especially popular as an attempt to address that problem by the brute force technique of simply reorganizing the code when it has outlived its initial interface design.<br>
In fact, distributed computing was my Ph.D. topic and I have followed that field ever since. However, people who first consider concurrency, multicores, etc., often confuse themselves by simply underestimating the cost of running an activity on a different processor.

### Future

[C++ Standards Committee][cplusplus_standards_committee], [C++11 FAQ][cplusplus11_faq] instead of C++0x FAQ<br>
The purpose of a (nonexperimental) programming language is to help build good systems.

[cplusplus_standards_committee]: http://www.open-std.org/jtc1/sc22/wg21/
[cplusplus11_faq]: http://stroustrup.com/C++11FAQ.html

### Teaching

Much computer science research is either too remote from everyday problems (even from conjectured future everyday problems), or so submerged in such everyday problems that it becomes little more than technology transfer.

## 2. Python (Guido van Rossum)

### The Pythonic Way

(about PEP) We were one of the first open source projects at the time to have something like this, and it’s been relatively widely copied.<br>
I should add that despite Python’s support for some functional tools like `map()` and `lambda`, Python does not have a functional-language subset: there is no type inferencing, and no opportunity for parallelization.<br>
Python probably has the reputation of supporting functional programming based on the inclusion of `lambda`, `map`, `filter`, and `reduce` in the language, but in my eyes these are just syntactic sugar, and not the fundamental building blocks that they are in functional languages.<br>
Early versions of the Python tutorial used a slogan something like “Python bridges the gap between C and shell programming,” because that was where I was myself, and the people immediately around me. ... The fact that it was useful for teaching first principles of programming in a middle school or college setting or for self-teaching was merely a lucky coincidence, enabled by the many ABC features that I kept---ABC was aimed specifically at teaching programming to nonprogrammers.

### The Good Programmer

The earliest intentions I had for Python were simply for it to be a language to be used in cases where C was overkill and shell scripts became too cumbersome.<br>
One specific thing to watch out for is regular expressions?it is easy to write a regular expression that runs in exponential time, so web applications that implement searches where the end user types in a regular expression should have some mechanism to limit the running time.

### Multiple Pythons

I also still don’t believe that trying to remove the GIL from CPython will work. I do believe that some support for managing multiple processes (as opposed to threads) is a piece of the puzzle, and for that reason Python 2.6 and 3.0 will have a new standard library module, multiprocessing, that offers an API similar to that of the threading module for doing exactly that. As a bonus, it even supports processes running on different hosts!

### Expedients and Experience

## 3. APL (Adin D. Falkoff)

## 4. Forth (Charles H. Moore)

## 5. BASIC (Thomas E. Kurtz)

## 6. AWK (Alfred Aho, Peter Weinberger and Brian Kernighan)

## 7. Lua (Luiz Henrique de Figueiredo and Roberto Ierusalimschy)

## 8. Haskell (Simon Peyton Jones, Paul Hudak, Philip Wadler and John Hughes)

## 9. ML (Robin Milner)

## 10. SQL (Don Chamberlin)

## 11. Objective-C (Tom Love and Brad Cox)

## 12. Java (James Gosling)

## 13. C# (Anders Hejlsberg)

## 14. UML (Ivar Jacobson, James Rumbaugh and Grady Booch)

## 15. Perl (Larry Wall)

## 16. PostScript (Charles Geschke and John Warnock)

## 17. Eiffel (Bertrand Meyer)

