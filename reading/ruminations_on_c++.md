# [Ruminations on C++][homepage] by Andrew Koenig and (editor) Barbara E. Moo, Addison-Wesley (1996)

source code in the homepage

(Koenig has been writing columns about C++ for a number of years, published in
 The C++ Journal, The C++ Report, and The Journal of Object-Oriented
 Programming.)

Knowledge can be acquired systematically; wisdom cannot.<br>
(Part II) You will also learn why it can be useful to conceal inheritance from
 its users and when to avoid inheritance altogether.<br>
Part III explores templates, which *I* think constitute the most important idea
 in C++. The reason I think templates are so important is that they provide a
 particularly powerful kind of abstraction.<br>
We are also grateful to the people ... Brian Kernighan (who gets extra credit
 for reading the entire book twice carefully with pen in hand), ...

(This work undoubtedly would have continued for some time being just a research
 project or at best would have resulted in an HP proprietary library, if Andrew
 Koenig of Bell Labs had not become aware of the work and asked Stepanov to
 present the main ideas at a November 1993 meeting of the ANSI/ISO committee for
 C++ standardization. The committee's response was overwhelmingly favorable and
 led to a request from Koenig for a formal proposal in time for the March 1994
 meeting. -- Standard Template Library, Wikipedia)

[homepage]: http://www.informit.com/store/ruminations-on-c-plus-plus-a-decade-of-programming-9780201423396

### Chapter 0 Prelude

Once upon a time, I met someone who has worked in many programming languages,
 but had never written a C or C++ program. He asked a question that made me
 think: "Can you convince me that I should learn C++ instead of C?"<br>
Dag Brück pointed out that putting efficiency first is a trademark of the C/C++
 culture.<br>
Indeed, the very notion that the state of a computation should be made explicit
 is fundamental to the whole idea of object-oriented programming. It is only a
 little too flip to say that the most fundamental difference between
 object-oriented programming and functional programming is that, in
 object-oriented programs, the state that results from a computation usually
 replaces the prior state, and, in functional programs, it never does.

## Part I Motivation

This book keeps returning to these two ideas: pragmatism and abstraction.

### Chapter 1 Why I use C++

The result of my effort to write a program without arbitrary limits was that
 most of the work in the program went into bookkeeping, rather than into solving
 the real problem.<br>
However, Bjarne Stroustrup's office was not far from mine, and we had talked
 about the language as it evolved.<br>
Moreover, I know from years of dealing with computer users that most of the
 people whose programs failed because they hadn't followed the conventions
 would blame me.<br>
There have been widely distributed C libraries, to be sure, but all the C
 libraries that I know of address specific problem domains. C++, in contrast,
 has enabled truly general-purpose libraries, where the library authors do not
 even know about the uses to which their work will be put. Such is the virtue of
 abstraction.

### Chapter 2 Why I work on C++

These chapters cover issues that differ in much the same way that using an
 automobile to commute to work differs from becoming an automobile designer!<br>
It is hard to help noticing how many of the most successful, best known software
 systems were originally the work of a small number of people. Similarly,
 surprisingly many large projects turn out to be mediocre at best. The answer, I
 think, is that unlike so many other industries, software construction has
 negative economy of scale. Most competent programmers cn dash off a program of
 a hundred lines in an hour or two, yet the total output from a large project is
 typically about 10 lines per programmer day.<br>
This communication overhead becomes serious as soon as there are too many people
 on the project to fit at one lunch table at the same time.<br>
I know that today I can easily solve problems that I would not have dreamed of
 trying when I first started programming---and I'm not alone. The tools that
 most consistently hold my interest have in common the notion of abstraction.
 Abstractions are so useful that successful programmers are constantly inventing
 them, and incorporating them into their programs.<br>
Even the notion of being able to write an expression, instead of a series of
 separate arithmetic operations, is powerful. This notion was unusual enough
 when first introduced in the fifties that it formed the basis for the name of
 FORTRAN: Formula Translation.<br>
Treated abstractly, many data structures have notions of initialization and
 termination that extend well beyond mere memory allocation. For example, a data
 structure that represents a buffered output file must somehow incorporate the
 idea that the buffer must be flushed before the file is closed.

### Chapter 3 Living in the real world

APL is more than a language---its first implementation included an entire
 operating system as well. By modern standards, that system was astonishingly
 efficient.<br>
It doesn't matter how wonderful a programming language is if its programs won't
 run in your environment. If we have to live with a body of existing software,
 and we can't rewrite all the software to fit our favorite tools, we must choose
 tools that will coexist with the software.<br>
Thus, Lisp programmers are at a serious disadvantage when they have to deal with
 data that do not fit the S-expression model neatly. It is hard to write Lisp
 programs that deal with, say, magnetic tapes of census data, or with strange
 I/O devices such as local area networks or wind tunnels. I have met Lisp
 programmers who react to these disadvantages by arguing that the problems would
 go away if only the whole world used Lisp and noting else.<br>
But it gives C++ two extremely important attributes: portability and
 coexistence. If a machine supports C, there is no fundamental difficulty in
 getting it to support C++, which means that C++ can run almost anywhere.<br>
C++ thrives in mixed environments. It lives in the real world!

## Part II Classes and inheritance

As every C++ programmer should know, run-time polymorphism occurs *only* when a
 program calls a virtual function through a pointer or reference to a base-class
 object. Less obvious may be the implications of this model. In particular, the
 fact that object creation and copying are *not* run-time polymorphic profoundly
 affects class design. Thus, containers can hold only values whose types are
 known at compile time.<br>
A more natural approach for C++ is to define a class whose purpose is to provide
 *and hide* the indirection. Such classes are often called *handle classes*. In
 its simplest form, a handle class lets us attach an object of a single type to
 an object of any type in a particular inheritance hierarchy. Handles therefore
 let us ignore the precise type with which we're dealing, while still avoiding
 the memory-management problems that come with pointers.

### Chapter 4 Checklist for class authors

If you don't want users to be able to copy objects of a class, make the copy
 constructor (and probably also the assignment operator) private. Usually,
 `operator=` should return an `X&` and end by saying `return *this;` for
 consistency with the built-in assignment operators. If we free the old
 destination value before copying the source value, and the source and
 destination are the same object, then we might wind up freeing the source
 before we copy it.<br>
The curious `[]` syntax comes from the effort of C++ to balance C compatibility
 with efficiency. A C++ implementation that does not wish to preempt an existing
 C implementtion's `malloc` must therefore implement `new` directly in terms of
 that `malloc`, and must not supply its own. Hence, the C++ library cannot
 necessarily figure out how large an array is when freeing that array. Even
 though `malloc` must store that size, it may store the size someplace that the
 C++ library cannot portably access. As a compromise, C++ therefore requires its
 users to tell the implementation whether what is being deleted is an array.<br>
`Complex operator+(const Complex& x, const Complex& y);` Otherwise, expressions
 such as `x+y+z` become impossible, because `x+y` is not an lvalue and therefore
 may not have a non`const` reference bound to it.<br>
Causing all classes to have virtual destructors automatically would violate the
 C++ philosophy of making people pay for only what they use; it would be
 analogous to leaving the landing gear extended all the time.

### Chapter 5 Surrogate classes

`Vehicle parking_lot[1000];` In effect, we have said that `parking_lot` is a
 collection of `Vehicle`s, not a collection of objects of classes derived from
 `Vehicle`.

In either alternative, the value of the operand of delete may be a null pointer
 value. / The free function causes the space pointed to by ptr to be
 deallocated, that is, made available for further allocation. If ptr is a null
 pointer, no action occurs. (from C++/C standards)

### Chapter 6 Handles: Part 1

This chapter will examine another kind of class, typically called a *handle*,
 that will allow us to avoid copying objects unnecessarily, while preserving the
 polymorphic behavior of surrogates. What we need is a way to obtain some of the
 advantages of pointers, particularly the ability to avoid copying objects and
 to use objects polymorphically, while avoiding some of the disadvantages, such
 as the lack of safety. Because handles behave somewhat like pointers, people
 sometimes also call them *smart pointers*. However, handles can behave
 differently enough from pointers that thinking of them as just a kind of
 pointer is probably too restrictive.<br>
At one point, we will show where a small change in the implementation of the
 handle class produces a large change in behavior.<br>
To hide that address, we must avoid `operator->`, nd must choose explicitly
 which `Point` operations we want our handles to make available.<br>
The use count cannot, of course, be part of the handle. Nor can the use count be
 part of the object.<br>
When we come to the modification functions, however, things suddenly become
 interesting. The reason is that we must now decide whether we want our handles
 to have value semantics or pointer semantics. More generally, the difference
 between objects and values shows up only when we try to change the objects.

### Chapter 7 Handles: Part 2

### Chapter 8 An object-oriented program

Whenever you need a type field, you should stop to think whether defining a set
 of classes related by inheritance might solve the problem more effectively.

### Chapter 9 Analysis of a classroom exercise: Part 1

### Chapter 10 Analysis of a classroom exercise: Part 2

### Chapter 11 When not to use virtual functions

## Part III Templates

### Chapter 12 Designing a container class

### Chapter 13 Accessing container elements

### Chapter 14 Iterators

### Chapter 15 Sequences

### Chapter 16 Templates as interfaces

### Chapter 17 Templates and generic algorithms

### Chapter 18 Generic iterators

### Chapter 19 Using generic iterators

### Chapter 20 Iterator adaptors

### Chapter 21 Function objects

### Chapter 22 Function adaptors

## Part IV Libraries

### Chapter 23 Libraries in everyday use

### Chapter 24 An object lesson in library-interface design

### Chapter 25 Library design in language design

### Chapter 26 Language design is library design

## Part V Technique

### Chapter 27 Classes that keep track of themselves

### Chapter 28 Allocating objects in clusters

### Chapter 29 Applicators, manipulators, and function objects

### Chapter 30 Decoupling application libraries from input-output

## Part VI Wrapup

### Chapter 31 Simplicity through complexity

### Chapter 32 What do you do after you say `Hello world`?

