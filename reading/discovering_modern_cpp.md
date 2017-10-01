# [Discovering Modern C++: An Intensive Course for Scientists, Engineers, and Programmers][homepage] by Peter Gottschling, Addison-Wesley (2016)

C++ In-Depth Series<br>
[source code][source_code], [errata][errata]

[homepage]: https://www.pearson.com/us/higher-education/program/Gottschling-Discovering-Modern-C-An-Intensive-Course-for-Scientists-Engineers-and-Programmers/PGM325821.html
[source_code]: https://github.com/petergottschling/discovering_modern_cpp
[errata]: http://www.simunova.com/errata_dmc

## Chapter 1. C++ Basics

[cplusplus.com][cplusplus_com], [cppreference.com][cppreference_com]<br>
`std::this_thread::sleep_for(std::chrono::seconds(1));` with `<chrono>` and
 `<thread>` (C++11)

Declare variables as late as possible. This makes programs more readable when
 they grow long. It also allows the compiler to use the memory more efficiently
 with nested scopes.<br>
`int b= 0b101'1001'0011'1010'1101'1010'0001;` (C++14)<br>
`std::string s2= "In C++ better like this";` with `<string>`<br>
`long l= {1234567890123};` Uniform Initialization (or Braced Initialization),
 values in braces cannot be narrowed (C++11)<br>
Do not use global variables. If you do use them, sooner or later you will regret
 it.<br>
As opposed to macros, an obsolete and reckless legacy feature from C that should
 be avoided at any price because it undermines all structure and reliability of
 the language.<br>
`static` variables live till the end of the execution but are only visible in
 the scope.

`throw` **operator**<br>
In short, the increment and decrement operations need something that is
 modifiable and addressable. The technical term for an addressable data item is
 *Lvalue*.<br>
In general, it is better to refrain from using increment and decrement in
 mathematical expressions. It is easier for human readers to understand and for
 the compiler to optimize when mathematical expressions have no *Side
 Effects*. In general, it is dangerous to modify values within expressions.<br>
Always use `bool` (not `int`) for logical expressions.<br>
member selection `x.m`, dereferred member selection `p->m`, [dereferred] member
 dereference `x.*q`<br>
`sizeof...(p)` (number of arguments), `alignof(x)` (alignment, C++11)<br>
operators the cannot be overloaded: `::`, `.` (may in C++17), `.*`, `?:`,
 `sizeof`, `sizeof...`, `alignof`, `typeid`<br>
As a rule of thumb: more comprehensible programs have a better potential for
 optimization.

An example where the choice between `if` and `?:` makes a difference is the
 `replace_copy` operation in the STL.

In contrast to mutable references, constnant ones allow for passing temporaries
 (temporary variables).<br>
The good news is that compilers are smart enough to elide the copy of the return
 value in many cases. In addition, the move semantics where data of temporaries
 is stolen avoids copies when the before-mentioned elision does not apply.
 Advanced libraries avoid returning large data structures altogether with a
 technique called expression templates and delay the computation until it is
 known where to store the result.

`<cassert>` and `-DNDEBUG`<br>
For rethrowing the current one, there exists a shorter notation: `throw`<br>
C++11 added a new qualification (`noexcept`) for specifying that no exceptions
 must be thrown out of the function. If an exception is thrown despite the
 qualification, the program is terminated. In templated functions, it can depend
 on the argument type(s) whether an exception is thrown. To handle this
 properly, `noexcept` can depend on a compile-time condition. Program errors
 that can already be detected during compilation can raise a `static_assert`.

In this regard, `endl` and `\n` differ: the former flushes the buffer while the
 latter does not. Flushing can help us when we are debugging (without a
 debugger) to find out between which outputs the program crashes. In contrast,
 when a large amount of text is written to files, flushing after every line
 slows down I/O considerably.<br>
(`<iomanip>`) `setw` changes only the next output while `setprecision` affects
 all following (numerical) output, like the other mainipulators. The provided
 width is understood as a minimum. We can further request that the values be
 left aligned (`left`), and the empty space be filled with a character of our
 choice (`setfill`). Another way of formatting is setting the flags directly.
 Some less frequently used format options can only be set this way, e.g.,
 whether the sign is shown for positive values as well
 (`cout.setf(ios_base::showpos)`). Furthermore, we force the `scientific`
 notation in the normalized exponential representation. Integer numbers can be
 represented in octal (`oct`) and hexadecimal (`hex`) base (`dec`). Boolean
 values are by default printed as integers 0 and 1. On demand, we can present
 them as true and false (`boolalpha`). Finally, we can reset all the format
 options that we changed
 (`cout.unsetf(ios_base::adjustfield | ios_base::basefield | ios_base::floatfield | ios_base::showpos | ios_base::boolalpha)`
 and `cout.precision(old_precision)`)<br>
Although the file does not exist, the opening operation does not fail. We can
 even read from the non-existing file and the program goes on. By default, the
 streams do not throw exceptions. The reason is historical: they are older than
 the exceptions and later the behavior was kept to not break software written in
 the meantime. To be sure that everything went well, we have to check error
 flags (`infile.good()`), in principle, after each I/O operation. If you want to
 use exceptions, we have to enable them
 (`stream.exceptions(ios_base::badbit | ios_base::failbit)`) during run time for
 each stream.

In standard C++, the size of the array must be constant and known at compile
 time. Some compilers (e.g., `gcc`) support run-time sizes. This feature was
 considered for C++14 but its inclusion postponed as not all subtleties were
 entrirely clarified.<br>
`nullptr` (C++11, or `int* ip4{};`), `0` (C++03), `NULL`<br>
The memory should be allocated in the object construction and deallocated in its
 destruction. This principle is called *Resource Allocation Is Initialization*
 (RAII). (It is a paradigm mainly developed by Bjarne Stroustrup and Andrew
 Koenig. The idea is tying resources to objects (that own resources) and using
 the mechanism of object construction and destruction to handle resources
 automatically in programs.)<br>
The problem with so-called *Raw Pointers* is that we have no notion whether a
 pointer is only referring to data or also in charge of releasing the memory
 when it is not needed any longer. To make this distinction explicit at the type
 level, we can use *Smart Pointers*. Three new smart-pointer types are
 introduced in C++11: `unique_ptr`, `shared_ptr`, and `weak_ptr`. The already
 existing smart pointer from C++03 named `auto_ptr` is generally considered as a
 failed attempt on the way to `unique_ptr` since the language was not ready at
 the time. It should not be used anymore. All smart pointers are defined in the
 header `<memory>`. If you cannot use C++11 features on your platform, the smart
 pointers in Boost are a decent replacement.<br>
Unique pointers (`.get()`) cannot be assigned to other pointer types or
 implicitly converted. It can only be moved. Unique pointer has a special
 implementation for arrays. An important benefit of `unique_ptr` is that it has
 absolutely no overhead over raw pointers: neither in time nor in memory. An
 advanced feature of unique pointers is to provide our own *Deleter*. (They are
 called whenever managed memory is released.)<br>
If possible, a `shared_ptr` (`.use_count()`) should be created with
 `make_shared`.<br>
For managing memory dynamically, there is no alternative to pointers. To only
 refer to other objects, we can use another language feature called
 `Reference`.<br>
As a compromise between pointers and references, C++11 offers a
 `reference_wrapper` class which behaves similarly to references but avoids some
 of their limitations.<br>
A `valarray` is a one-dimensional array with element-wise operations. We
 recommend using established C++ libraries for linear algebra. Hopefully, future
 standards will contain one.

> Almost every macro demonstrates a flaw in the programming language, in the
> program, or the programmer. -- Bjarne Stroustrup
Visual Studio defines--even today!!!--`min` and `max` as macros, and we strongly
 advise you to disable this by compiling with `/DNO_MIN_MAX`.<br>
Double quotes is equivalent to quoting with angle brackets and adding the
 current directory to the search path. The slashes are portable and also work
 under Windows.<br>
`#pragma once` is not part of the standard but all major compilers support it
 today.

[Matrix Market][matrix_market] data format

[cplusplus_com]: http://www.cplusplus.com
[cppreference_com]: http://en.cppreference.com/w/
[martrix_market]: http://math.nist.gov/MatrixMarket/

## Chapter 2. Classes

[Hourglass Interfaces for C++ APIs - CppCon 2014][hourglass_interfaces_for_cpp_apis]

Class members before the first modifier are all `private`.<br>
Within our class, we can grant free functions and classes (`friend`) the special
 allowance to access `private` and `protected` members.<br>
`(*p).r` or `p->r`

A member data item of a class type is implicitly default-constructed when it is
 not contained in the initialization list. All member variables are constructed
 before the constructor body is reached. The rule is that names in the
 initialization list outside the parentheses always refer to members. Inside the
 parentheses, the names follow the scoping rules of a member function.<br>
`complex z2()` (not `complex z1`) looks absolutely like a call for the default
 constructor but it is interpreted as the declaration of the function named
 `z2`. Scott Meyers called this interpretation the *Most Vexing Parse*.
 Construction with a single argument can be written with an assignment-like
 notation (`complex z4= 4`).<br>
Define a default constructor whenever possible. For some classes, however, it is
 very difficult to define a default constructor, e.g., when some of the members
 are references or contain them. In those cases, it can be preferable to accept
 the before-mentioned drawbacks instead of building badly designed default
 constructors.<br>
`complex z2(z1);`, (C++11, non-narrowing) `complex z3{z1};` - If the user does
 not write a copy constructor, the compiler will generate one in the standard
 way: calling the copy constructors of all members (and base classes) in the
 order of their definition. In general, it is not advisable to use a mutable
 reference as an argument. Then one can copy only mutable objects. The arguments
 of the copy constructor must not be passed by value. (To pass an argument by
 value, we need the copy constructor which we are about to define. Thus, we
 create a self-dependency that might lead compilers into an infinite loop.)
 These are cases where the default copy constructor does not work, especially
 when the class contains pointers. `unique_ptr` sounds like a better choice than
 the raw pointer. Not only would the memory be released automatically, the
 compiler could not generate the copy constructor automatically because the copy
 constructor is deleted in `unique_ptr`.<br>
`complex c1= 3.0;` - The implicit convresion kicks in when one type is needed
 and another one is given. The implicit conversion can be disabled by declaring
 the constructor as `explicit`. In C++03, the `explicit` attribute only mattered
 for single-argument constructors. From C++11 on, `explicit` is also relevant
 for constructors with multiple arguments due to uniform initialization
 (`{}`).<br>
`complex(double r) : complex{r, 0.0} {}` - C++11 offers *Delegating
 Constructors*; these are constructors that call other constructors. Another new
 feature in C++ is default values for member variables. Then we only need to set
 values in the constructor that are different from the defaults.<br>
The operator that assigns values of the object's type is called *Copy
 Assignment* and can be synthesized by the compiler. What happens if we assign a
 `double` to a `complex`? Once again, we have an implicit conversion: the
 implicit constructor creates a `complex` on the fly and assigns this one. It is
 advised that copy assignment and constructor be consistent to avoid utterly
 confused users.

```c++
vector& operator=(const vector& src)
{
     if (this == &src)
         return *this;
     assert(my_size == src.my_size);
     for (int i= 0; i < my_size; ++i)
         data[i]= src.data[i];
     return *this;
}
```

`vector v{1.0, 2.0, 3.0};`, `v= {1.0, 2.0, 3.0};`,
 `vector x= lu_solve(A, vector{1.0, 2.0, 3.0});` - C++11 introduces the
 *Initializer Lists* as a new feature--not to be confused with "member
 initialization list". To use it. we must include the header
 `<initializer_list>`. We need a constructor and an assignment accepting
 `initializer_list<double>` as an argument.<br>
The free functions `begin` and `end` (`std::begin(values)`, inline) were
 introduced in C++11. In C++03, we have to use the corresponding member
 functions, e.g., `values.begin()`.<br>
Braces `{}` are used in C++11 as universal notation for all forms of variable
 initialization. Direct member setting is only allowed for arrays and classes if
 all (non-static) variables are `public` and the class has no user-defined
 constructor. Such types are called *Aggregates* and setting their values with
 braced lists accordingly *Aggregate Initialization*. Using a list as an
 argument of uniform intialization would actually require double braces
 (`vector v1= {{1.0, 2.0, 3.0}};`). To simplify our life, C++11 provides *Brace
 Elision* is a uniform intializer. Brace elision is a blessing and a curse.<br>
We introduce another feature from C++11 for it: *Move Semantics*. The idea is
 that variables (in other words all named items) are copied deeply and
 temporaries (objects that cannot be referred to by names) transfer their data.
 The compiler tell the difference between temporay and persistent data for us.
 In the C++ lingo, the temporaries are called *Rvalues* because they can only
 appear on the right side in an assignment. C++11 introduces rvalue references
 that are denoted by two ampersands `&&`. Values with a name, so-called lvalues,
 cannot be passed to rvalue references. By providing a move constructor and a
 move assignment, we can assure that rvalues are not expensively copied. The
 move constructor steals the data from its source and leaves it in an empty
 state. Note that an rvalue reference like `vector&& v` is not an rvalue itself
 but an lvalue as it possesses a name. If we wanted to pass our `v` to another
 method that helps the move constructor with the data robbery, we would have to
 turn it into an rvalue again with the standard function `std::move`. (Actually
 this function does not move, it only casts an lvalue to an rvalue. Another take
 on this is that objects are considered expired after `std::move`. Phrased
 differently, they are not dead yet but retired.) The move assignment can be
 implemented in a simple manner by swapping (`std::swap``) the pointers to the
 data.<br>
If we add logging in these two functions, we might realize that our move
 constructor is not called as often as we thought. The reason for this is that
 modern compilers provide an even better optimization than stealing the data.
 This optimization is called *Copy Elision* where compiler omits a copy of the
 data and modifies the generation of the data such that it is immediately stored
 to the target address of the copy operation. Its most important use case is
 *Return Value Optimization (RVO)*, especially when a new variable is
 initialized with a function result. Copy elision was already available in many
 compilers before move semantics.

**Never** throw an exception in a destructor! It is likely that your program
 will crash and the exception will never be caught. In C++11 or higher, it is
 always treated as a run-time error which aborts the execution (destructors are
 implicitly declared `noexcept`). In C++-3, what happens depends on the compiler
 implementation, but a program abortion is the most likely reaction.<br>
If a class contains a `virtual` function, the destructor should be `virtual`,
 too.<br>
Note that `delete` already tests whether a pointer is `nullptr`.

```c++
struct result_set_deleter
{
    result_set_deleter(shared_ptr<Connection> conn,
                       Statement* stmt)
      : conn(conn), stmt(stmt) {}
    void operator()( ResultSet *rs )
    {
        stmt->closeResultSet(rs);
        conn->terminateStatement(stmt);
    }
    shared_ptr<Connection> conn;
    Statement*             stmt;
};

class db_manager
{
  public:
    using ResultSetSharedPtr= std::shared_ptr<ResultSet>;

    db_manager(string const& dbConnection, string const& dbUser,
               string const& dbPw)
      : environment(Environment::createEnvironment(),
                    environment_deleter{}),
        connection(environment->createConnection(dbUser, dbPw,
                                                 dbConnection),
                   connection_deleter{environment} )
    {}
    ResultSetSharedPtr query(const std::string& q) const
    {
        Statement *stmt= connection->createStatement(q);
        ResultSet *rs= stmt->executeQuery();
        auto deleter= result_set_deleter{connection, stmt};
        return ResultSetSharedPtr{rs, deleter};
    }
  private:
    shared_ptr<Environment> environment;
    shared_ptr<Connection>  connection;
};
```

Accessing the `real` part should also work when the `complex` number is
 constant. Thus, we further need a constant version of this function, regarding
 argument and result.<br>
Constant member functions can be called with non-constant objects as arguments
 (because C++ implicitly converts non-constant references into constant
 references when necessary). Therefore, it is often sufficient to provide only
 the constant member function.<br>
Data members can be declared `mutable`. Then they can even be changed in `const`
 methods. This is intended for internal states--like caches--that do not affect
 the observable behavior.<br>
We can *Ref-Qualify* the vector's assignment operators to disable them for
 temporary objects. Two ampersands allow us to restrict a member function to
 rvalues; that is, the method should only be callable on temporaries
 (`something_good donate_my_data() && { ... }`).

The only predefined aspect of operators is their *Arity*: the number of
 arguments and the relative priority of the operators. The only operator
 allowing for an arbitrary arity is the application operator: `operator()`.
 Another freedom that the language provides us is the choice of the arguments'
 types. We are also free to choose the return type of each operator
 arbitrarily.<br>
The following operators--any kind of assignment, `operator[]`, `operator->`, and
 `operator()`--must be non-static methods to ensure that their first argument is
 an lvalue. In contrast, binary operators with an intrinsic type as first
 argument can only be defined as free functions. Implement binary operators as
 free functions.

[hourglass_interfaces_for_cpp_apis]: https://de.slideshare.net/StefanusDuToit/cpp-con-2014-hourglass-interfaces-for-c-apis

## Chapter 3. Generic Programming

Generic programming is a programming paradigm aiming for maximal applicability
 while providing correctness. Its main tools are templates. Mathematically it is
 founded on *Formal Concept Analysis*.

Rvalue references with a type parameter of the form `T&&` accept lvalues as well
 (by reference collapsing). For this reason, Scott Meyers coined the term
 *Universal Reference* for them. Here we stick with the standard term *Forward
 Reference*. When we pass such a reference parameter to another function, we
 want the lvalue reference to be passed as an lvalue and the rvalue reference as
 an rvalue. However, the references themselves are lvalues in both cases (since
 they have names). Here, we need a conditional cast. This is achieved by
 `std::forward`. `forward` must be instantiated with the (unqualified) type
 parameter. Like `move`, `forward` is a pure cast and does not generate a single
 machine operation. People have phrased this as: `move` does not move and
 `forward` does not forward. They rather cast their arguments to be moved or
 forwarded.<br>
The double colons in front of `max` avoid ambiguities wit the standard library's
 `max` which some compilers may include implicitly (e.g., `g++`).<br>
C++ converts arguments implicitly when no exact match exists, but not for
 template arguments. The template mechanism is supposed to provide enough
 flexibility on the type level. In addition, combining template instantiation
 with implicit conversion has such a high potential for ambiguities.<br>
Java in contrast compiles templates only once and executes them for different
 types by casting them to the corresponding types.<br>
C++11 introduced lambdas with automatic `return` types whereas the `return` type
 of functions is still mandatory. In C++14, we can let the compiler deduce the
 `return` type (`auto`).

This `using` declaration works in functions and namespaces but not in classes
 (where it would collide with other `using` declarations). Similarly, we can
 import an entire namespace (`using namespace`). We can rename them with a
 *Namespace Alias*:
 `namespace nested= long_namespace_name::yet_another_name::nested;`<br>
*Argument-Dependent Lookup*, or ADL, expands the search of function names to the
 namespaces of their arguments--but not to their respective parent namespaces.
 This saves us from verbose namespace qualification for functions.<br>
With `using std::swap`, both `swap` overloads are candidates but the one in our
 class is prioritized by overload resolution as its argument type is more
 specific than that of the standard implementation. In fact, `std::swap` is
 already overloaded for standard containers for the same reason. Do not qualify
 namespaces of function templates for which user-type overloads might exist.

The template defaults can refer to preceding parameters. The default can even be
 expressions of preceding parameters.<br>
A funny detail of the post-increment definition is the fake `int` parameter that
 is only present for distinction from the pre-increment definition.<br>
Now, the type and the size are deduced. This in turn means that if we sum over
 two arrays of the same type and different size, the function will be
 instantiated twice.

```c++
template <typename T, unsigned N> // non-type templates
T sum(const T (&array)[N])
{
    T sum(0);
    for (int i= 0; i < N; ++i)
        sum+= array[i];
    return sum;
}
```

The type deduction with `auto` variables works exactly like the deduction of
 function parameters.<br>
The other new feature in C++11 is `decltype`. It is like a function that returns
 the type of an expression. This code snippet also introduces another new
 feature: *Trailing Return Type*. An interesting aspect of `decltype` is that it
 only operates on the type level and does not evaluate the expression given as
 an argument. The two features `auto` and `decltype` differ not only in their
 application; the type deduction is also different. While `auto` follows the
 rules of function template parameters and often drops reference and `const`
 qualifiers, `decltype` takes the expression type as it is. With (C++14)
 `decltype(auto)`, we can declare `auto` variables that have the same type as
 with `decltype`.

```c++
template <typename Vector1, typename Vector2>
auto operator+(const Vector1& v1, const Vector2& v2)
  -> vector< decltype(v1[0] + v2[0]) >;
```

For writing new software without the need of compiling with pre-11 compilers, we
 highly recommend you use `using` instead of `typedef`.
 `using float_fun= float (float, int);`

Such a set of type requirements is called a *Concept*. A concept `CR` that
 contains all requirements of concept `C` and possibly additional requirements
 is called a *Refinement* of `C`. A type `t` that holds all requirements of
 concept `C` is called a *Model* of `C`. Future C++ Standrds will most likely
 support concepts as a central language feature. A technical specification
 mainly by Andrew Sutton, "C++ Extensions for Concepts," is in progress and may
 be part of C++17.<br>
[Standard Template Library Programmer's Guide][stl_programmers_guide]

It is a great advantage that we can use the same implementation for many
 arguments types. For some argument types we may, however, know a more efficient
 implementation, and this can be realized in C++ with *Template
 Specialization*.<br>
[Why Not Specialize Function Templates?][why_not_specialie_function_templates]<br>
When we implement template classes, we will sooner or later run into situations
 where we wan to specialize a template class for another template class. The
 solution that aoids the implementation redundancy and the ignorance of new
 types is *Partial Specialization*.<br>
If you use a compiler without C++11 support, your compiler may interpret two
 subsequent `>` as shift operator `>>`.

Values (integer numbers, `bool` and pointers) can be template arguments as well.

C++11 introduced lambda expressions. A λ-expression is simply shorthand for a
 functor.<br>
`auto sc_l= [](doube x){ return sin(x) + cos(x); };`<br>
`[](double x)->double { return sin(x) + cos(x); };`<br>
Captured parameters are copied, but in constrast to function parameters passed
 by value it is forbidden to modify them. As a consequence, modifying the
 captured variables later has no effect on the lambda.<br>
`auto l_mut= [phi](double x) mutable {phi+= 0.6; return phi; };`<br>
`auto pxr = [&phi,&xi](double x){ return sin(phi * x) + cos(x) * xi; };`
 (captured by reference, the values of `phi` and `xi` at the time of the
 function call are used--not those when the lambda was created)<br>
`[=]` (capture all by copy), `[&]` (capture all by reference), `[=,&a,&b,&c]`,
 `[&,a,b,c]` - Scott Meyers advises not using the capture-all feature as it
 increases the danger of stale references and of ignoring static or member
 variables.<br>
The generalization of capturing is brought in by *Init Capture* (C++14). It
 allows us to move variables into a closure and to give new names to context
 variables or expressions thereof (`var= expr`).<br>
Lambdas in C++11 determined the `return` type but the arguments needed to be
 declared explicitly. This restriction was lifted in C++14.

Variadic templates (C++11) are handled by recursion. We break down the so-called
 *Parameter Pack* and deal with subsets thereof. Use cases are, for instance,
 type-safe `printf` implementations, different kinds of reduction, and all forms
 of generic forwarding functions. `typename ...P`, `<P...>`, `P ...p`,
 `sum(p...)`, `sizeof...(P)`

[stl_programmers_guide]: http://www.sgi.com/tech/stl/
[why_not_specialie_function_templates]: http://www.gotw.ca/publications/mill17.htm

## Chapter 4. Libraries

`begin()` and `end()` return `const_iterator` for constant list. C++11
 introduced the member functions `cbegin` and `cend` returning `const_iterator`
 for both constant and mutable containers. The corresponding free functions were
 introduced in C++14.<br>
More generally, we can apply this `for`-loop to all types with `begin` and `end`
 functions returning iterators. This broader concept including all containers is
 called *Range*. Ranges are gaining importance in the evolution of C++.<br>
The `<iterator>` library provides two basic operations: `advance(it, n)` and
 `distance(it1, it2)`.<br>
Modern compilers are smart enough to realize that the third argument in each
 overload of `advance_aux` is unused and that the tag types are empty classes.
 As a consequence, the argument passing and construction of the tag type objects
 are optimized out. Thus, the extra function layer and the tag dispatching don't
 cause any run-time overhead.<br>
With `push_back`, we have to construct an object first and then it is copied or
 moved to a new entry in the `vector`. In contrast, `emplace_back` (C++11)
 constructs a new object directly in the `vector`'s new entry. If the size of a
 vector is known at compile time and not changed later, we can take the C++11
 container `array` instead.<br>

[]: https://www.codeproject.com/Articles/5425/An-In-Depth-Study-of-the-STL-Deque-Container

## Chapter 5. Meta-Programming

## Chapter 6. Object-Oriented Programming

## Chapter 7. Scientific Projects

## Appendix A. Clumsy Stuff

## Appendix B. Programming Tools

## Appendix C. Language Definitions

