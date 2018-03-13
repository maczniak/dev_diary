# [C++ Super-FAQ][homepage]

[homepage]: https://isocpp.org/wiki/faq

## General Topics

### Const Correctness

The tailing `const` on `inspect()` member function should be used to mean the
 method won't change the object's *abstract* (client-visible) state. This is
 slightly different from saying the method won't change the "raw bits" of the
 object's `struct`. C++ compilers aren't allowed to take the "bitwise"
 interpretation unless they can solve the aliasing problem, which normally can't
 be solved (i.e., a non-`const` alias could exist which could modify the state
 of the object). Another (important) insight from this aliasing issue: pointing
 at an object with a pointer-to-`const` doesn't guarantee that the object won't
 change; it meerely promises that the object won't change *via that
 pointer*.<br>
A `const` member function must not change (or allow a caller to change) the
 `this` object's *logical* state (AKA *abstract* state AKA *meaningwise* state).
 Think of what an object *means*, not how it is internally implemented. The
 `const`ness of a method should makes sense from outside the object.<br>
The most common use of `const` overloading is with the subscript operator.
 Subscript operators often come in pairs. You can, of course, also use
 `const`-overloading for things other than the subscript operator (such as
 funcall-operators).<br>
The C++ compiler language uses the `mutable` keyword to help you embrace this
 *logical* `const` notion. In this case, you would mark the cache with the
 `mutable` keyword, that way the compiler knows it is allowed to change inside a
 `const` method or via any other `const` pointer or reference. The other
 approach, not preferred, is to cast away the `const`ness of the `this` pointer,
 probably via the `const_cast` keyword:
 `Set* self = const_cast<Set*>(this);`<br>
C++ allows the (safe) conversion `Foo*` → `Foo const*`, but gies an error if you
 try to implicitly convert `Foo**` → `const Foo**`. The reason the conversion
 from `Foo**` → `const Foo**` is dangerous is that it would let you silently and
 accidentally modify a `const Foo` object without a cast. There is a conceptual
 similarity between this and the prohibition against converting `Derived**` to
 `Base**`. Here is the most common solution: simply change `const Foo**` to
 `const Foo* const*`.

