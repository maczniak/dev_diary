# [TypeScript Handlbook][homepage]

TypeScript standard library? browser compatibility?

[homepage]: http://www.typescriptlang.org/docs/handbook/basic-types.html

## Basic Types

(tuple) When accessing an element outside the set of known indices, a union type
 is used instead.<br>
(`any`) Variables of type `Object` only allow you to assign anyvalue to them -
 you can't call arbitrary methods on them, even ones that actually exist.<br>
By default `null` and `undefined` are subtypes of all other types. However, when
 using the `--strictNullChecks` flag, `null` and `undefined` are only assignable
 to `void` and their respective types.<br>
The `never` type represents the type of values that never occur. For instance,
 `never` is the return type for a function expression or an arrow function
 expression that always throws an exception or one that never returns; Variables
 also acquire the type `never` when narrowed by any type guards that can never
 be true. The `never` type is a subtype of, and assignable to, every type;
 however, *no* type is a subtype of, or assignable to, `never` (except `never`
 itself). Even `any` isn't assignable to `never`.<br>
A type assertion is like a type cast in other languages, but performs no special
 checking or restructuring of data. It has no runtime impact, and is used purely
 by the compiler. Type assertions have two forms. One is the "angle-bracket"
 syntax. And the other is the `as`-syntax. When using TypeScript with JSX, only
 `as`-style assertions are allowed.<br>
Many common problems in JavaScript are alleviated by using `let`, so you should
 use it instead of `var` whenever possible.

## Variable Declarations

`let` and `const` are two relatively new types of variable declarations in
 JavaScript. As we mentioned earlier, `let` is similar to `var` in some
 respects, but allows users to avoid some of the common "gotchas" that users run
 into in JavaScript.<br>
`var` declarations are accessible anywhere within their containing function,
 module, namespace, or global scope regardless of the containing block. Some
 people call this *`var`-scoping* or *function-scoping*. Parameters are also
 function-scoped. These scoping rules can cause several types of mistakes. One
 problem they exacerbate is the fact that it is not an error to delcare the same
 variable multiple times.<br>
A common work around is to use an IIFE (an Immediately Invoked Function
 Expression) to capture `i` at each iteration.<br>
When a variable is declared using `let`, it uses what some call
 *lexical-scoping* or *block-scoping*. While these variables are "present"
 throughout their scope, all points up until their declaration are part of their
 [*temporal dead zone*][temporal_dead_zone_and_errors_with_let]. This is just a
 sophisticated way of saying you can't access them before the `let` statement.
 Something to note is that you can still *capture* a block-scoped variable
 before it's declared. The only catch is that it's illegal to call that function
 before the declaration. If targeting ES2015, a modern runtime will throw an
 error; however, right now TypeScript is permissive and won't report this as an
 error.<br>
With `var` declarations, we mentioned that it didn't matter how many times you
 declared your variables; you just got one.<br>
`let` declarations have drastically different behavior when declared as part of
 a loop. Rather than just introducing a new environment to the loop itself,
 these declarations sort of create a new scope *per iteration*.<br>
`const` have the same scoping rules as `let`, but you can't re-assign to them.
 Thie should not be confused with the idea that the values they refer to are
 *immutable*.

Another ECMAScript 2015 feature that TypeScript has is
 [destructuring][destructuring_assignment].

```typescript
let [first, ...rest] = [1, 2, 3, 4];
// Since this is JavaScript, you can just ignore trailing elements you don't care about.
let [first] = [1, 2, 3, 4];
let [, second, , fourth] = [1, 2, 3, 4];

// Notice that we had to surround this statement with parenthese. JavaScript normally parses a { as the start of block.
({ a, b } = { a: "baz", b: 101 });
let { a, ...passthrough } = o;
let { a: newName1, b: newName2 } = o; // property renaming
                                      // let newName1 = o.a;
                                      // let newName2 = o.b;
// Confusingly, the colon here does not indicate the type.
// The type, if you specify it,  still need to be written after the entire destructuring.
let { a, b }: { a: string, b: number } = o;
function keepWholeObject(wholeObject: { a: string, b?: number }) {
    let { a, b = 1001 } = wholeObject; // default values
}
type C = { a: string, b?: number }
function f({ a, b }: C): void {
    // ...
}
function f({ a, b } = { a: "", b: 0 }): void {
    // You need to remember to put the pattern before the default value.
}
function f({ a, b = 0 } = { a: "" }): void {
    // f() is ok, but f({}) is error because 'a' is required if you supply an argument.
}

let bothPlus = [0, ...first, ...second, 5]; // spread
```

The spread operator is the opposite of destructuring. Spreading creates a
 shallow copy of `first` and `second`.<br>
Object spreading is more complex than array spreading. Like array spreading, it
 proceeds from left-to-right, but the result is still an object. This means that
 properties that come later in the spread object overwrite properties that come
 earlier. Object spread also has a couple of other surprising limits. First, it
 only includes an objects' own, enumerable properties. Basically, that means you
 lose methods when you spread instances of an object. Second, the Typescript
 compiler doesn't allow spreads of type parameters from generic functions. That
 feature is expected in future versions of the language.

[temporal_dead_zone_and_errors_with_let]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let#Temporal_Dead_Zone_and_errors_with_let
[destructuring_assignment]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment

## Interfaces

One of TypeScript's core principles is that type-checking focuses on the *shape*
 that values have. This is sometimes called "duck typing" or "structural
 subtyping". In TypeScript, interfaces fill the role of naming these types, and
 are a powerful way of defining contracts.<br>
TypeScript comes with a `ReadonlyArray<T>` type that is the same as `Array<T>`
 with all mutating methods removed.<br>
Variables use `const` whereas properties use `readonly`.<br>
Object literals get special treatment and undergo *excess property checking*
 when assigning them to other variables, or passing them as arguments. If an
 object literal has any properties that the "target type" doesn't have, you'll
 get an error. Getting around these checks is actually really simple. The
 easiest method is to just use a type assertion. However, a better approach
 might be to add a string index signature if you're sure that the object can
 have some extra properties that are used in some special way.

```typescript
let mySquare = createSquare({ width: 100, opacity: 0.5 } as SquareConfig);

interface SquareConfig {
    color?: string;
    width?: number;
    [propName: string]: any;
}
```

To describe a function type with an interface, we give the interface a call
 signature. This is like a function declaration with only the parameter list and
 return type given. For function types to correctly type-check, the names of the
 parameters do not need to match.<br>
Indexable types have an *index signature* that describes the types we can use to
 index into the object, along with the corresponding return types when indexing.
 These are two types of supported index signatures: string and number. It is
 possible to support both types of indexers, but the type returned from a
 numeric indexer must be a subtype of the type returned from the string indexer.
 This is because when indexing with a `number`, JavaScript will actually convert
 that to a `string` before indexing into an object. While string index
 signatures are a powerful way to describe the "dictionary" pattern, they also
 enforce that all properties match their return type. This is because a string
 index declares that `obj.property` is also available as `obj["property"]`.

```typescript
interface SearchFunc {
    (source: string, subString: string): boolean;
}

interface StringArray {
    [index: number]: string;
}
```

Interfaces describe the public side of the class, rather than both the public
 and private side.<br>
When a class implements an interface, only the instance side of the class is
 checked. Since the constructor sits in the static side, it is not included in
 this check. Instead, you would need to work with the static side of the class
 directly. In this example, we define two interfaces, `ClockConstructor` for the
 constructor and `ClockInterface` for the instance methods. Then for convenience
 we define a constructor function `createClock` that creates instances of the
 type that is passed to it.

```typescript
interface ClockConstructor {
    new (hour: number, minute: number): ClockInterface;
}
interface ClockInterface {
    tick();
}
function createClock(ctor: ClockConstructor, hour: number, minute: number): ClockInterface {
    return new ctor(hour, minute);
}
class DigitalClock implements ClockInterface {
    constructor(h: number, m: number) { }
    tick() { console.log("beep beep"); }
}
let digital = createClock(DigitalClock, 12, 17);

interface Counter {
    (start: number): string;
    interval: number;
    reset(): void;
}
function getCounter(): Counter {
    let counter = <Counter>function (start: number) { };
    counter.interval = 123;
    counter.reset = function () { };
    return counter;
}
```

When an interface type extends a class type it inherits the members of the class
 but not their implementations. It is as if the interface had declared all of
 the members of the class without providing an implementation. Interfaces
 inherit even the private and protected members of a base class. This means that
 when you create an interface that extends a class with private or protected
 members, that interface type can only be implemented by that class or a
 subclass of it. Only descendants of `Control` will have a `state` private
 member that originates in the same declaration, which is a requirement for
 private members to be compatible.

## Classes

Starting with ECMAScript 2015, also known as ECMAScript 6, JavaScript
 programmers will be able to build their applications using this object-oriented
 class-based approach. In TypeScript, we allow developers to use these
 techniques now, and compile them down to JavaScript that works across all major
 browsers and platforms, without having to wait for the next version of
 JavaScript.<br>
In TypeScript, each member is `public` by default.<br>
However, when comparing types that have `private` and `protected` members, we
 treat these types differently. For two types to be considered compatible, if
 one of them has a `private` member, then the other must have a `private` member
 that originated in the same declaration.<br>
*parameter properies* let you create and initialize a member in one place
 (constructor). Parameter properties are declared by prefixing a constructor
 parameter with an accessibility modifier or `readonly`, or both.<br>
A couple of things to note about accessors: First, accessors require you to set
 the compiler to output ECMAScript 5 or higher. Downlevelling to ECMAScript 3 is
 not supported. Second, accessors with a `get` and no `set` are automatically
 inferred to be `readonly`. This is helpful when generating a `.d.ts` file from
 your code, because users of your property can see that they can't change
 it.<br>
Unlike an interface, an `abstract` class may contain implementation details for
 its members. Abstract methods share a similar syntax to interface methods.
 However, abstract methods must include the `abstract` keyword and may
 optionally include access modifiers.<br>
We're also creating another value that we call the *constructor function*. This
 is the function that is called when we `new` up instances of the class.<br>
Here we use `typeof Greeter`, that is "give me the type of the `Greeter` class
 itself" rather than the instance type. Or, more precisely, "give me the type of
 the symbol called `Greeter`," which is the type of the constructor function.

```typescript
let greeterMaker: typeof Greeter = Greeter;
greeterMaker.standardGreeting = "Hey there!";
let greeter2: Greeter = new greeterMaker();
```

## Functions

```typescript
let myAdd: (x: number, y: number) => number = // function type
    function(x: number, y: number): number { return x + y; };
```

The return type is a required part of the function type, so if the function
 doesn't return a value, you would use `void` instead of leaving it off.<br>
In JavaScript, every parameter is optional, and users may leave them off as they
 see fit. When they do, their value is `undefined`. We can get this
 functionality in TypeScript by adding a `?` to the end of parameters we want to
 be optional.<br>
In TypeScript, we can also set a value that a parameter will be assigned if the
 user does not provide one, or if the user passes `undefined` in its place.
 Unlike plain optional parameters, default-initialized parameters doesn't *need*
 to occur after required parameters.<br>
`function buildName(firstName: string, lastName = "Smith") {` share the same
 type `(firstName: string, lastName?: string) => string`. The default value of
 `lastName` disappears in the type, only leaving behind the fact that the
 parameter is optional.<br>
In JavaScript, you can work with the arguments directly using the `arguments`
 variable that is visible inside every function body. In TypeScript, you can
 gather these arguments together into a variable (`...restOfName: string[]`,
 rest parameters).<br>
[Understanding JavaScript Function Invocation and "this"][understanding_javascript_function_invocation_and_this]<br>
A top-level non-method syntax call like this will use `window` for `this`.
 (Note: under strict mode, `this` will be `undefined` rather than `window`). To
 fix this, we change the function expression to use the ECMAScript 6 arrow
 syntax. Arrow functions capture the `this` where the function is created rather
 than where it is invoked. Even better, TypeScript will warn you when you make
 this mistake if you pass the `--noImplicitThis` flag to the compiler. It will
 point out that `this` is of type `any`.<br>
Unfortunately, the type of `this.suits[pickedSuit]` is still `any`. That's
 because `this` comes from the function expression inside the object literal. To
 fix this, you can provide an explicit `this` parameter. `this: void` parameter
 are fake parameters that com first in the parameter list of a function.<br>
Because the library that calls your callback will call it like a normal
 function, `this` will be `undefined`. First, the library author needs to
 annotate the callback type with `this`. `this: void` means that
 `addClickListener` expects `onclick` to be a function that does not require a
 `this` type. Because `onClickGood` specifies its `this` type as `void`, it is
 legal to pass to `addClickListener`. Of course, this also means that it can't
 use `this.info`. If you want both then you'll have to use an arrow function.
 This works because arrow functions don't capture `this`, so you can always pass
 them to something that expect `this: void`. The downside is that one arrow
 function is created per object of type Handler. Methods, on the other hand, are
 only created once and attached to Handler's prototype.<br>
The answer is to supply multiple function types for the same function as a list
 of overloads. This list is what the compiler will use to resolve function
 calls. In order for the compiler to pick the correct typecheck, it follows a
 similar process to the underlying JavaScript. It looks at the overload list,
 and proceeding with the first overload attempts to call the function with the
 provided prarameters. Note that the `function pickCard(x): any` piece is not
 part of the overload list. Calling `pickCard` with any other parameter types
 would cause an error.

[understanding_javascript_function_invocation_and_this]: http://yehudakatz.com/2011/08/11/understanding-javascript-function-invocation-and-this/

## Generics

When we use `GenericIdentityFn`, we now will also need to specify the
 corresponding type argument (`number`), effectively locking in what the
 underlying call signature will use. Note that it is not possible to create
 generic enums and namespaces.<br>
Generic classes are only generic over their instance side rather than their
 static side, so when working with classes, static members can not use the
 class's type parameter.

```typescript
function loggingIdentity<T extends Lengthwise>(arg: T): T { ...

function getProperty<T, K extends keyof T>(obj: T, key: K) { ...

function create<T>(c: {new(): T; }): T { ...
function createInstance<A extends Animal>(c: new () => A): A { ...
```

## Enums

TypeScript provides both numeric and string-based enums.<br>
Numeric enums can be mixed in computed (e.g., `G = "123".length`) and constant
 members. The short story is, enums without initializers either need to be
 first, or have to come after numeric enums initialized with numeric constants
 or other constant enum members.<br>
Technically enums can be mixed with string and numeric members, but it's not
 clear why you would ever want to do so.<br>
When all members in an enum have literal enum values, some special semantics
 come to play. The first is that enum members also become types as well! The
 other change is that enum types themselves effectively become a *union* of each
 enum member.<br>
Enums are real objects that exist at runtime.<br>
Numeric enums members also get a *reverse mapping* from enum values to enum
 names.<br>
To avoid paying the cost of extra generated code and additional indirecion when
 accessing enum values, it's possible to use `const` enums. Const enums can only
 use constant enum expressions and unlike regular enums they are completely
 removed during compilation. Const enum members are inlined at use sites. This
 is possible since const enums cannot have computed members.

## Type Inference

Because the best common type has to be chosen from the provided candidate types,
 there are some cases where types share a common structure, but no one type is
 the super type of all candidate types. To correct this, instead explicitly
 provide the type when no one type is a super type of all other candidates. When
 no best common type is found, the resulting inference is the union array type,
 `(Rhino | Elephant | Snake)[]`.<br>
Contextual typing occurs when the type of an expression is implied by its
 location. The function expression with an explicit type annotation on the
 parameter will override the contextual type.<br>
Ambient enums (`declare`) are used to describe the shape of already existing
 enum types (i.e., *have no effect on the output of your code*). An ambient (and
 non-const) enum member that does not have initializer is *always* considered
 computed.

## Type Compatibility

Structural subtyping is in contrast with nominal typing. TypeScript's structural
 type system was designed based on how JavaScript code is typically written.
 JavaScript widely uses anonymous objects like function expressions and object
 literals.<br>
TypeScript's type system allows certain operations that can't be known at
 compile-time to be safe. When a type system has this property, it is said to
 not be "sound".

In `y = x`, every parameter of `x` has a corresponding compatible parameter in
 `y`, so the assignment is allowed.<br>
The type system enforces that the source function's return type be a subtype of
 the target type's return type.<br>
(function parameter bivariance) When comparing the types of function parameters,
 assignment succeeds if either the source parameter is assignable to the target
 parameter, or vice versa. This is unsound because a caller might end up being
 given a function that takes a more specialized type, but invokes the function
 with a less specialized type. In practice, this sort of error is rare, and
 allowing this enables many common JavaScript patterns.<br>
When comparing functions for compatibility, optional and required parameters are
 interchangeable. Extra optional parameters of the source type are not an error,
 and optional parameters of the target type without corresponding parameters in
 the source type are not an error. This is unsound from a type system
 perspective, but from a runtime point of view the idea of an optional parameter
 is generally not well-enforced since passing `undefined` in that position is
 equivalent for most functions.<br>
When a function has overloads, each overload in the source type must be matched
 by a compatible signature on the target type. This ensures that the target
 function can be called in all the same situations as the source function.<br>
Enums are compatible with numbers, and numbers are compatible with enums. Enum
 values from different enum types are considered incompatible.<br>
When comparing two objects of a class type, only members of the instance are
 compared. Static members and constructors do not affect compatibility.<br>
(Generics) Because TypeScript is a structural typesystem, type parameters only
 affect the resulting type when consumed as part of the type of a member. For
 generic types that do not have their type arguments specified, compatiblity is
 checked by specifying `any` in place of all unspecified type arguments.<br>
So far, we've used 'compatible', which is not a term defined in the
 [language spec][typescript_spec]. In TypeScript, there are two kinds of
 compatibility: subtype and assignment. These differ only in that assignment
 extends subtype compatibility with rules to allow assignment to and from `any`
 and to and from enum with corresponding numeric values. Different places in the
 language use one of the two compatibility mechanisms, depending on the
 situation. For practical purposes, type compatibility is dictated by assignment
 compatibility even in the cases of the `implements` and `extends` clauses.

[typescript_spec]: https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md

## Advanced Types

```typescript
function extend<T, U>(first: T, second: U): T & U {
    let result = <T & U>{}; // intersection types
    for (let id in first) {
        (<any>result)[id] = (<any>first)[id];
    }
    for (let id in second) {
        if (!result.hasOwnProperty(id)) {
            (<any>result)[id] = (<any>second)[id];
        }
    }
    return result;
}
```

If we have a value that has a union type, we can only access members that are
 common to all types in the union.<br>
A *type guard* is some expression that performs a runtime check that guarantees
 the type in some scope. To define a type guard, we simply need to define a
 function whose return type is a *type predicate*. A predicate takes the form
 `parameterName is Type`, where `parameterName` must be the name of a parameter
 from the current function signature.<br>
You don't need to abstract `typeof x == "number" into its own function because
 TypeScript will recognize it as a type guard on its own. These *`typeof` type
 guards* are recognized in two different forms: `typeof v === "typename"` and
 `typeof v !== "typename"`, where `"typename"` must be `"number"`, `"string"`,
 `"boolean"`, or `"symbol"`.<br>
*`instanceof` type guards* are a way of narrowing types using their constructor
 function.<br>
The `--strictNullChecks` flag fixes this: when you declare a variable, it
 doesn't automatically include `null` or `undefined`. With `--strictNullChecks`,
 optional parameters and optional properties automatically add
 `| undefined`.<br>
In cases (`name || "Bob"`) where the compiler can't eliminate `null` or
 `undefined`, you can use the type assertion operator `!` to manually remove
 them. `identifier!` removes `null` and `undefined` from the type of
 `identifier`. The example uses a nested function because the compiler can't
 eliminate nulls inside a nested function (except immediately-invoked function
 expressions). That's because it can't track all calls to the nested function.

Aliasing doesn't actually create a new type---it creates a new *name* to refer
 to that type. For instance, error messages won't use the alias name. Type
 aliases cannot be extended or implemented from (nor can they extend/implement
 other types).<br>
`type LinkedList<T> = T & { next: LinkedList<T> };`

String literal types allow you to specify the exact value a string must have. In
 practice string literal types combine nicely with union types, type guards, and
 aliases. String literal types can be used in the same way to distinguish
 overloads.<br>
TypeScript also has numeric literal types. These are seldom written explicitly,
 they can be useful when narrowing can catch bugs.

