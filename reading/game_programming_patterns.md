# [Game Programming Patterns][homepage] by Robert Nystrom ([blog][author_blog], [twitter][author_twitter]), Genever Benning (2014)

[source code][source_code], [Zero to 95,688: How I wrote Game Programming Patterns][zero_to_95688_how_i_wrote_game_programming_patterns]<br>
[Korean translator's related blog posts][korean_blog_posts]

[homepage]: http://gameprogrammingpatterns.com/
[author_blog]: http://journal.stuffwithstuff.com/
[author_twitter]: https://twitter.com/munificentbob
[source_code]: https://github.com/munificent/game-programming-patterns
[zero_to_95688_how_i_wrote_game_programming_patterns]: http://journal.stuffwithstuff.com/2014/04/22/zero-to-95688-how-i-wrote-game-programming-patterns/
[korean_blog_posts]: http://parkpd.egloos.com/tag/게임프로그래밍패턴

## I. Introduction

[Game Programming Gems][game_programming_gems] series

[game_programming_gems]: http://www.satori.org/game-programming-gems/

### 1. Architecture, Performance, and Games

One trick to ensuring your prototype code isn't obliged to become real code is
 to write it in a language different from the one your game uses.

## II. Design Patterns Revisited

### 2. Command

A command is a *reified (/ first-class) method call*. (reflection is a reified
 type system.)<br>
Commands are an object-oriented replacement for callbacks. -- DP<br>
In some ways, the Command pattern is a way of emulating closures in languages
 that don't have them.

### 3. Flyweight

[instanced rendering][instanced_rendering]<br>
separate out an object's data into two kinds: intrinsic state (context-free) vs
 extrinsic state<br>
Sharing objects to save memory should be an optimization that doesn't affect the
 visible behavior of the app. Because of this, Flyweight objects are almost
 always immutable.

[instanced_rendering]: https://en.wikipedia.org/wiki/Geometry_instancing

### 4. Observer

observer, subject<br>
synchronous (see Event Queue for asynchronous communication)<br>
see Object Pool to avoid fragmentation<br>
"intrusive" linked lists (the data contains the node) are less flexible, but
 more efficient. They are popular in places like the Linux kernel where that
 trade-off makes sense.<br>
what happens when you delete a subject or an observer?<br>
[lapsed listener problem][lapsed_listener_problem] prevents an observer from
 being garbage collected because a subject holds a reference to an observer.<br>
If I were designing an observer system today, I'd make it function-based instead
 of class-based. ([C++ example][generic_type_safe_delegations_and_events_in_cpp])<br>
Many recent application frameworks now use "data binding".

[lapsed_listener_problem]: https://en.wikipedia.org/wiki/Lapsed_listener_problem
[generic_type_safe_delegations_and_events_in_cpp]: https://blog.molecular-matters.com/2011/09/19/generic-type-safe-delegates-and-events-in-c/

### 5. Prototype

### 6. Singleton

### 7. State

## III. Sequencing Patterns

### 8. Double Buffer

### 9. Game Loop

### 10. Update Method

## IV. Behavioral Patterns

### 11. Bytecode

### 12. Subclass Sandbox

### 13. Type Object

## V. Decoupling Patterns

### 14. Component

### 15. Event Queue

### 16. Service Locator

## VI. Optimization Patterns

### 17. Data Locality

### 18. Dirty Flag

### 19. Object Pool

### 20. Spatial Partition

