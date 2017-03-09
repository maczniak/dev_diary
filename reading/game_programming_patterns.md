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

[Universal Design Pattern][universal_design_patern]

[universal_design_patern]: http://steve-yegge.blogspot.kr/2008/10/universal-design-pattern.html

### 6. Singleton

[Singleton Pattern][singleton_pattern] (WikiWikiWeb)<br>
When much of the industry moved to object-oriented programming from C, one
 problem they ran into was "how do I get an instance?" They had some method they
 wanted to call but didn’t have an instance of the object that provides that
 method in hand. Singletons (in other words, making it global) were an easy way
 out.

[singleton_pattern]: http://wiki.c2.com/?SingletonPattern

### 7. State

In all three, you have a main object that delegates to another subordinate one.
 The difference is *intent*.
* With Strategy, the goal is to *decouple* the main class from some portion of
  its behavior.
* With Type Object, the goal is to make a *number* of objects behave similarly
  by *sharing* a reference to the same type object.
* With State, the goal is for the main object to *change* its behavior by
  *changing* the object it delegates to.

A finite state machine isn’t even *Turing complete*. Automata theory describes
 computation using a series of abstract models, each more complex than the
 previous. A *Turing machine* is one of the most expressive models.<br>
concurrent state machines (n + m states instead of n × m states), hierarchical
 state machines (roll up the chain of superstates),
 [pushdown automata][pushdown_automata] (history with stack)<br>
consider [behavior trees][behavior_trees] and [planning systems][planning_systems] for more complex AI

[pushdown_automata]: https://en.wikipedia.org/wiki/Pushdown_automaton
[behavior_trees]: http://web.archive.org/web/20140402204854/http://www.altdevblogaday.com/2011/02/24/introduction-to-behavior-trees/
[planning_systems]: http://alumni.media.mit.edu/~jorkin/goap.html

## III. Sequencing Patterns

invent time of a virtual world

### 8. Double Buffer

### 9. Game Loop

Decouple the progression of game time from user input and processor speed.<br>
In order to run in real time, game physics engines are approximations of the
 real laws of mechanics. To keep those approximations from blowing up, damping
 is applied. That damping is carefully tuned to a certain time step. Vary that,
 and the physics gets unstable.

```
double previous = getCurrentTime();
double lag = 0.0;
while (true)
{
  double current = getCurrentTime();
  double elapsed = current - previous;
  previous = current;
  lag += elapsed;

  processInput();

  while (lag >= MS_PER_UPDATE)
  {
    update();
    lag -= MS_PER_UPDATE;
  }

  render(lag / MS_PER_UPDATE);
}
```

Fixed update time step, variable rendering - It updates with a fixed time step,
 but it can drop *rendering* frames if it needs to catch up to the player's
 clock.<br>
see also: [Fix Your Timestep][fix_your_timestep],
 [deWiTTERS Game Loop][dewitters_game_loop], Unity3D MonoBehaviour Lifecycle
 ([cached][unity_lifecycle_cached], [comments][unity_lifecycle_comments])

[fix_your_timestep]: http://gafferongames.com/game-physics/fix-your-timestep/
[dewitters_game_loop]: http://www.koonsolo.com/news/dewitters-gameloop/
[unity_lifecycle_cached]: http://whatiseeinit.blogspot.kr/2012/10/unity3d-monobehaviour-lifecycle.html
[unity_lifecycle_comments]: https://disqus.com/home/discussion/richardfine/unity3d_monobehaviour_lifecycle/

### 10. Update Method

This pattern, along with Game Loop and Component, is part of a trinity that
 often forms the nucleus of a game engine.<br>
see also: Unity's [MonoBehaviour][unity_monobehaviour], XNA's [Game][xna_game]
 and [GameComponent][xna_gamecomponent], Quintus's [Sprite][quintus_sprite]

[unity_monobehaviour]: https://docs.unity3d.com/ScriptReference/MonoBehaviour.Update.html
[xna_game]: https://msdn.microsoft.com/en-us/library/microsoft.xna.framework.game.update.aspx
[xna_gamecomponent]: https://msdn.microsoft.com/en-us/library/microsoft.xna.framework.game.update.aspx
[quintus_sprite]: http://www.html5quintus.com/guide/sprites.md

## IV. Behavioral Patterns

### 11. Bytecode

move behaviour out of code entirely and into data<br>
There's a reason most programming languages in wide use aren't based on the
 Interpreter pattern. It's just too slow, and it uses up too much memory. Ruby
 was implemented like this for something like 15 years. At version 1.0, they
 switched to bytecode like this chapter describes.<br>
If you don't have the resources to build an authoring tool, then bytecode isn't
 for you. And you'll miss your debugger.<br>
an instruction in Lua--probably the most well-known register-based VM--is a full
 32-bits. It uses 6 bits for the instruction type, and the rest are arguments.
 Register-based VMs got a reputation for being a bit faster after Lua converted
 to that style, but it depends *deeply* on your actual instructions and on lots
 of other details of your VM.
 [A No-Frills Introduction to Lua 5.1 VM Instructions][intro_to_lua_5_1_VM_inst]<br>
My recommendation is that if you can stick with a single data type, do that.
 Otherwise, do a tagged union. That's what almost every language interpreter in
 the world does.<br>
*You have to define a syntax.* Defining a grammar that makes parsers happy is
 easy. Defining one that makes *users* happy is *hard*. Syntax design is user
 interface design, and that process doesn't get easier when you constrain the
 user interface to a string of characters.<br>
*You have to handle syntax errors.* This is one of the most important and most
 difficult parts of the process.<br>
If you look behind many of the games you love, you'll often find the secret was
 fun authoring tools.<br>
My own little scripting language, [Wren][wren], is a simple stack-based bytecode
 interpreter.

[intro_to_lua_5_1_VM_inst]: http://luaforge.net/docman/83/98/ANoFrillsIntroToLua51VMInstructions.pdf
[wren]: https://github.com/munificent/wren

### 12. Subclass Sandbox

Define behavior in a subclass using a set of operations provided by its base
 class.<br>
Once we have these toys to play with, we need a place to use them. For that,
 we'll define a *sandbox method*, an abstract protected method that subclasses
 must implement.<br>
A **base class** defines an abstract **sambox method** and several **provided
 operations**.<br>
[fragile base class problem][fragile_base_class_problem]<br>
This pattern is a role reversal of the [Template Method][template_method]
 pattern. You can also consider this a variation on the [Facade][facade]
 pattern. That pattern hides a number of different systems behind a single
 simplified API. With Subclass Sandbox, the base class acts as a facade that
 hides the entire game engine from the subclasses.

[fragile_base_class_problem]: https://en.wikipedia.org/wiki/Fragile_base_class
[template_method]: https://en.wikipedia.org/wiki/Template_method_pattern
[facade]: https://en.wikipedia.org/wiki/Facade_pattern

### 13. Type Object

Allow the flexible creation of new "classes" by creating a single class, each
 instances of which represents a different type of object.<br>
Each breed instance is an *object* that represents a different conceptual
 *type*, hence the name of the pattern: Type Object. We've essentially lifted a
 portion of the type system out of the hard-coded class hierarchy into data we
 can define at runtime. We can create hundreds of different breeds by
 instantiating more instances of `Breed` with different values. If we create
 breeds by initializing them from data read from some configuration file, we
 hae the ability to define new types of monsters completely in data.<br>
Define a **type object** class and a **typed object** class. Each type object
 instance represents a different logical type. Each typed object stores a
 **refernce to the type object that describes its type**.<br>
The vtable is our breed object, and the pointer to the vtable is the reference
 the monster holds to its breed. C++ classes are the Type Object pattern applied
 to C, handled automatically by the compiler.

## V. Decoupling Patterns

### 14. Component

Allow a single entity to span multiple domains without coupling the domains to
 each other.<br>
[entity component system][entity_component_system]

[entity_component_system]: https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system

### 15. Event Queue

[event-driven programming][event_driven_programming], like
 [backboard systems][backboard_systems] in the AI field<br>
When you receive an event, you have to be careful not to assume the *current*
 state of the world reflects how the world was *when then event was raised*.
 This means queued events tend to be more data heavy than events in synchronous
 systems.<br>
[fibonacci heap][fibonacci_heap], [skip list][skip_list]<br>
this pattern is the asynchronous cousin to the well-known Observer pattern.

[event_driven_programming]: https://en.wikipedia.org/wiki/Event-driven_programming
[backboard_systems]: https://en.wikipedia.org/wiki/Blackboard_system
[fibonacci_heap]: https://en.wikipedia.org/wiki/Fibonacci_heap
[skip_list]: https://en.wikipedia.org/wiki/Skip_list

### 16. Service Locator

service (interface) + service provider (concrete class) + locator<br>
it functions as a more flexible, more configurable cousin of the Singleton
 pattern. it defers wiring it up until runtime.

## VI. Optimization Patterns

### 17. Data Locality

[Pitfalls of Object Oriented Programming][pitfalls_of_object_oriented_programming] ([pdf][pitfalls_of_object_oriented_programming_pdf]), *[Data-Oriented Design][data_oriented_design]* ([article][data_oriented_design_article])<br>
[Cachegrind][cachegrind]<br>
The [Artemis][artemis] game engine is one of the first and better-known
 frameworks that uses simple IDs for game entities.

[pitfalls_of_object_oriented_programming]: http://seven-degrees-of-freedom.blogspot.kr/2009/12/pitfalls-of-object-oriented-programming.html
[pitfalls_of_object_oriented_programming_pdf]: https://github.com/Michaelangel007/game_dev_pdfs/blob/master/c%2B%2B/Pitfalls_of_Object_Oriented_Programming_GCAP_09.pdf
[data_oriented_design]: http://www.dataorienteddesign.com/dodmain/
[data_oriented_design_article]: http://gamesfromwithin.com/data-oriented-design
[cachegrind]: http://valgrind.org/docs/manual/cg-manual.html
[artemis]: http://gamadu.com/artemis/

### 18. Dirty Flag

[dirty bit][dirty_bit]<br>
Phil Karlton famously said, "There are only two hard things in Computer Science:
 cache invalidation and naming things."<br>
The term in human-computer interaction for an intentional delay between when a
 program receives user input and when it responds is [hysteresis][hysteresis].

[dirty_bit]: https://en.wikipedia.org/wiki/Dirty_bit
[hysteresis]: https://en.wikipedia.org/wiki/Hysteresis

### 19. Object Pool

Most console makers require games to pass "soak tests" where they leave the game
 running in demo mode for several days. If the game crashes, they don't allow it
 to ship. While soak tests sometimes fail because of a rarely occurring bug,
 it's usually creeping fragmentation or memory leakage that brings the game
 down.<br>
[free list][free_list]

[free_list]: https://en.wikipedia.org/wiki/Free_list

### 20. Spatial Partition

[pigeonhole sort][pigeonhole_sort]<br>
One spatial partition deserves special mention because it has some of the best
 characteristics of both fixed partitions (object-independent partitioning) and
 adaptable ones (object-dependent *hierarchy*): quadtrees.

with more dimensions | 1D data structure
---------------------|------------------
[grid][grid] | persistent [bucket sort][bucket_sort]
[BSP][bsp], [k-d tree][k_d_tree], [bounding volume hierarchy][bounding_volume_hierarchy] | [binary search tree][binary_search_tree]
[quadtree][quadtree], octree | [trie][trie]

[pigeonhole_sort]: https://en.wikipedia.org/wiki/Pigeonhole_sort
[grid]: https://en.wikipedia.org/wiki/Grid_(spatial_index)
[bucket_sort]: https://en.wikipedia.org/wiki/Bucket_sort
[bsp]: https://en.wikipedia.org/wiki/Binary_space_partitioning
[k_d_tree]: https://en.wikipedia.org/wiki/K-d_tree
[bounding_volume_hierarchy]: https://en.wikipedia.org/wiki/Bounding_volume_hierarchy
[binary_search_tree]: https://en.wikipedia.org/wiki/Binary_search_tree
[quadtree]: https://en.wikipedia.org/wiki/Quadtree
[trie]: https://en.wikipedia.org/wiki/Trie

