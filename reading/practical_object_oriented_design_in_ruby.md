# [Practical Object-Oriented Design in Ruby][homepage] by Sandi Metz, Addison-Wesley (2013)

[homepage]: http://www.poodr.com/

Practical Object-Oriented Design in Ruby (POODR)

Ruby itself, life Smalltalk, is a completely object-oriented (OO) language. This
 book guides yu every step of the way, from the most basic questions of what to
 put in a class, through basic concepts such as the Single Responsibility Principle, all the way through to making tradeoffs between inheritance and composition, and figuring out how to test objects in isolation.<br>
This book uses Ruby to teach OOD but you do not need to know Ruby to understand
 the concepts herein. The fact that Ruby is dynamically typed simplifies the
 syntax of the examples and distills the design ideas to their essense, but
 every concept in this book can be directly translated to a statically typed OO
 language.

## 1. Object-Oriented Design

This book is about designing object-oriented software, and it views the world as
 a series of spontaneous interactions between objects. Object-oriented design
 (OOD) requires that you shift from thinking of the world as a collection of
 predefined procedures to modeling the world as a series of messages that pass
 between objects.<br>
An if you will, image one more thing: once written, this application need never
 change. For this case, design does not matter.<br>
Getting the right message to the correct target object requires that the sender
 of the message know things about the receiver. This knowledge creates
 dependencies between the two and these dependencies stand in the way of change.
 Object-oriented design is about *managing dependencies*. It is a set of coding
 techniques that arrange dependencies such that objects can tolerate change. In
 the absence of design, changing one object forces change upon its
 collaborators, which in turn, forces change upon its collaborators, *ad
 infinitum*. Test are caught in the crossfire and begin to feel like a hindrance
 rather than a help.<br>
Every application is a collection of code; the code's arrangement is the
 *design*.<br>
Designs that anticipate specific future requirements almost always end badly. It
 doesn't guess the future; it preserves your opinions for accommodating the
 future. The purpose of design is to allow you to do design *later* and its
 primary goal is to reduce the cost of change.<br>
Just as a sculptor has chisels and files, an object-oriented designer has
 tools--principles and patterns. The SOLID acronym, conined by Michael Feathers
 and popularized by Robert Martin, represents five of the most well known
 principles of object-oriented design: **S**ingle Responsibility,
 **O**pen-Closed, **L**iskov Substitution, **I**nterface Segregation, and
 **D**ependency Inversion. Other principles include Andy Hunt and Dave Thomas's
 DRY (Don't Repeat Yourself) and the Law of Demeter (LoD) from the Demeter
 project at Northeastern University. In addition to principles, object-oriented
 design involves *patterns*.<br>
Agile believes that your customers can't define the software they want before
 seeing it, so it's best to show them sooner rather than later. Agile believes
 that the most cost-effective way to produce what customers really want is to
 collaborate with them, building software one small bit at a time such that each
 delivered bit has the opportunity to alter ideas about the next. If Agile is
 correct, two other things are also true. First, there is absolutely no point in
 doing a Big Up Front Design (BUFD) (because it cannot possibly be correct), and
 second, no one can predict when the application will be done (because you don't
 know in advance what it will eventually do). Agile's acceptance of this truth
 allows it to provide strategies to overcome the handicap of developing software
 while knowing neither the target nor the timeline.<br>
Bad OOD metrics are indisputably a sign of bad design; code that scores poorly
 *will* be hard to change. Unfortunately, good scores don't prove the opposite,
 that is, they don't guarantee that the next change you make will be easy or
 cheap. OOD metrics cannot identify designs that do the wrong thing in the right
 way. The ultimate software metric would be *cost per feature over the time
 interval that matter*, but this is not easy to calculate.<br>
In a procedural language variables have a single data type; knowledge of this
 data type lets you have expectations about which operations are valid. In Ruby
 an object may have many types, one of which will always come from its class.

## 2. Designing Classes with a Single Responsibility

Anyone can arrange code to make it work right now. Creating an easy-to-change
 application, however, is a different matter. This quality of easy changeability
 reveals the craft of programming. Achieving it takes knowledge, skill, and a
 bit of artistic creativity.<br>
The problem is not one of technical knowledge but of organization; you know how
 to write the code but not where to put it.<br>
If your application succeeds many of the decisions you make today will need to
 be changed later. Design is more the art of preserving changeability than it is
 the act of achieving perfection.<br>
Code should be:
* Transparent - The consequences of change should be obvious in the code that is
  changing and in distant code that relies upon it
* Reasonable - The cost of any change should be proportional to the benefits the
  change archieves
* Usable - Existing code should be usable in new and unexpected contexts
* Exemplary - The code itself should encourage those who change it to perpetuate
  these qualities

The first step in creating code that is TRUE is to ensure that each class has a
 single, well-defined responsibility.<br>
Applications that are easy to change consist of classes that are easy to reuse.
 An application that is easy to change is like a box of building blocks; you can
 select just the pieces you need and assemble them in unanticipated ways. A
 class that has more than one resposibility is difficult to reuse.<br>
How can you determine if the `Gear` class contains behavior that belongs
 somewhere else? One way is to pretend that it's sentient and to interrogate it.
 Another way yo hone in on what a class is actually doing is to attempt to
 describe it in one sentence.<br>
The Single Responsibility Principle (SRP) has its roots in Rebecca Wirfs-Brock
 and Brian Wilkerson's idea of Responsibility-Driven Design (RDD). They way "A
 class has responsibilities that fulfill its purpose." SRP doesn't require that
 a class do only one very narrow thing or that it change for only a single
 nitpicky reason, instead SRP requires that a class be cohensive--that
 everything the class does be highly related to its purpose.<br>
Do not feel compelled to make design decisions permaturely. When faced with an
 imperfect and muddled class like `Gear`, ask yourself: *"What is the future
 cost of doing nothing today?"*<br>
Always wrap instance variables in accessor methods instead of directly referring
 to variables. Data very often has behavior that you don't yet know about. Send
 messages to access variables, even if you think of them as data. If being
 attached to an instance variable is bad, depending on a complicated data
 structure is worse.

## 3. Managing Dependencies

Because well designed objects have a single responsibility, their very nature
 requires that they collaborate to accomplish complex tasks. To collaborate, an
 object must know something know about others. *Knowing* creates a
 dependency.<br>
These unnecessary dependencies make the code less *reasonable*. The more `Gear`
 knows about `Wheel`, the more tightly coupled they are. The more tightly
 coupled two objects are, the more they behave like a single entity.<br>
Another entire class of dependencies is that of tests on code. In the world
 outside of this book, tests come first. The natural tendency of
 "new-to-testing" programmers is to write tests that are too tightly coupled to
 code.<br>
Using dependency injection to shape code relies on your ability to recognize
 that the responsibility for knowing the name of a class and the responsibility
 for knowing the name of a message to send to that class may belong in different
 objects. Just because `Gear` needs to send `diameter` somewhere does not mean
 that `Gear` should know about `Wheel`. If you cannot remove unnecessary
 dependencies, you should isolate them within your class. Although not every
 external method is a candidate for this preemptive isolation, it's worth
 examining your code, looking for and wrapping the most vulnerable dependencies.

```ruby
def wheel
  @wheel ||= Wheel.new(rim, tire) # lazily creates a new instance
end

def initialize(args)
  args = defaults.merge(args)
  @chainring = args[:chainring] # @chainring = args.fetch(:chainring, 40)
end

def defaults
  {:chainring => 40, :cog => 18}
end
```

If you have control over the `Gear` `initialize` method, change the code to take
 a hash of options instead of a fixed list of parameters.<br>
An alternative way to eliminate these side effects is to avoid the problem from
 the very beginning by reversing the direction of the dependency. The choices
 you make about the direction of dependencies have far reaching consequences
 that manifest themselves for the life of your application. *Depend on things
 that change less often than you do* is a heuristic that stands in for all the
 ideas in this section.

## 4. Creating Flexible Interfaces

The roots of this new problem lie not in what each class *does* but with what it
 *reveals*. These exposed methods comprise the class's *public interface*. This
 chapter will address the first kind of interface, that is, methods within a
 class and how and what to expose to others. The next chapter explores the
 second kind of interface, the one that represents a concept that is broader
 than a class and is defined by a set of messages.<br>
Indead, public methods should read like a description of responsibilities.<br>
These classes spring to mind because they represent *nouns* in the application
 that have both *data* and *behavior*. Call them *domain objects*. Domain
 objects are easy to find but they are not at the design center of your
 application. If you fixate on domain objects you will tend to coerce behavior
 into them. Design experts focus not on these objects but on the messages that
 pass between them. These messages are guides that lead you to discover other
 objects, ones that are just as necessary but far less obvious.<br>
These is a perfect, low-cost way to experiment with objects and messages:
 *sequence diagrams*. Sequence diagrams are defined in the Unified Modeling
 Language (UML) and are one of many diagrams that UML supports.<br>
Also, notice now that you have drawn a sequence diagram, this design
 conversation has been inverted. Instead of deciding on a class and then
 figuring out its responsibilities, you are now deciding on a message and
 figuring out where to send it. This transition from class-based design to
 message-based design is a turning point in your design career. Changing the
 fundamental design question from "I know I need this class, what should it do?"
 to "I need to send this message, who should respond to it?" is the first step
 in that direction.<br>
The distinction between a mesage that asks for what the sender wants and a
 message that tells the receiver how to behave may seem subtle but the
 consequences are significant. When the conversation between `Trip` and
 `Mechanic` switched from a *how* to a *what*, one side effect was that the size
 of the public interface in `Mechanic` was drastically reduced.<br>
The things that `Trip` knows about other objects make up its *context*. The
 context that an object expects has a direct effect on how difficult it is to
 reuse. The best possible situation is for an object to be completely
 independent of its context.<br>
Expanding in this idea, `Trip` could place a number of such objects into an
 array and send each the `prepare_trip` message, trusting every preparer to do
 whatever it does because of the kind of thing that it is. This pattern allows
 you to add newly introduced preparers to `Trip` without changing any of its
 code, that is, you can *extend* `Trip` without *modifying* it.<br>
If objects were human and could describe their own relationships, in Figure 4.5
 `Trip` would be telling `Mechanic`: "I know what I want and I know how you do
 it;" in Figure 4.6: "I know what I want and I know whay you do" and in Figure
 4.7: "I know what I want and *I trust you to do your part*." This blind trust
 is a keystone of object-oriented design. It allows objects to collaborate
 without binding themselves to context and is necessary in any application that
 expects to grow and change.<br>
Either do not test private methods or, if you must, segregate those tests from
 the tests of public methods. Do not allow your tests to fool others into
 unintentionally depending on the changeable, private interface.<br>
Ruby provides three relevant keywords: `public`, `protected`, and `private`. Use
 of these keywords serves two distinct purposes. First, they indicate which
 methods are stable and which are unstable. Second, they control how visible a
 method is to other parts of your application.<br>
The Law of Demeter (LoD) is a set of coding rules that results in loosely
 coupled objects. Demeter restricts the set of objects to which a method may
 *send* messages; it prohibits routing a message to a third object via a second
 object of a different type. Demeter is often paraphrased as "only talk to your
 immediate neighbors" or "use only one dot." Demeter is trying to tell you
 something and it isn't "use more delegation."

## 5. Reducing Costs with Duck Typing

Duck types are public interfaces that are not tied to any specific class. Duck
 typed objects are chameleons that are defined more by their behavior than by
 their class.<br>
Class is just one way for an object to acquire a public interface; the public
 interface an object obtains by way of its class may be one of several that it
 contains. Applications may define many public interfaces that are not related
 to one specific class; these interfaces cut across class.<br>
Across-class types, duck types, have public interfaces that represent a contract
 that must be explicit and well-documented.<br>
Polymorphism in OOP refers to the ability of many different objects to respond
 to the same message. Senders of the message need not care about the class of
 the receiver; receivers supply their own specific version of the behavior. A
 single message thus has many (poly) forms (morphs).<br>
Several common coding patterns indicate the presence of a hidden duck. You can
 replace the following with ducks: 1) Case statements that switch on class, 2)
 `kind_of?` and `is_a?`, 3) `responds_to?`

## 6. Acquiring Behavior Through Inheritance

Inheritance is, at its core, a mechanism for *automatic message delegation*.<br>
The term *classical* is a play on the word *class*, not a node to an archaic
 technique, and it serves to distinguish this superclass/subclass mechanism from
 other inheritance techniques. JavaScript, for example, has prototypical
 inheritance and Ruby has *modules*, both of which also provide a way to share
 code via automatic delegation of messages. Because duck types cut across
 classes, they do not use classical inheritance to share common behavior. Duck
 types share code via Ruby modules.<br>
`nil` is an instance of class `NilClass`; it's an object like any other. Ruby
 contains two implementations of `nil?`, one in `NilClass`, and the other in
 `Object`. The implementation in `NilClass` unconditionally returns `true`, and
 one in `Object`, `false`.<br>
A decision to put off the creation of the `Bicycle` hierarhcy commits you to
 writing `MountainBike` and `RoadBike` classes that duplicate a great deal of
 code. A decision to proceed with the hierarchy accepts the risk that you may
 not yet have enough information to identify the correct abstraction.<br>
The general rule for refactoring into a new inheritance hierarchy is to arrange
 code so that you can promote abstractions rather than demote concretions.
 ("What will happen *when* I'm wrong?")<br>
This technique of defining a basic structure in the superclass and sending
 messages to acquire subclass-specific contributions is known as the *template
 method* pattern. Where it permits them to influence the algorithm, it sends
 messages. Subclasses contribute to the algorithm by implementing matching
 methods. Always document template method requirements by implementing matching
 methods that raise useful errors.<br>
It makes sense that subclasses know the specializations they contribute (they
 are obviously the only classes who *can* know them), but forcing a subclass to
 know how to interact with its abstract superclass causes many problems. Instead
 of allowing subclasses to know the algorithm and requiring that they send
 `super`, superclasses can instead send `hook` messages, ones that exists solely
 to provide subclasses a place to contribute information by implementing
 matching methods. This strategy removes knowledge of the algorithm from the
 subclass and returns control to the superclass.<br>
Well-designed inheritance hierarchies are easy to extend with new subclasses,
 even for programmers who know very little about the application. This ease of
 extension is inheritance's greatest strength.

## 7. Sharing Role Behavior with Modules

Use of classical inheritance is always optional; every problem that it solves
 can be solved another way. This chapter explores an alternative that uses the
 techniques of inheritance to share a *role*. Some problems require sharing
 behavior among otherwise unrelated objects. This common behavior is orthogonal
 to class; it's a *role* an object plays.<br>
The existence of a `Preparer` role suggests that there's also a parallel
 `Preparable` role (these things often come in pairs). The `Preparable` role is
 not terribly obvious because `Trip` is its only player but it's important to
 recognize that it exists.<br>
Many object-oriented languages provide a way to define a named group of methods
 that are independent of class and can be mixed in to any object. In Ruby, these
 mix-ins are called *modules*. Methods can be defined in a module
 (`module ... end`) and then the module can be added (`include`) to any
 object.<br>
This chapter has been careful to maintain a distinction between classical
 inheritance and sharing code via modules. This *is-a* versus *behaves-like-a*
 difference definitely matters, each choice has distinct consequences. However,
 the coding techniques for these two things are very similar and this similarity
 exists because both techniques rely on automatic message delegation.<br>
If all attempts to find a suitable method fail, you might expect the search to
 stop, but many languages make a second attempt to resolve the message. Ruby
 gives the original receiver a second change by sending it a new message,
 `method_missing`, and passing `:spares` as an argument.<br>
It is also possible to add a module's methods to a single object, using Ruby's
 `extend` keyword. Finally, any object can also have ad hoc methods added
 directly to its own personal "Singleton class." These ad hoc methods are unique
 to this specific object.<br>
Subclass that override a method to raise an exception like "does not implement"
 are a symptom of this problem. While it is true that expediency pays for all
 and that it is sometimes most cost effective to arrange code in just this way,
 you should be reluctant to do so. When subclasses override a method to declare
 that you *do not do that thing* they come perilously close to declaring that
 they *are not that thing*.<br>
Subclass agree to a *contract*; they promise to be substitutable for their
 superclasses. Where superclass place restrictions on input arguments and return
 values, subclasses can indulge in a slight bit of freedom without violating
 their contract. Subclasses may accept input parameters that have broader
 restrictions and may return results that have narrower restrictions, all while
 remaining perfectly substitutable for their superclasses. When you honor the
 contract, you are following the Liskov Substitution Principle (LSP), which is
 named for its creator, Barbara Liskov, and supplies the "L" in the SOLID design
 principles. Her principle states: Let *q*(*x*) be a property provable about
 objects *x* of type *T*. Then *q*(*x*) should be true for objects *y* of type
 *S* where *S* is a subtype of *T*.<br>
Hook methods solve the problem of sending `super`, but, unfortunately, only for
 adjacent levels of the hierarchy. The limitations of hook methods are just one
 of the many reasons to create shallow hierarchies.

## 8. Combining Objects with Composition

The line attaches to `Bicycle` with a black diamond; this black diamond
 indicates *composition*, it means that a `Bicycle` is composed of `Parts`.<br>
As formally defined *composition* means something a bit more specific; it
 indicates a *has-a* relationship where the contained object has no life
 independent of its container. *Aggregation* is exactly like composition except
 that the contained object has an independent life.<br>
Classical inheritance is a *code arrangement technique*. Behavior is dispersed
 among objects and these objects are organized into class relationships such
 that automatic delegation of messages invokes the correct behavior. Think of it
 this way: For the cost of arranging objects in a hierarchy, you get message
 delegation for free. Composition is an alternative that reverses these costs
 and benefits. In composition, the relationship between objects is not codified
 in the class hierarchy; instead objects stand alone and as a result must
 explicitly know about and delegate messages to one another. Composition allows
 objects to have structural independence, but at the cost of explicit message
 delegation. If you cannot explicitly defend inheritance as a better solution,
 use composition.

```ruby
require 'forwardable'
class Parts
  extend Forwardable
  def_delegators :@parts, :size, :each
  include Enumerable

  # ...
```

This is classical inheritance's greatest strength and biggest weakness;
 subclasses are bound, irrevocably and by design, to the classes above them in
 the hierarchy. These built-in dependencies amplify the effects of modifications
 made to superclasses.<br>
As you write code for a wider audience, your ability to anticipate needs
 necessarily decreases and the suitability of requiring inheritance as part of
 the interface goes down. Avoid writing frameworks that require users of your
 code to subclass your objects in order to gain your behavior. Their
 application's objects may already be arranged in a hierarchy; inheriting from
 your framework may not be possible.<br>
Use Inheritance for *is-a* Relationships / Use Duck Types for *behaves-like-a*
 (role) Relationships / Use Composition for *has-a* Relationships

## 9. Designing Cost-Effective Tests

From a practical point of view, changeability is the only design metric that
 matters; code that's easy to change *is* well-designed. Good design preserves
 maximum flexibility at minimum cost by putting off decisions at every
 opportunity, deferring commitments until more specific requirements arrive.
 When that day comes, *refactoring* is how you morph the current code structure
 into one that will accommodate the new requirements. New features will be added
 only after you have successfully refactored the code. An understanding of
 object-oriented design, good refactoring skills, and the ability to write
 efficient tests form a three-legged stool upon which changeable code rests.<br>
Intentionally depending on interfaces allows you to use tests to put off design
 decisions safely and without penalty. They let you put off design decisions and
 create abstractions to any useful depth.<br>
Your goal is to gain all of the benefits of testing for the least cost possible.
 One simple way to get better value from tests is to wrie fewer of them.

