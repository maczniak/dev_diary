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

## 5. Reducing Costs with Duck Typing

## 6. Acquiring Behavior Through Inheritance

## 7. Sharing Role Behavior with Modules

## 8. Combining Objects with Composition

## 9. Designing Cost-Effective Tests

