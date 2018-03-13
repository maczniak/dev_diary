# [Introduction to Reliable and Secure Distributed Programming, Second Edition][homepage] by Christian Cachin, Rachid Guerraoui and Luís Rodrigues, Springer (2011)

The central theme of the book is the tolerance to uncertainty and adversarial
 influence in a distributed system, which may arise from network delays, faults,
 or even malicious attacks.<br>
Many aspects (of *distributed-system model*) have a fundamental impact on how an
 algorithm is designed, such as the reliability of the links, the degree of
 synchrony of the system, the severity of the failures, and whether a
 deterministic or a randomized solution is sought.<br>
Abstractions and algorithms in a model of distributed computing that allows
 adversarial attacks have become known as *Byzantine fault-tolerance*.<br>
The first edition of this book contained a companion set of running examples
 implemented in the Java programming language, using the *Appia* protocol
 composition framework.

[homepage]: http://www.distributedprogramming.net

## 1. Introduction

This very notion of *partial failures* is a characteristic of a distributed
 system. In fact, this notion can be useful if one really feels the need to
 differentiate a distributed system from a concurrent system.<br>
When using these applications, we are typically faced with the simplest form of
 distributed computing: *client-server* computing.<br>
We will use mainly two abstractions to represent the underlying physical system:
 *processes* and *links*.<br>
The cooperation among processes can sometimes be modeled as a distributed
 *agreement* problem. In some cases, it may be acceptable for the cooperating
 processes to take a given step only if all other processes also agree that such
 a step should take place. This form of agreement is crucial in the processing
 of distributed transactions, where this problem is known as the *atomic
 commitment* problem. Process may not only need to agree on which actions they
 should execute but also need to agree on the order in which these actions
 should be executed. This form of agreement is the basis of one of the most
 fundamental techniques to replicate computation in order to achieve fault
 tolerance, and it is called the *total-order broadcast* problem (deterministic
 replicas).<br>
Often applications that are not inherently distributed also use sophisticated
 abstractions from distributed programming. This need sometimes appears as an
 artifact of the engineering solution to satisfy some specific requirements such
 as *fault tolerance*, *load balancing*, or *fast sharing*.

Events from the same component are processed in the order in which they were
 triggered. The messages among different processes may also need to be ordered
 according to some criteria, using mechanisms orthogonal to this one. We assume
 that every process executes the code triggered by events in a mutually
 exclusive way.<br>
An algorithm that uses conditional event handlers relies on the run-time system
 to buffer external events until the condition on internal variables becomes
 satisfied. It is not difficult to avoid conditional event handlers in an
 implementation. Every conditional event handler can be transformed into a
 combination of a (pure) event handler and two handlers for internal events in
 three steps.<br>
The APIs of our components include two types of events, *requests* (input) and
 *indications* (output).<br>
classes of algorithms (not disjoint):
1. *fail-stop* algorithms, designed under the assumption that processes can fail
   by crashing but the crashes can be reliably detected by all the other
   processes, crash-stop + perfect links + perfect failure detector
1. *fail-silent* algorithms, where process crashes can never be reliably
   detected, crash-stop + perfect links + none
1. *fail-noisy* algorithms, where processes can fail by crashing and the crashes
   can be detected, but not always in an accurate manner (accuracy is only
   eventual), crash-stop + perfect links + eventually perfect failure detector
   or eventual leader detector
1. *fail-recovery* algorithms, where processes can crash and later recover and
   still participate in the algorithm, crash-recovery + stubborn links +
   eventual leader dectector
1. *fail-arbitrary* (or *fail-silent-arbitrary*) algorithms, where processes can
   deviate arbitrarily from the protocol specification and act in malicious,
   adversarial ways, fail-arbitrary + authenticated perfect links + Byzantine
   eventual leader-detector = *fail-noisy-arbitrary*
1. *randomized* algorithms, where in addition to the classes presented so far,
   processes may make probabilistic choices by using a source of randomness,
   Randomization is sometimes the only way to solve a problem or to circumvent
   inherent inefficiencies of deterministic algorithms

## 2. Basic Abstractions

As the transmission delay of a network is typically much larger than the local
 computation delay, the number of communication steps of an algorithm has a
 significant impact on the latency and the performance of a distributed
 algorithm.<br>
Basically, a *safety property* is a property of a distributed algorithm that can
 be violated at some time *t* and never be satisfied again after that time.
 Roughly speaking, safety properties state that the algorithm should not do
 anything wrong.<br>
*Liveness properties* ensure that eventually something good happens. More
 precisely, a liveness property is a property of a distributed system execution
 such that, for any time *t*, there is some hope that the property can be
 satisfied at some time *t'* ≥ *t*.<br>
The challenge is to guarantee both liveness and safety. Requiring that messages
 are not lost is a liveness property. Requiring that messages are not duplicated
 and that they are received in the order in which they were sent are safety
 properties.

process failures:
* crash
* omission
* crash with recovery
* eavesdropping
* arbitrary

The relation between the number *f* of potentially faulty processes and the
 total number *N* of processes in the system is generally called
 *resilience*.<br>
It is also important to notice that, in practice, the crash-stop process
 abstraction neither precludes the possibility of recovery nor does it mean that
 recovery should be prevented for a given algorithm to behave correctly. It
 simply means that the algorithm should not rely on some of the processes to
 recover in order to pursue its execution. In some sense, an algorithm that is
 not relying on crashed processes to recover would typically be faster than an
 algorithm relying on some of the processes to recover.<br>
Omission faults are not discussed further in this book. The crash-recovery
 abstraction can be viewed as an omission fault, with one exception, however: a
 process might suffer *amnesia* when it crashes and lose its internal state.
 This significantly complicates the design of algorithms because, upon recovery,
 the process might send new messages that contradict messages that the process
 might have sent prior to the crash. To cope with this issue, we sometimes
 assume that every process has a *stable storage* (also called a *log*), which
 can be accessed through *store* and *retrieve* operations. The ⟨*Init*⟩ event
 is considered atomic with respect to recovery. More precisely, if a process
 crashes in the middle of its initialization procedure, the process resumes
 again with processing the initialization procedure and then continues to
 process the ⟨*Recovery*⟩ event.<br>
In some sense, a crash-recovery kind of failure matches an omission fault if we
 consider that every process stores every update to any of its variables in
 stable storage. This is not very practical because access to stable storage is
 usually expensive. Therefore, a crucial issue in devising algorithms with the
 crash-recovery abstraction is to minimize the access to stable storage. One way
 to alleviate the need for accessing any form of stable storage is to assume
 that some of the processes never crash. In fact, the processes that do not
 crash implement a virtual stable storage abstraction, and the algorithm can
 exploit this without knowing in advance which of the processes will not crash
 in a given execution.<br>
Data encryption is generally orthogonal to the problems considered in this book,
 and confidentiality plays no significant role in implementing our distributed
 programming abstractions.<br>
The terms "arbitrary faulty" and "Byzantine" are synonyms throughout the
 book.

A *message-authentication code (MAC)* authenticates data between two entities.
 It is based on a shared symmetric key.<br>
A signature scheme is more powerful than a MAC in the sense that authenticated
 messages can be verified by all entities and relayed even by untrusted
 entities.

architectures that materialize the link abstraction - fully connected mesh,
 broadcast medium (Ethernet), ring, mesh of links interconnected by bridges and
 routers (Internet)<br>
They will usually have means to identify which reply message is a response to
 which request message. This can be achieved by having the processes generate
 timestamps or unique identifiers, based on sequence numbers, local clocks, or a
 source of randomness.<br>
We assume that some messages reach their destination because preventing all
 communication among two processes is difficult. We assume that such
 denial-of-service attacks cannot prevent all communication between correct
 processes in a distributed system.<br>
(crash-fault) *fair-loss links* (messages might be lost but the probability for
 a message not to be lost is nonzero), *stubborn* (delivered an unbounded number
 of times, retransmit forever with a timeout service), *perfect links*
 (*reliable links*, no duplication), (crash-recovery) *logged perfect links*
 (notify the layer above of only the name identifier of the logging variable in
 stable storage), (arbitrary faults) *authenticated links* (the no creation
 property of subborn links cannot be ensured)<br>
We prefer the term *deliber* over the more general term *receive*. A message is
 typically *received* at a given port of the network and stored within some
 buffer, and then some algorithm is executed to make sure the properties of the
 required link abstraction are satisfied, before the message is actually
 *delivered*.

*happened-before* relation *e*<sub>1</sub> → *e*<sub>2</sub> (*potentially
 caused*)<br>
Although assuming an *asynchronous* system comes down to not making any physical
 timing assumption on processes and links. assuming a *synchronous* system comes
 down to assuming the following properties: synchronous computation /
 communication / physical clocks (all with known upper bounds). In a synchronous
 distributed system, several useful services can be provided: timed failure
 detection, measure of transit delays, coordination based on time (such as a
 lease abstraction), worst-case performance, synchronized clocks (to execute
 synchronized global steps and timestamp events, events within the clock
 synchronization precision *δ* cannot be ordered). The major limitation of
 assuming a synchronous system model is the *coverage* of the model.<br>
Practical systems are *partially synchronous*. One way to capture partial 
 synchrony is to assume that the timing assumptions only hold eventually. This
 means that there is a time after which these assumptions hold forever, but this
 time is not known. In a way, instead of assuming a synchronous system, we
 assume a system that is *eventually synchronous*. The assumption simply
 captures the very fact that the system may not always be synchronous, and there
 is no bound on the period during which it is asynchronous. However, we expect
 that there are periods during which the system is synchronous, and some of
 these periods are long enough for an algorithm to do something useful or to
 terminate its execution.

We consider the *failure-detector* abstraction as a particularly useful way to
 abstract timing assumptions.<br>
In the face of Byzantine faults, a failure-detector abstraction is difficult to
 implement and its output may not be very useful. Therefore, we do not consider
 failure detectors with Byzantine process abstractions; but we will later
 discuss a realistic leader-detector abstraction that relies crucially on
 algorithm-specific information about the proper performance of a remote
 process.<br>
*perfect failure-detector* abstraction - (exclude on timeout) However, one would
 want to detect and react to failures earlier, with a shorter timeout. The risk
 here is that the probability to falsely detect a crash is higher. One way to
 cope with such a trade-off is to assume an imperfect failure detector.<br>
*leader election* abstraction - Often one may not need to detect which processes
 have failed, but rather need to identify one process that has *not* failed.
 This process may then act as the *leader* that coordinates some steps of a
 distributed algorithm, and in a sense it is *trusted* by the other processes to
 act as their leader. The abstraction is particularly useful in a primary-backup
 replication scheme, for instance.<br>
*eventually(◇) perfect failure detector* abstraction - (increasing timeout)
 detects crashes accurately after some a priori unknown point in time, but may
 make mistakes before that time.<br>
Instead of focusing on faulty processes, it may be better to look at correct
 ones. In particular, it is sometimes convenient to elect a correct process that
 will perform certain computations on behalf of the others. We can implement a
 weaker notion of leader election, which ensures the uniqueness of the leader
 only *eventually*. This abstraction is useful within consensus algorithms. An
 eventual leader-election abstraction can also be implemented with
 crash-recovery and arbitrary-fault process abstractions.<br>
*eventual leader election* - Nothing precludes the possibility for leaders to
 change in an arbitrary manner and for an arbitrary period of time. Moreover,
 many leaders might be elected during the same period of time without having
 crashed. Once a unique leader is determined, and does not change again, we say
 that the leader has *stabilized*. Monarchical Eventual Leader Detection and
 Elect Lower Epoch (every process maintains an *epoch number* that keeps track
 of how many times the process crashed and recovered)  algorithms.<br>
*Byzantine leader election* - Our approach is best described as "trust, but
 verify." More specifically, we assume that the leader should perform some
 actions according to a higher-level algorithm, within some time bounds. To make
 this work in an eventually synchronous system, every elected leader is given
 progressively more time than its predecessors to achieve its goal. We assume
 that every correct process successively increases the time between issuing
 complaints. In contrast to the eventual leader election, one cannot require
 that every correct process eventually trusts a *correct* process because a
 Byzantine process may behave just like a correct process. The Rotating
 Byzantine Leader Detection algorithm (assuming that *N* > 3*f*) maintains a
 continuously increasing round number and deterministically derives the leader
 from it. Whenever a process receives more than 2*f* COMPLAINT messages against
 the current leader, it switches to the next round. Furthermore, when a process
 receives more than *f* COMPLAINT messages but has not sent a COMPLAINT message
 itself in the current round, it joins the complaining process and also send a
 COMPLAINT message.<br>
For systems with fail-arbitrary processes, it is not possible to define failure
 detectors independently of the algorithms that rely on them. In constrast to
 fail-stop processes, such failures are not context-free.

A combination of (1) a process abstraction, (2) a link abstraction, and possibly
 (3) a failure-detector abstraction defines a *distributed-system model*.<br>
A *quorum* is a set of processes with special properties. Several algorithms
 rely on quorums and exploit the fact that every two quorums overlap in at least
 one process. In a system consisting of arbitrary-fault process abstractions,
 two majority quorums may not intersect in a correct process. A *Byzantine
 quorum* tolerating *f* faults is a set of *more than* (*N* + *f*)/2
 processes.<br>
Algorithms that have their performance go proportionally down when the number of
 failures increases are sometimes called *gracefully degrading* algorithms.

*Impossibility of distributed consensus with one faulty process* (Fischer,
 Lynch, and Patrson, 1985) established the fundamental result that no
 deterministic algorithm solves the consensus problem in an asynchronous system,
 even if only one process fails and it can only do so by crashing. Consensus can
 be solved in partially synchronous systems (*Consensus in the presence of
 partial synchrony*, Dwork, Lynch, and Stockmeyer, 1988).<br>
Algorithms that rely on unreliable failure detectors have been called
 "indulgent".<br>
Apart from the majority quorums considered here, there exist many other
 quorum-system constructions, which also ensure that every two quorums overlap
 in at least one process. They can replace the majority quorums in the
 algorithms in this book and sometimes also improve the performance of these
 algorithms.

## 3. Reliable Broadcast

(for crash-stop processes) *best-effort* (ensures delivery among all correct
 processes if the sender does not fail), *reliable* (ensures all-or-nothing
 delivery semantics, even if the sender fails), *totally ordered* (ensures that
 the delivery of messages follow the same global order), *terminating* (ensures
 that the processes either deliver a message or are eventually aware tht they
 should never deliver the message), (for arbitrary-faulty processes)
 *consistency* (ensure that two correct processes, if they deliver a message at
 all, deliver the same message)

*best-effort broadcast* (for crash faults) - no delivery guarantees are offered
 in case the sender fails.

*(regular) reliable broadcast* (for crash faults) - Ensuring agreement even when
 the sender fails is an important property for many practical applications that
 rely on broadcast. Our algorithm is said to be *lazy* in the sense that it
 retransmits a message only if the original sender has been detected to have
 crashed.

*uniform reliable broadcast* (for crash faults) - This definition is stronger in
 the sense that it guarantees that the set of messages delivered by *faulty*
 processes is always a *subset* of the messages delivered by correct processes.
 Many other abstraction also have such *uniform* variants. Uniformity is
 typically important if the processes interact with the external world. The
 other algoritms do not ensure *uniform agreement* (if a message *m* is
 delivered by some process **(whether correct or faulty)**, then *m* is
 eventually delivered by every correct process.) because a process may
 *rb*-deliver a message and then crash. We now give a uniform reliable broadcast
 algorithm that does not rely on a perfect failure detector but assuems a
 majority of correct processes, i.e., *N* > 2*f* if we assume that up to *f*
 processes may crash.

*stubborn broadcast* (for crash faults and crash-recovery) - communication
 abstractions in the fail-recovery model usually rely on logging their output to
 variables in stable storage. For stubborn broadcast, however, logging is not
 necessary because every delivered message is delivered infinitely often.

*logged best-effort broadcast* (for crash-recovery)

*logged uniform reliable broadcast* (for crash-recovery)

*probabilistic broadcast* (for crash faults) - these are often called *epidemic*
 (dissemination), *rumor mongering*, *gossip*, or *probablistic broadcast*
 algorithms. The parameter *k* is called the *fanout* of a gossip algorithm.
 Each step consisting of receiving a message and resending it is called a
 *round* of *gossiping*. The algorithm performs up to *R* rounds of gossiping
 for each message.

*causal broadcast* (for crash faults) - Causal order is a generalization of FIFO
 order that additionally preserves the potential causality among messages from
 multiple senders. These orderings are orthogonal to the reliability guarantees.
 / Instead of keeping a record of all past messages, it represents the past with
 a vector of *sequence numbers* (*vector clock*).

cryptography alone is seldom the solution for tolerating Byzantine processes. /
 consequently, we do not define any "uniform" variants of primitives in the
 fail-arbitrary model. / > (*N* + *f*)/2

*Byzantine consistent broadcast* (for Byzantine processes) - If a designated
 sender process *s* is faulty then the primitive ensures that every correct
 process delivers the same message, if it delivers one at all. This property is
 called *consistency* (safety property).

*Byzantine reliable broadcast* (for Byzantine processes) - how to complement
 *consistency* with a liveness property, such that the two properties together
 imply the equivalent of an *agreement* property in the fail-arbitrary model.
 extending Byzantine consistent broadcast with a *totality* property (if some
 message is delivered by any correct process, every correct processes eventually
 delivers a message). / The algorithm contains one more mechanism: when a
 process receives only *f* + 1 READY messages but has not sent a READY message
 yet, it also sends a READY message. This step implements an amplification of
 the READY messages and is crucial for the *totality* property.

*Byzantine Broadcast Channels* - delivers multiple messages and provides an
 equivalent to the reliable broadcast abstractions for crash-stop processes. two
 forms (Byzantine consistent|reliable (broadcast) channel)

## 4. Shared Memory

The memory abstractions are called *registers* because they resemble those
 provided by multiprocessor machines at the hardware level.<br>
We distinguish three kinds of semantics: *safe*, *regular*, and *atomic*.
 Roughly speaking, a safe register may return an arbitrary value when a write is
 concurrently ongoing. A regular register ensures a minimal guarantee in the
 face of concurrent or failed operations, and may only return the previous value
 or the newly written value. An atomic register is even stronger and provides a
 strict form of consistency even in the face of concurrency and failures.<br>
Each correct process accesses the registrs in a *sequential* manner, which means
 that afer a process has invoked an operation on a register, the process does
 not invoke any further operation on that register until the previous operation
 completes. There were no such restrictions for the broadcast abstractions.<br>
By a *serial* execution we mean that a process does not invoke an operation on a
 register if some other process has invoked an operation and has not received
 any reply for the operation. Note that this notice is stronger than the notion
 of sequential access.<br>
Any process that invokes a read or write operation and does not crash should
 eventually return from that invocation. Algorithms with this property are
 sometimes also called *robust* or *wait-free*.<br>
An operation is *comeplete* if its invocation and completion events have *both*
 occurred. An operation is said to *fail* when the process that invoked it
 crashes *before* the corresponding completion event occurs. If two operations
 are such that one precedes the other then we say that the operations are
 *sequential*. If neither one of two operations precedes the other then we say
 that they are *concurrent*. When no two operations are concurrent and all
 operations are complete, as in a serial execution, the order is total.<br>
*o*<sub>*r*</sub> reads from *o*<sub>*w*</sub> / value *v* is read from
 *o*<sub>*w*</sub> / value *v* is written (by *o*<sub>*w*</sub>)

(1,*N*) Regular Register (ONRR)

(1,*N*) Atomic Register (ONAR)

## 5. Consensus

## 6. Consensus Variants

## 7. Concluding Remarks

