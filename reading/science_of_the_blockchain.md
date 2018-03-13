# The Science of the Blockchain by [Roger Wattenhofer][author], Inverted Forest Publishing (2016)

[Principles of Distributed Computing][principles_of_distributed_computing]
 (Spring 2016) by Roger Wattenhofer<br>
[Notes on Theory of Distributed Systems][notes_on_theory_of_distributed_systems]
 (Fall 2017) by James Aspnes

[author]: https://disco.ethz.ch/members/wroger
[principles_of_distributed_computing]: https://disco.ethz.ch/courses/podc_allstars/lecture/podc.pdf
[notes_on_theory_of_distributed_systems]: http://www.cs.yale.edu/homes/aspnes/classes/465/notes.pdf

## 1. Introduction

Coordination problems of distributed systems are so prevalent, they come with
 various flavors and names: Blockchain, consistency, agreement, consensus,
 ledger, event sourcing, etc.

## 2. Fault-Tolerance & Paxos

A set of nodes achieves *state replication*, if all nodes execute a (potentially
 infinite) sequence of commands c<sub>1</sub>,c<sub>1</sub>,c<sub>1</sub>,...,
 in the same order. State replication is a fundamental property for distributed
 systems. The Bitcoin blockchain is indeed one way to implement state
 replication.<br>
Since state replication is trivial with a single server, we can designate a
 single server as a *serializer*. This idea is sometimes also referred to as
 *master-slave replication*.<br>
This *locking* idea appears in many contexts and with different names, usually
 with slight variations, e.g. *two-phase locking (2PL)*. Another example is the
 *two-phase commit (2PC)* protocol, typically presented in a database
 environment. The first phase is called the *preparation* of a transaction, and
 in the second phase the transaction is either *commiteed* or *aborted*. The 2PC
 process is not started at the client but at a designated server node that is
 called the *coordinator*. This consistency benefit was even improved in a
 protocol that use an additional phase (3PC).

A *ticket* is a weaker form of a lock, with the following properties: Reissuable
 and Ticket expiration (only accept the most recently issued ticket)<br>
(possible optimizations) The performance can be improved by letting the servers
 send negative replies in phases 1 and 2 if the ticket expired. The contention
 between different clients can be alleviated by randomizing the waiting times
 between consecutive attempts.<br>
If the client with the first successful proposal does not crash, it will
 directly tell every server to execute *c*. Note that Paxos cannot make progress
 if half (or more) of the servers crash, as clients cannot achieve a majority
 anymore.<br>
The original description of Paxos uses three roles: Proposers, acceptors and
 learners. Learners have a trivial role: They do nothing, they just learn from
 other nodes which command was chosen.<br>
Clients (Proposers) must be trusted to follow the protocol strictly. However,
 this is in many scenarios not a reasonable assumption. In suc scenarios, the
 role of the proposer can be executed by a set of servers, and clients need to
 contact proposers, to propose values in their name.<br>
We call such a single decision an *instance* of Paxos. If we want to execute
 multiple commands, we can extend each instance with an instance number, that is
 sent around with every message. If a server did not realize that the previous
 instance came to a decision, the server can ask other servers about the
 decisions to catch up.<br>
Leslie Lamport introduced Paxos in 1989. Lamport described the algorithm as the
 solution to a problem of the parliament of a fictitious Greek society on the
 island Paxos. He even liked this idea so much, that he gave some lectures in
 the persona of an Indiana-Jones-style archaeologist! The paper was rejected,
 but Lamport refused to rewrite it. A few years later, when the need for a
 protocol like Paxos arose again, Lamport simply took the paper out of the
 drawer and gave it to his colleagues. They liked it. So Lamport decided to
 submit the paper (*The part-time parliament*, 1998) again, 8 years after he
 wrote it. But as it is admittedly hard to read, he had mercy, and later wrote a
 simpler description of Paxos (*Paxos made simple*, 2001).

## 3. Consensus

(consensus) There are n nodes, of which at most f might crash, i.e., at least n
 - f nodes are *correct*. Node i starts with an input value v<sub>i</sub>. The
 nodes must decide for one of those values, satisfying the following properties:
 Agreement, Termination (in finite time), Validity (not a random value)<br>
Paxos does not guarantee termination, if two clients continuously request
 tickets and neither of them ever manages to acquire a majority.<br>
For algorithms in the asynchronous model, the *runtime* is the number of time
 units from the start of the execution to its completion in the worst case,
 assuming that each message has a delay of *at most* one time unit.<br>
The *configuration* includes the state of every node, and all messages that are
 in transit (sent but not yet received). We call a configuration C *univalent*
 (with agreement) if the decision value is determined independently of what
 happens afterwards. A configuration C is called *bivalent* (without agreement)
 if the nodes might decide for 0 or 1. We say that a configuration C is
 *critical*, if C is bivalent, but all configurations that are direct children
 of C in the configuration tree are univalent. The only way how an algorithm can
 *enforce* to arrive in a univalent configuration is by reaching a critical
 configuration.

There is no deterministic algorithm which always achieves consensus in the
 asynchronous model, with f > 0 (if a single node may crash). If an algorithm
 solves consensus, all executions starting from the bivalent configuration C
 must reach a critical configuration. But if the algorithm reaches a critical
 configuration, a single crash can prevent agreement. How can the situation be
 improved? For example by giving each node access to randomness, i.e., we allow
 each node to toss a coin.<br>
As long as no node sets *decided* to true, Randomized Consensus Algorithm always
 makes progress, independent of which nodes crash.

## 4. Byzantine Agreement

## 5. Authenticated Agreement

## 6. Quorum Systems

## 7. Eventual Consistency & Bitcoin

## 8. Distributed Storage

