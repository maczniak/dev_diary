# [Reinforcement Learning: An Introduction, Second edition][homepage] by Richard S. Sutton and Andrew G. Barto, The MIT Press (not yet)

(bookdraft2017june19.pdf)

[homepage]: http://incompleteideas.net/sutton/book/the-book-2nd.html

### 1. The Reinforcement Learning Problem

Learning from interaction is a foundational idea underlying nearly all theories
 of learning and intelligence.<br>
three characteristics - 1) being closed-loop in an essential way, 2) not having
 direct instructions as to what actions to take, and 3) where the consequences
 of actions, including reward signals, play out over extended time periods<br>
The MDP formulation is intended to include just these three aspects--sensation,
 action, and goal--in their simplest possible forms without trivializing any of
 them. Any method that is well suited to solving such problems we consider to be
 a RL method.<br>
RL is trying to maximize a reward signal instead of (unsupervised learning)
 trying to find hidden structure.<br>
One of the challenges that arise in RL, and not in other kinds of learning, is
 the trade-off between exploration and exploitation. Another key feature of RL
 is that it explicitly considers the *whole* problem of a goal-directed agent
 interacting with an uncertain environment. This is in contrast with many
 approaches that consider subproblems without addressing how they might fit into
 a larger picture.<br>
Of all the forms of machine learning, RL is the closet to the kind of learning
 that humans and other animals do, and many of the core algorithms of RL were
 originally inspired by biological learning systems. Finally, RL is also part of
 a larger trend in AI back toward simple general principles.

Beyond the agent and the environment, one can identify four main subelements of
 a RL system: a *policy*, a *reward signal*, a *value function*, and,
 optionally, a *model* of the environment. A *policy* defines the learning
 agent's way of behaving at a given time. A *reward signal* defines the goal in
 a RL problem. On each time step, the environment sends to the RL agent a single
 number, a *reward*. The agent's sole objective is to maximize the total reward
 it receives over the long run. In Chaper 3 we explain how the idea of a reward
 function being unalterable by the agent is consistent with what we see in
 biology where reward signals are generated within an animal's brain. Whereas
 the reward signal indicates what is good in an immediate sense, a *value
 function* specifies what is good in the long run. In fact, the most important
 component of almost all RL algorithms we consider is a method for efficiently
 estimating values. The fourth and final element of some RL systems is a *model*
 of the environment. This is something that mimics the behavior of the
 environment, or more generally, that allows inferences to be made about how the
 environment will behave. For example, given a state and action, the model might
 predict the resultant next state and next reward. Models are used for
 *planning*, by which we mean any way of deciding on a course of action by
 considering possible future situations before they are actually experienced.
 Methods for solving RL problems that use models and planning are called
 *model-based* methods, as opposed to simpler *model-free* methods that are
 explicitly trial-and-error learners--viewd as almost the *opposite* of
 planning.<br>
For example, methods such as genetic algorithms, genetic programming, simulated
 annealing, and other optimization methods have been used to approach RL
 problems without evr appealing to value functions. These methods evaluate the
 "lifetime" behavior of many non-learning agents, each using a different policy
 for interacting with its environment, and select those that are able to obtain
 the most reward. We call these *evolutionary* methods because their operation
 is analogous to the way biological evolution produces organisms with skilled
 behavior even when they do not learn during their individual lifetimes. If the
 space of policies is sufficiently small, or can be structured so that good
 policies are common or easy to find--or if a lot of time is available for the
 search--then evolutionary methods can be effective. In addition, evolutionary
 methods have advantages on problems in which the learning agent cannot
 accurately sense the state of its environment.<br>
These methods search in spaces of policies defined by a collection of numerical
 parameters. They estimate the directions the parameters should be adjusted in
 order to most rapidly improve a policy's performance. Unlike evolutionary
 methods, however, they produce these estimates while the agent is interacting
 with its environment and so can take advantages of the details of individual
 behavioral interactions. Methods like this, called *policy gradient methods*,
 have proven useful in many problems, and some of the simplest RL methods fall
 into this category.

Standard engineering practice has long required careful examination of any
 result of an optimization process before using that result in constructing a
 product, a structure, or any real-world system whose safe performance people
 will rely upon.<br>
Because models have to be reasonably accurate to be useful, model-free methods
 can have advantages over more complex methods when the real bottleneck in
 solving a problem is the difficulty of constructing a sufficiently accurate
 environment model. Model-free methods are also important building blocks for
 model-based methods.<br>
[cyberneticzoo.com][cyberneticzoo_com] (a history of cybernetic animals and
 early robots)<br>
The temporal-difference and optimal control threads were fully brought together
 in 1989 with Chris Watkin's development of Q-learning. This work extended and
 integrated prior work in all three threads of RL research.<br>
In the time since publication of the first edition of this book, a flourishing
 subfield of neuroscience developed that focuses on the relationship between
 RL algorithms and RL in the nervous system. Most responsible for this is an
 uncanny similarity between the behavior of temporal-difference algorithms and
 the activity of dopamine producing neurons in the brain.<br>
Risk is important for reinforcement learning as well but beyond our scope here.

[cyberneticzoo_com]: http://cyberneticzoo.com

## I. Tabular Solution Methods

### 2. Multi-armed Bandits

nonassociative - does not involve learning to act in more than one situation<br>
*NewEstimate* ← *OldEstimate* + *StepSize* [*Target* - *OldEstimate*] (noisy
 target, *error* in the estimate)<br>
Although sequences of step-size parameters that meet these convergence
 conditions are often used in theoretical work, they are seldom used in
 applications and empirical research.<br>
*upper confidence bound* (UCB) action selection - *A*<sub>*t*</sub> ≐
 argmax<sub>*a*</sub>[*Q*<sub>*t*</sub>(*a*) +
 *c*√(log*t*/*N*<sub>*t*</sub>(*a*))] (measure of the uncertainty or variance in
 the estimate of *a*'s value, the confidence level)<br>
UCB will often perform well, but is more difficult than *ε*-greedy to extend
 beyond bandits to the more general RL settings. One difficulty is in dealing
 with nonstationary problems. Another difficulty is dealing with large state
 spaces, particularly function approximation. In these more advanced setting
 there is currently no known practical way of utilizing the idea of UCB action
 selection.<br>
The gradient bandit algorithm (that estimates not action values, but action
 preferences) is based on the idea of stochastic gradient ascent.

However, in a general RL task there is more than one situation, and the goal is
 to learn a policy: a mapping from situations to the actions that are best in
 those situations. This is an example of an *associative search* task (or
 *contextual bandits* in the modern literature), so called because it involves
 both trial-and-error learning in the form of *search* for the best actions and
 *association* of these actions with the situations in which they are best.
 Associative search tasks are intermediate between the *k*-armed bandit problem
 and the full RL problem (learning a policy, but only the immediate reward). We
 prefer to reserve associative RL as a synonym for the full RL problem.<br>
*Bayesian* methods assume a known initial distribution over the action values
 and then updates the distribution exactly after each step (assuming that the
 true action values are stationary). This method, sometimes called *posterior
 sampling* or *Thompson sampling*, often performs similarly to the best of the
 distribution-free methods we have presented in this chapter. In the Bayesian
 setting it is even conceivable to compute the *optimal* balance between
 exploration and exploitation.

### 3. Finite Markov Decision Processes

The general rule we follow is that anything that cannot be changed arbitarily by
 the agent is considered to be outside of it and thus part of its environment
 (including the reward source). The agent-environment boundary represents the
 limit of the agent's *absolute control*, not of its knowledge. Everything
 inside the agent is completely known and controllable by the agent.<br>
The reward signal is your way of communicating to the robot *what* you want it
 to achieve, not *how* you want it achieved.<br>
In this book, by "the state" we mean whatever information is available to the
 agent. We assume that the state is given by some preprocessing system that is
 nominally part of the environment.<br>
What we would like, ideally, is a state signal that summarizes past sensations
 compactly, yet in such a way that all relevant information is retained. A
 state signal that succeeds in retaining all relevant information is said to be
 *Markov*, or to have *the Markov property*. This is sometimes also referred to
 as an "independence of path" property because all that matters is in the
 current state signal. It is useful to think of the state at each time step as
 an approximation to a Markov state, although one should remember that it may
 not fully satisfy the Markov property. A full understanding of the theory of
 the Markov case is an essential foundation for extending it to the more complex
 and realistic non-Markov case. Finally, we note that the assumption of Markov
 state representations is not unique to RL but is also present in most if not
 all other approaches to AI.<br>
As a result, the state representations people use to make their poker decisions
 are undoubtedly non-Markov, and the decisions themselves are presumably
 imperfect. Nevertheless, people still make very good decisions in such tasks.
 We conclude that the inability to have access to a *perfect* Markov state
 representation is probably not a severe problem for a reinforcement learning
 agent.

A RL task that satisfies the Markov property is called a *Markov decision
 process*, or *MDP*. If the state and action spaces are finite, then it is
 called a *finite Markov decision process*. Finite MDPs are all you need to
 understand 90% of modern RL. In this edition we will predominantly use the
 explicit notation (not their expectation) of *p*(*s*′,*r*|*s*,*a*) ≐
 Pr{*S*<sub>*t*+1</sub>=*s*′, *R*<sub>*t*+1</sub>=*r* | *S*<sub>*t*</sub>=*s*,
 *A*<sub>*t*</sub>=*a*} (dynamics of the environment, dynamics of a finite MDP,
 or world's dynamics)<br>
We call those diagrams *backup diagrams* because they diagram relationships that
 form the basis of the update or *backup* operations that are at the heart of RL
 methods. These operations transfer value information *back* to a state (or a
 state-action pair) from its successor states (or state-action pairs).<br>
The term greedy is used in computer science to describe any search or decision
 procedure that selects alternatives based only on local or immediate
 considerations, without considering the possibility that such a selection may
 prevent future access to even better alternatives. The beauty of
 *v*<sub>∗</sub> is that if one uses it to evaluate the short-term consequences
 of actions--specifically, the one-step consequences--then a greedy policy is
 actually optimal in the long-term sense in which we are interested. By means of
 *v*<sub>∗</sub>, the optimal expected long-term return is turned into a
 quantity that is locally and immediately available for each state.

Many RL methods can be clearly understood as approximately solving the Bellman
 optimality equation, using actual experienced transitions in place of knowledge
 of the expected transitions. For example, heuristic search methods can be
 viewed as expanding the right-hand side several times, up to some depth,
 forming a "tree" of possibilities, and then using a heuristic evaluation
 function to approximate *v*<sub>∗</sub> at the "leaf" nodes. (Heuristic search
 methods such as A<sup>∗</sup> are almost always based on the episodic case.)
 The methods of dynamic programming can be related even more closely to the
 Bellman optimality equation.<br>
A critical aspect of the problem facing the agent is always the computational
 power available to it, in particular, the amount of computation it can perform
 in a single time step. The memory available is also an important constraint. In
 tasks with small, finite state sets, it is possible to form these
 approximations using arrays or tables with one entry for each state (or
 state-action pair). This we call the *tabular* case, and the corresponding
 methods we call tabular methods.<br>
However, it also presents us with some unique opportunities for achieving useful
 approximations. For example, in approximating optimal behavior, there may be
 many states that the agent faces with such a low probability that selecting
 suboptimal actions for them has little impact on the amount of reward the agent
 receives.<br>
The counterpart of the Bellman optimality equation for continuous time and state
 problems is known as the Hamilton-Jacobi-Bellman equation (or often just the
 Hamilton-Jacobi equation), indicating its roots in classical physics.

### 4. Dynamic Programming

In fact, all of these methods can be viewed as attempts to achieve much the same
 effect as DP, only with less computation and without assuming a perfect model
 of the environment. Although DP ideas can be applied to problems with
 continuous state and action spaces, exact solutions are possible only in
 special cases. A common way of obtaining approximate solutions for tasks with
 continuous states and actions is to quantize the state and action spaces and
 then apply finite-state DP methods.<br>
DP algorithms are obtained by turning Bellman equations such as these into
 assignments, that is, into update rules for improving approximations of the
 desired value functions.<br>
All the backups done in DP algorithms are called *full backups* because they are
 based on all possible next states rather than on a sample next state.<br>
This slightly different (in-place) algorithm also converges to *v*<sub>π</sub>;
 in fact, it usually converges faster than the two-array version, as you might
 expect, since it uses new data as soon as they are available. We usually have
 the in-place version in mind when we think of DP algorithms.<br>
These sorts of nonlinearities and arbitary dynamics often occur in real problems
 and cannot easily be handled by optimization methods other than dynamic
 programming.

In fact, the policy evaluation step of policy iteration can be truncated in
 several ways without losing the convergence guarantees of policy iteration. One
 important special case is when policy evaluation is stopped after just one
 sweep (one backup of each state). This algorithm is called *value iteration*.
 Faster convergence is often achieved by interposing multiple policy evaluation
 sweeps between each policy improvement sweep.<br>
A major drawback to the DP methods that we have discussed so far is that they
 involve operations over the entire state set of the MDP, that is, they require
 sweeps of the state set. *Asynchronous* DP algorithms are in-place iterative
 DP algorithms that are not organized in terms of systematic sweeps of the state
 set. Asynchronous DP algorithms allow great flexibility in selecting states to
 which backup operations are applied. We can try to order the backups to let
 value information propagate from state to state in an efficient way.
 Asynchronous algorithms also make it easier to intermix computation with
 real-time interaction. This makes it possible to *focus* the DP algorithm's
 backups onto parts of the state set that are most relevant to the agent. This
 kind of focusing is a repeated theme in RL.<br>
The value function stabilizes only when it is consistent with the current
 policy, and the policy stabilizes only when it is greedy with respect to the
 current value function.

If we ignore a few technical details, then the (worst case) time DP methods take
 to find an optimal policy is polynomial in the number of states and actions.
 DP is comparatively better suited to handling large state spaces than competing
 methods such as direct search and linear programming. In practice, policy
 iteration and value iteration (the two most popular DP methods) usually
 converge much faster than their theoretical worst-case run times, particularly
 if they are started with good initial value functions or policies.<br>
That is, they update estimates on the basis of other estimates. We call this
 general idea *bootstrapping*. Many RL methods perform bootstrapping, even those
 that do not require, as DP requires, a complete and accurate model of the
 environment. In the next chapter we explore RL methods that do not require a
 model and do not bootstrap. In the chapter after that we explore methods that
 do not require a model but do bootstrap.<br>
The version of the algorithm that uses two arrays, one holding the old values
 while the other is updated, is often called a *Jacobi-style* algorithm, after
 Jacobi's classical use of this method. It is also sometimes called a
 *synchronous* algorithm because it can be performed in parallel, with separate
 processors simultaneously updating the values of individual states using input
 from other processors. The in-place version of algorithm is often called a
 *Gauss-Seidel-style* algorithm after the classical Gauss-Seidel algorithm for
 solving systems of linear equations.

### 5. Monte Carlo Methods

Monte Carlo methods are ways of solving the RL problem based on averaging sample
 returns (as opposed to methods that learn from partial returns, considered in
 the next chapter). Monte Carlo methods can thus be incremental in an
 episode-by-episode sense, but not in a step-by-step (online) sense. Both
 first-visit MC and every-visit MC converge to *v*<sub>π</sub> as the number of
 visits (or first visits) to *s* goes to infinity.<br>
An important fact about Monte Carlo methods is that the estimates for each state
 are indenpendent (no bootstrap).<br>
If a model is not available, then it is particularly useful to estimate *action*
 values rather than *state* values. With a model, state values alone are
 sufficient to determine a policy. Thus, one of our primary goals for Monte
 Carlo methods is to estimate *q*<sub>π</sub>.

How can we avoid the unlikely assumption of exploring starts? On-policy methods
 attempt to evaluate or improve the policy that is used to make decisions,
 whereas off-policy methods evaluate or improve a policy different from that
 used to generate the data.<br>
In on-policy control methods the policy is generally *soft*, meaning that
 π(*a*|*s*) > 0 all *s* ∈ 𝒮 and *a* ∈ 𝒜(*s*). The on-policy method we present
 in this section uses ε-*greedy* policies. The ε-*greedy* policies are examples
 of ε-*soft* policies, defined as policies for which π(*a*|*s*) ≥ ε/|𝒜(*s*)|
 for all states and actions, for some ε > 0.<br>
A more straightforward approach is to use two policies, one that is learned
 about and that becomes the optimal policy, and one that is more exploratory and
 is used to generate behavior. The policy being learned about is called the
 *target policy*, and the policy used to generate behavior is called the
 *behavior policy*. Off-policy methods require additional concepts and notation,
 and because the data is due to a different policy, off-policy methods are often
 of greater variance and are slower to converge. On the other hand, off-policy
 methods are more powerful and general. Off-policy methods also have a variety
 of additional uses in applications. For example, they can often be applied to
 learn from data generated by a conventional non-learning controller, or from a
 human expert.<br>
Almost all off-policy methods utilize *importance sampling*, a general technique
 for estimating expected values under one distribution given samples from
 another.

### 6. Temporal-Difference Learning

### 7. Multi-step Bootstrapping

### 8. Planning and Learning with Tabular Methods

## II. Approximate Solution Methods

### 9. On-policy Prediction with Approximation

### 10. On-policy Control with Approximation

### 11. Off-policy Methods with Approximation

### 12. Eligibility Traces

### 13. Policy Gradient Methods

## III. Looking Deeper

### 14. Psychology

### 15. Neuroscience

### 16. Applications and Case Studies

### 17. Frontiers

