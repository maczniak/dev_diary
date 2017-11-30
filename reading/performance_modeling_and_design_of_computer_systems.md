# [Performance Modeling and Design of Computer Systems][homepage] by Mor Harchol-Balter, Cambridge University Press (2013)

[errata][errata], [Mathematica code][software]

[homepage]: http://www.cs.cmu.edu/~harchol/PerformanceModeling/book.html
[errata]: http://www.cs.cmu.edu/~harchol/PerformanceModeling/errata.html
[software]: http://www.cs.cmu.edu/~harchol/PerformanceModeling/software.html

## Part I: Introduction to Queueing

### 1 Motivating Examples of the Power of Analytical Modeling

Many times, however, without buying any additional resources at all, one can
 improve performance just by deploying a smarter scheduling policy or different
 routing policy to reduce delays.<br>
Queueing theory is built on a much broader area of mathematics called stochastic
 modeling and analysis. For example, the CPU requirements of UNIX processes
 might be modeled using a Pareto distribution, whereas the arrival process of
 jobs at a busy web server might be well modeled by a Poisson process with
 Exponentially distributed interarrival times.<br>
Although it is generally possible to come up with a stochastic model that
 adequately represents the jobs or customers in a system and its service
 dynamics, these stochastic models are not always analytically tractable with
 respect to solving for performance. As we discuss in Part IV, *Markovian
 assumptions*, such as assuming Exponentially distributed service demands or a
 Poisson arrival process, greatly simplify the analysis; hence much of the
 existing queueing literature relies on such Markovian assumptions. However, in
 some cases Markovian assumptions are very far from reality; for example, in the
 case in which service demands of jobs are highly variable or are
 correlated.<br>
I therefore put great emphasis on integrating measured workload distributions
 into the analysis. In my opinion, a major reason why computer scientists are so
 slow to adopt queueing theory is that the standard Markovian assumptions often
 do not fit. However, there are often ways to work around these assumptions,
 many of which are shown in this book, such as using phase-type distributions
 and matrix-analytic methods.

Random, Round-Robin, Shortest-Queue, Size-Interval-Task-Assignment (SITA),
 Least-Work-Left (LWL), Central-Queue<br>
(lowest mean response time) In fact, for a long time it was believed that SITA
 is always better than LWL when job size variability is high. However, it was
 recently discovered that SITA can be far worse than LWL even under job size
 variability tending to infinity. It turns out that other properties of the
 workload, including load and fractional moments of the job size distribution,
 matter as well.<br>
Task assignment policies that are best for FCFS servers are often a disaster
 under PS (Processor-Sharing) servers. For PS servers, the Shortest-Queue policy
 is near optimal, whereas that policy is pretty bad for FCFS servers if job size
 variability is high.<br>
For example, *cycle stealing* (talking advantage of a free host to process jobs
 in some other queue) can be combined with many existing task assignment
 policies to create improved policies. There are also other metrics to consider,
 like minimizing the variance of response time, rather than mean response time,
 or maximizing fairness.

First-Come-First-Served (FCFS), Non-Preemptive Last-Come-First-Served (LCFS),
 Random<br>
Suppose we change the non-preemptive LCFS policy to a Preemptive-LCFS policy
 (PLCFS), which works as follows: Whenever a new arrival enters the system, it
 immediately preempts the job in the service. If the job size distribution is at
 least moderately variable, then PLCFS will be a huge improvement. If the job
 size distribution is hardly variable (basically constant), then PLCFS policy
 will be up to a factor of 2 worse.<br>
We mention many such open problems in this book, and we encourage readers to
 attempt to solve these!

### 2 Queueing Theory Terminology

* Service Order
* Average Arrival Rate λ, Mean Interarrival Time 1/λ
* Service Requirement, Size
* Mean Service Time **E**[*S*], Average Service Rate μ = 1/**E**[*S*]
* Response Time, Turnaround Time, Time in System, Sojourn Time *T*
* Waiting Time, Delay, time in queue, wasted time *T*<sub>*Q*</sub>, **E**[*T*]
  = **E**[*T*<sub>*Q*</sub> + **E**[*S*]
* Number of Jobs in the System *N*
* Number of Jobs in Queue *N*<sub>*Q*</sub>
* Device Utilization ρ (fraction of time device is busy)
* Device Throughput *X* = μ·ρ (rate of completions at device), *the Utilization
  Law* ρ = *X*·**E**[*S*]

Therefore queueing (waiting) results from *variability* in service time and/or
 interarrival time distributions.<br>
Queueing networks can be classified into two categories: open networks and
 closed networks.
(in open networks) *X* = λ (changing μ affects the *maximum possible X*),
 Throughput and response time are *not* related<br>
Closed queueing networks can be classified into two categories: Interactive
 (terminal-driven) and Batch system.<br>
(interactive system) The number of jobs in the system is fixed (equal to the
 number of terminals). This number is sometimes called the load or MPL
 (multiprogramming level). There is a think time, *Z*, which is a random
 variable representing the time at each terminal between receiving the result of
 one job and sending out the next job.<br>
**E**[*T*] = **E**[*R*] + **E**[*Z*], Although "response time" in open systems
 is denoted by the random variable (r.v.) *T*, for closed interactive systems,
 we refer to *T* as the *system time* (or "time in system") and reserve the r.v.
 *R* for *response time*.<br>
Schroeder et al. proposes the idea of a "partly-open" system. Here users arrive
 from outside as in an open system, but make *k* requests to the system when
 they arrive, where each request can only be made when the previous request
 completes (as in a closed system).
(in closed network) *X* = μ, Throughput and response time are related<br>
Nope, the slower server is still not always busy. What we're missing here is the
 fact that sometimes the slow server is faster than the fast server--because
 these service rates are just averages!

## Part II: Necessary Probability Background

### 3 Probability Review

### 4 Generating Random Variables for Simulation

### 5 Sample Paths, Convergence, and Averages

## Part III: The Predictive Power of Simple Operational Laws: "What-If" Questions and Answers

### 6 Little's Law and Other Operational Laws

### 7 Modification Analysis: "What-If" for Closed Systems

## Part IV: From Markov Chains to Simple Queues

### 8 Discrete-Time Markov Chains

### 9 Ergodicity Theory

### 10 Real-World Examples: Google, Aloha, and Harder Chains

### 11 Exponential Distribution and the Poisson Process

### 12 Transition to Continuous-Time Markov Chains

### 13 M/M/1 and PASTA

## Part V: Server Farms and Networks: Multi-server, Multi-queue Systems

### 14 Server Farms: M/M/k and M/M/k/k

### 15 Capacity Provisioning for Server Farms

### 16 Time-Reversibility and Burke's Theorem

### 17 Networks of Queues and Jackson Product Form

### 18 Classes Network of Queues

### 19 Closed Networks of Queues

## Part VI: Real-World Workloads: High Variability and Heavy Tails

### 20 Tales of Tails: A Case Study of Real-World Workloads

### 21 Phase-Type Distributions and Matrix-Analytic Methods

### 22 Networks with Time-Sharing (PS) Servers (BCMP)

### 23 The M/G/1 Queue and the Inspection Paradox

### 24 Task Assignment Policies for Server Farms

### 25 Transform Analysis

### 26 M/G/1 Transform Analysis

### 27 Power Optimization Application

## Part VII: Smart Scheduling in the M/G/1

### 28 Performance Metrics

### 29 Scheduling: Non-Preemptive, Non-Size-Based Policies

### 30 Scheduling: Preemptive, Non-Size-Based Policies

### 31 Scheduling: Non-Preemptive, Size-Based Policies

### 32 Scheduling: Preemptive, Size-Based Policies

### 33 Scheduling: SRPT and Fairness

