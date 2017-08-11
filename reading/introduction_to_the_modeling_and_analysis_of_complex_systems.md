# [Introduction to the Modeling and Analysis of Complex Systems][homepage], by Hiroki Sayama, Open SUNY Textbooks (2015)

[Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License][cc_noncommercial_3_0]

[homepage]: http://bingweb.binghamton.edu/~sayama/textbook/
[cc_noncommercial_3_0]: https://creativecommons.org/licenses/by-nc-sa/3.0/us/

The contents of this book are focused on dynamical models, and as such, they are
 not inteded to cover all aspects of complex systems science. There are many
 important topics that are not included in the text because of the limitation of
 space and time. For example, topics that involve probability, stochasticity,
 and statistics, such as information theory, entropy, complexity measurement,
 stochastic models, statistical analysis, and machine learning, are not
 discussed much in this book.

## I. Preliminaries

### 1. Introduction

These are two core concepts that go across almost all subareas of complex
 systems: emergence and self-organization. While emergence is about scale,
 self-organization is about time (in addition to scale). Self-organization is a
 dynamic process that looks as if it were going against the second law of
 thermodynamics.<br>
[organizational map of complex systems science][organizational_map_of_complex_systems_science]<br>
[Human Connectome Project][human_connectome_project]

[organizational_map_of_complex_systems_science]: https://en.wikipedia.org/wiki/File:Complex_systems_organizational_map.jpg
[human_connectome_project]: http://www.humanconnectomeproject.org/

### 2. Fundamentals of Modeling

All the "laws of nature" are no more than well-tested hypotheses at best. In
 this sense, all science can do is just build *models* of nature.<br>
NetLogo, Repast, MASON, [Golly][golly], C, C++, Java, Python, R, Mathematica,
 MATLAB, PyCX (Python 2.7)<br>
A good model is simple, valid, and robust.

[golly]: http://golly.sourceforge.net/

## II. Systems with a Small Number of Variables

### 3. Basics of Dynamical Systems

A phase space of a dynamical system is a theoretical space where every state of
 the system is mapped to a unique spatial location. The number of state
 variables needed to uniquely specify the system's state is called the *degrees
 of freedom* in the system. Therefore, the degrees of freedom of a system equal
 the dimensions of its phase space.<br>
Such a converging point or a region is called an *attractor*. The concept of
 attractors is particularly important for understanding the self-organization of
 complex systems. For each attractor, you can find the set of all the initial
 states from which you will eventually end up falling into that attractor. This
 set is called the *basin of attraction* of that attractor.

### 4. Discrete-Time Models I: Modeling

Autonomous system: A dynamic equation whose rules don't explicitly include time
 *t* or any other external variables. Non-autonomous, higher-order difference
 equations can always be converted into autonomous, first-order forms, by
 introducing additional state variables. Autonomous first-order equations can
 cover all the dynamics of any non-autonomous, higher-order equations. This is
 probably why autonomous first-order difference equations are called by a
 particular name: *iterative maps*.<br>
Linear equations are always analytically solvable, while nonlinear equations
 don't have analytical solutions in general. Obtaining a closed-form solution
 (analytical solution) is helpful because it gives you a way to calculate the
 system's state directly from *t* at any point in time in the future, without
 actually simulating the whole history of its behavior.<br>
matplotlib (pylab)<br>
Linear dynamical systems can show only exponential growth/decay, periodic
 oscillation, stationary states, or their hybrids. (Sometimes they can also show
 behaviors that are represented by polynomials (or products of polynomials and
 exponentials) of time. This occurs when their coefficient matrices are
 *non-diagonalizable*.<br>
*x*<sub>*t*<sub> = *x*<sub>*t*-1<sub> + *rx*<sub>*t*-1<sub>(1 -
 *x*<sub>*t*-1<sub>/*K*) is called the *logistic growth* model in mathematical
 biology and several other disciplines.<br>
*causal loop diagram* in *System Dynamics*<br>
The model we have created above is actually a variation of the *Lotka-Volterra
 model*, which describes various forms of predator-prey interactions. The
 Lotka-Volterra model is probably one of the most famous mathematical models of
 nonlinear dynamic systems that involve multiple variables.

### 5. Discrete-Time Models II: Analysis

When you analyze an autonomous, first-order discrete-time dynamic system (a.k.a.
 iterative map), one of the first things you should do is to find its
 *equilibrium points* (also called fixed points or steady states).<br>
One possible way to solve the overcrowded phase space of a discrete-time system
 is to create two phase spaces, one for time *t* - 1 and another for *t*, and
 then draw trajectories of the system's state in a meta-phase space that is
 obtained by placing those two phase spaces orthogonally to each other. A
 *cobweb plot* plays an important role as an intuitive analytical tool to
 understand the nonlinear dynamics of one-dimensional systems.

```python
from mpl_toolkits.mplot3d import Axes3D
ax = gca(projection='3d')
ax.plot(xresult, yresult, zresult, 'b')

import networkx as nx # NetworkX
g = nx.DiGraph()
g.add_edge((x, y), ((x * y) % 6, x))
ccs = [cs for cc in nx.connected_components(g.to_undirected())]
for i in xrange(len(ccs)):
    subplot(h, w, i + 1)
    nx.draw(nx.subgraph(g, ccs[i]), with_labels = True)
```

You should try variable rescaling to eliminate as many parameters as possible
 from your model before conducting a mathematical analysis. The logistic map,
 *x*<sub>*t*</sub> = *rx*<sub>*t*-1</sub>(1 - *x*<sub>*t*-1</sub>), is the
 simplified version of the logistic growth model. It is arguably the most
 extensively studied 1-D nonlinear iterative map.<br>
This assumption doesn't apply to *defective* (non-diagonalizable) matrices that
 don't have *n* linearly independent eigenvectors. However, such cases are
 rather rare in real-world applications, because any arbitarily small
 perturbations added to a defective matrix would make it diagonalizable.
 Problems with such sensitive, ill-behaving properties are sometimes called
 *pathological* in mathematics and physics.<br>
The dominant eigenvalue tells us the asymptotic ratio of magnitudes of the state
 vectors between two consecutive time points. After a long period of time, the
 system's state (*x*<sub>*t*</sub>, *y*<sub>*t*</sub>) will be proportional to
 the dominant eigenvector, *regardless of its initial state*. Dynamics of a
 linear system are *decomposable* into multiple independent one-dimensional
 exponential dynamics, each of which takes place along the direction given by an
 eigenvector. If a linear system's coefficient (in these cases, asymmetric)
 matrix has complex conjugate eigenvalues, the system's state is rotating around
 the origin its phase space (i.e., linear systems that show oscillatory
 behaviors). The absolute value of those complex conjugate eigenvalues still
 determines the stability of the system.<br>
(for nonlinear dynamical systems) *x*<sub>eq</sub> + Δ*x*<sub>*t*</sub> =
 *F*(*x*<sub>eq</sub> + Δ*x*<sub>*t*-1</sub>) ≈ *F*(*x*<sub>eq</sub>) +
 *F*′(*x*<sub>eq</sub>)Δ*x*<sub>*t*-1</sub><br>
Δ*x*<sub>*t*</sub> ≈ *J*Δ*x*<sub>*t*-1</sub><br>
If |λ<sub>*d*</sub>| = 1, it indicates that the system may be *neutral* (also
 called *Lyapunov stable*). But actually, proving that the point is truly
 neutral requires more advanced nonlinear analysis, which is beyond the scope of
 this textbook. When the eigenvalues are complex conjugates, equilibrium points
 are called a stable or unstable *spiral focus* or a *neutral center*.

### 6. Continuous-Time Models I: Modeling

*F*(*x*) ⇔ *x* + *G*(*x*)Δ*t*, *A* ⇔ *I* + *B*Δ*t*<br>
Simulation of a continuous-time model is equivalent to the *numerical
 integration* of differential equations, which, by itself, is a major research
 area in applied mathematics and computational science with more than a century
 of history. Here we focus on the simplest possible method for simulating a
 continuous-time model, by using the *Euler forward method*. There are many
 other more sophisticated methods for the numerical integration of differential
 equations, such as the backward Euler method, Heun's method, the Runge-Kutta
 methods, etc.

### 7. Continuous-Time Models II: Analysis

*Susceptible-Infected-Recovered (SIR) model* (a mathematical model of
 epidemiological dynamics) *dS*/*dt* = -*aSI*, *dI*/*dt* = *aSI* - *bI*,
 *dR*/*dt* = *bI*<br>
Python's `matplotlib` has a specialized function called `streamplot`, which is
 precisely designed for drawing phase spaces of continuous-time models.

```python
xvalues, yvalues = meshgrid(arange(0, 3, 0.1), arange(0, 3, 0.1))
xdot = xvalues - xvalues * yvalues
ydot = - yvalues + xvalues * yvalues
streamplot(xvalues, yvalues, xdot, ydot)
```

A typical starting point to do so is to find the *nullclines* in a phase space.
 A nullcline is a set of points where at least one of the time derivatives of
 the state variables becomes zero. These nullclines serve as "walls" that
 separate the phase space into multiple contiguous regions. Inside each region,
 the signs of the time derivatives never change, so just sampling one point in
 each region gives you a rough picture of how the phase space looks.

### 8. Bifurcations

### 9. Chaos

## III. Systems with a Large Number of Variables

### 10. Interactive Simulation of Complex Systems

### 11. Cellular Automata I: Modeling

### 12. Cellular Automata II: Analysis

### 13. Continuous Field Models I: Modeling

### 14. Continuous Field Models II: Analysis

### 15. Basics of Networks

### 16. Dynamical Networks I: Modeling

### 17. Dynamical Networks II: Analysis of Network Topologies

### 18. Dynamical Networks III: Analysis of Network Dynamics

### 19. Agent-Based Models

