# [Deep Learning and the Game of Go][homepage] by Max Pumperla and Kevin Ferguson, Manning (2018?)

[BetaGo][betago], [source code][source_code]

[homepage]: https://www.manning.com/books/deep-learning-and-the-game-of-go
[betago]: https://github.com/maxpumperla/betago
[source_code]: https://github.com/maxpumperla/deep_learning_and_the_game_of_go

## Part 1: AI and Go

### 1. Toward deep learning: a machine learning introduction

We load the data in raw format and carry out some preprocessing steps to create
 features, i.e. input data that can be fed into a machine learning
 algorithm.<br>
Supervised learning is powerful, but finding quality training data can be a
 major obstacle. There are no examples to use as training data---our robot
 doesn't even exist yet. Instead, we can apply reinforcement learning, a sort of
 trial and error approach. That whole experience gives us a small chunk of
 training data, and we can use it to improve the control system. By repeating
 the whole process over and over, we can gradually home in on an efficient
 control function.<br>
Machine learning is a family of techniques for generating functions from data
 instead of writing them directly. You can use machine learning to solve
 problems that are too ambiguous to solve directly.

### 2. Go as a machine learning problem

Two eyes is the only strategy we will specifically code into our bot's logic.
 All the more advanced Go strategies will be inferred through machine
 learning.<br>
Territory scoring is more common in casual play, but it turns out that area
 scoring is slightly more convenient for computers. Komi is usually 6.5 under
 territory scoring or 7.5 points under area scoring---the extra half point
 ensures there are no ties.<br>
In the context of game tree search, the number of possible moves on a given turn
 is the branching factor.<br>
Two places we can apply deep learning in Go are move selection and position
 evaluation. Move selection is the problem of narrowing the set of moves we need
 to consider in a particular board position. Position evaluation is the problem
 of estimating which player is ahead and by how much.<br>
Open source Go engines such as GNU Go and Pachi provide good benchmarks. GNU Go
 plays at around a 5 kyu level, and Pachi is about 1 dan (Pachi's level varies a
 bit depend on how much computing power you provide it).

### 3. Implementing our first Go bot

Enums are part of the standard library in Python 3. If you are using Python 2,
 you can get a back-port from the `enum34` package.

## Part 2: Way to go

### 4. Playing games with tree search

### 5. Getting started with neural networks

(unavailable)

### 6. Enter deep learning

(unavailable)

### 7. Learning from data: deep learning bots

(unavailable)

### 8. Enter deep reinforcement learning

(unavailable)

### 9. Reinforcement learning with the policy gradient algorithm

(unavailable)

### 10. Reinforcement learning with value methods

(unavailable)

### 11. Reinforcement with actor-critic methods

(unavailable)

## Part 3: Bringing it all together

### 12. AlphaGo: Combining approaches

(unavailable)

### 13. Bots in the wild: deployment and scale-out

(unavailable)

## Appendixes

### A. Mathematical foundations with Python

(unavailable)

### B. The backpropagation algorithm

(unavailable)

### C. Sample games and resources

(unavailable)

### D. Go servers and data

(unavailable)

