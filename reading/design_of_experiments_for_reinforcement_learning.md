# [Design of Experiments for Reinforcement Learning][homepage] by Christopher Gatti, Springer (2015)

[Springer Theses][springer_theses]-the "best of the best", Recognizing
 Outstanding Ph.D. Research 

[springer_theses]: http://www.springer.com/series/8790

## 1. Introduction

One of the fundamental limitations of applying RL to more challenging and
 significant applications is our lack of knowledge regarding the behavior and
 performance of the current methods, including the learning algorithms and the
 fundamental representation of the knowledge.<br>
This dissertation explores RL using a design of experiments (DoE) approach in
 order to understand how RL algorithms perform under a variety of conditions. We
 are primarily interested in how learning algorithm and representation
 parameters affect learning algorithm we focus on is the temporal difference
 algorithm TD(λ), and the form of knowledge representation is a neural
 network.<br>
In this work, we use two types of experimentation, each of which serves a
 specific purpose. The use of the temporal difference algorithm with neural
 networks may not always converge. Consequently, finding combinations of
 parameters of the learning algorithm and representation that allow for learning
 convergence is a requisite first task to a successful RL implementation. We
 develop a novel sequential experimentation procedure that is able to find small
 subregions of the parameter space in which RL converges with a high
 probability. After finding convergent parameter subregions, we use a second
 experiment to explore these regions by creating metamodels, or surrogate
 models, of the performance of RL with respect to the learning algorithm and
 representation parameters. These metamodels allow for further exploration of
 the response surface, such as identifying the most influential parameters of
 the learning algorithm and representation, thus providing us with a better
 understanding of what affects this learning method.<br>
In fact, we consider this work to be *basic science*, if you will, of RL.

## 2. Reinforcement Learning

## 3. Design of Experiments

classical DoE - Screening Experimentss, Full Factorial Designs, Fractional
 Factorial Designs, Analysis of Variance (ANOVA), Response Surface Methods
 (RSM) (central composite designs (CCD), Box-Behnken designs (BBD)), Taguchi
 Methods, Sequential Experimentation<br>
contemporary DoE - Metamodeling (surrogate modeling), Experimental Designs of
 Metamodels (Number-Theoretic Methods (NTM), Statistical Form of Metamodels
 (classification and regression trees (CART), random forests, multi-variate
 adaptive regression splines (MARS), artificial neural networks, Kriging
 (spatial correlation modeling), Sequential Experimentation in Computer
 Experiments (variograms)

## 4. Methodology

## 5. The Mountain Car Problem

## 6. The Truck Backer-upper Problem

## 7. The Tandem Truck Backer-Upper Problem

## 8. Discussion

## A. Parameter Effects in the Game of Chung Toi

## B. Design of Experiments for the Moutain Car Problem

## C. Supporting Tables

