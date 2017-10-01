# [Learning TensorFlow][homepage] by Tom Hope, Yehezkel S. Resheff and Itay Lieder, O'Reilly (2017)

[source code][source_code]

[homepage]: http://shop.oreilly.com/product/0636920063698.do
[source_code]: https://github.com/Hezi-Resheff/Oreilly-Learning-TensorFlow

## 1. Introduction

TensorFlow is, in fact, a second-generation system for implementing and
 deploying deep neural networks at Google, succeeding the DistBelief project
 that started in 2011. TensorFlow was released to the public as an open source
 framework with an Apache 2.0 license in November 2015.<br>
TensorFlow comes with abstraction libraries such as Keras and TF-Slim, offering
 simplified high-level access to TensorFlow. Additionally, as we will see
 further along in this book, TensorFlow comes with many more features aimed at
 boosting scalability. These include support for asynchronous computation with
 threading and queues, efficient I/O and data formats, and much more.

## 2. Go with the Flow: Up and Running with TensorFlow

[MNIST handwritten digit database][mnist_handwritten_digit_database]<br>
(as of TensorFlow 1.0) `tf.losses.softmax_cross_entropy`,
 `tf.metrics.accuracy`<br>
Supervised learning is usually subdivided into the case where labels are
 continuous (regression) or discrete (classification).

## 3. Understanding TensorFlow Basics

TensorFlow and NumPy are tightly coupled--for example, the output returned by
 `sess.run()` is a NumPy array.<br>
`tf.InteractiveSession()` allows you to replace the usual `tf.Session()`, so
 that you don't need a variable holding the session for running ops. This can be
 useful in interactive Python environments, like when writing IPython notebooks,
 for instance.<br>
Another very common loss, especially for categorical data, is the *cross
 entropy*. The more similar the two distributions, the smaller our cross entropy
 will be.<br>
Using smaller batches usually works faster, and the smaller the size of the
 batch, the faster are the calculations. However, there is a trade-off in that
 small samples lead to lower hardware utilization and tend to have high
 variance, causing large fluctuations to be objective function. Nevertheless, it
 turns out that some fluctuations are beneficial since they enable the set of
 parameters to jump to new and potentially better local minima. Using a
 relatively smaller batch size is therefore effective in that regard, and is
 currently overall the preferred approach.<br>
`np.random.binomial(1,y_data_pre_noise)`

```python
# the binary version of the cross entropy
#  == tf.nn.sigmoid_cross_entropy_with_logits(labels=,logits=)
y_pred = tf.sigmoid(y_pred)
loss = y_true*tf.log(y_pred) - (1-y_true)*tf.log(1-y_pred)
loss = tf.reduce_mean(loss)
```

[mnist_handwritten_digit_database]: http://yann.lecun.com/exdb/mnist/

## 4. Convolutional Neural Networks

There are motivations commonly cited as leading to the CNN approach, coming from
 different schools of thought. The first angle is the so-called neuroscientific
 inspiration behind the model. The second dels with insight into the nature of
 images, and the third relates to learning theory.

`tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')` (the size of the
 result of the operation is the same as the size of `x`.)<br>
`feature map` is simply a commonly used term referring to the outut of each such
 layer.<br>
The more theoretical reason for applying pooling is that we would like our
 computed features not to care about small changes in position in an image.<br>
`tf.nn.max_pool(x, ksize=[1, 2, 2, 1], strides=[1, 2, 2, 1], padding='SAME')`<br>
*Dropout* is a regularization trick used in order to force the network to
 distribute the learned representation across all the neurons (forcing the
 network to learn a representation that will work even after the dropout). This
 process is often thought of as traiing an "ensemble" of multiple networks.<br>
`tf.nn.dropout(layer, keep_prob=keep_prob)`<br>
Put simply, a bad initialization can make the training process "get stuck," or
 fail completely due to numerical issues. Using random rather than constant
 initializations helps break the symmetry between learned features, allowing the
 model to learn a diverse and rich representation. Using bound values helps,
 among other things, to control the magnitude of the gradients, allowing the
 network to converge more efficiently.<br>
In machine learning and especially in deep learning, an epoc refers to a single
 pass over all the training data.

[CIFAR10][cifar10], [who is the best in CIFAR-10 ?][who_is_the_best_in_cifar_10]

[cifar10]: https://www.cs.toronto.edu/~kriz/cifar.html
[who_is_the_best_in_cifar_10]: http://rodrigob.github.io/are_we_there_yet/build/classification_datasets_results.html#43494641522d3130

## 5. Text I: Working with Text and Sequences, and TensorBoard Visualization

A fundamental mathematical construct in statistics and probability, which is
 often used as a building block for modeling sequential patterns via machine
 learning is the Markov chain model. Figuratively speaking, we can view our data
 sequences as "chains," with eaach node in the chain dependent in some way on
 the previous node, so that "history" is not erased but carried on. This is very
 related to the Markov chain and their hidden Markov model (HMM) extensions,
 which are not discussed in this book.<br>
The update step for our simple vanilla RNN is *h*<sub>*t*</sub> =
 *tanh*(*W*<sub>x</sub>*x*<sub>*t*</sub> + *W*<sub>*h*</sub>*h*<sub>*t-1*</sub>
 + *b*)<br>
In a trend in cutting-edge deep learning research, advanced models attempt to
 exploit various kinds of sequential structures in images, trying to capture in
 some sense the "generative process" that created each image. Intuitively, this
 all comes down to the notion that nearby areas in images are somehow related,
 and trying to model this structure.<br>
`tf.scan()` was added to TensorFlow to allow us to introduce loops into the
 computation graph, instead of just "unrolling" the loops explicitly by adding
 more and more replications of the same operations.<br>
Once we have created the `rnn_cell` via `tf.contrib.rnn.BasicRNNCell()`, we feed
 it into `tf.nn.dynamic_rnn()`. This function replace `tf.scan()` in our vanilla
 implementation and creates an RNN specified by `rnn_cell`. Note that *contrib*
 refers to the fact that code in this library is contributed and still requires
 testing. `BasicRNNCell` was moved to `contrib` in TensorFlow 1.0 as part of
 ongoing development. In version 1.2, many of the RNN functions and classes were
 moved back to the core namespace with aliases kept in `contrib` for backward
 compatibility.<br>
As of this writing, in early 2017, TensorFlow includes a static and a dynamic
 function for creating an RNN. The static version creates an unrolled graph of
 fixed length. The dynamic version uses a `tf.While` loop to dynamically
 construct the graph at execution time, leading to faster graph creation, which
 can be significant.

A powerful approach to work around this issue is to use word embeddings. The
 embedding is, in a nutshell, simply a mapping from high-dimensional one-hot
 vectors encoding words to lower-dimensional dense vectors. We use the built-in
 `tf.nn.embedding_loopup()` function, which efficiently retrieves the vectors
 for each word in a given sequence of word indices.<br>
We create an LSTM cell with `tf.contrib.rnn.BasicLSTMCell()` and feed it to
 `tf.nn.dynamic_rnn()`. We also give `dynamic_rnn()` the length of each sequence
 in a batch of examples. We take the last valid output vector--in this case
 conveniently available for us in the `states` tensor returned by
 `dynamic_rnn()`--and pass it through a linear layer (and the softmax function),
 using it as our final prediction. Adding more layers is straightforward, using
 the `MultiRNNCell()` wrapper that combines multiple RNN cells into one
 multilayer cell.

## 6. Text II: Word Vectors, Advanced RNN, and Embedding Visualization

We thus would like a way to use this data to train word representations, in a
 unsupervised fashion. Whether old or new, these all rely at their core on the
 *distributional hypothesis*, which is most easily explained by a well-known
 quote by linguist John Firth: "You shall know a word by the company it keep."
 In other words, words that tend to appear in similar contexts tend to have
 similar semantic meanings.<br>
We focus on the most popular word2vec implementation, which trains a model that,
 given an input word, predicts the word's context by using something known as
 *skip-grams*. In addition to these pairs we extract from the data, we also
 sample "fake" pairs--that is, for a given input word (such as "AI"), we also
 sample random noise words as context (such as "monkeys"), in a process known as
 *negative sampling*. We use the true pairs combined with noise pairs to build
 our training instances and labels, which we use to train a binary classifier
 that learns to distinguish between them.

## 7. TensorFlow Abstractions and Simplifications

## 8. Queues, Threads, and Reading Data

## 9. Distributed TensorFlow

## 10. Exporting and Serving Models with TensorFlow

## A. Tips on Model Construction and Using TensorFlow Serving

