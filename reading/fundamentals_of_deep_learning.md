# [Fundamentals of Deep Learning][homepage] by Nikhil Buduma, O'Reilly (2017)

[source code][source_code]

[homepage]: http://shop.oreilly.com/product/0636920039709.do
[source_code]: https://github.com/darksigma/Fundamentals-of-Deep-Learning-Book

## 1. The Neural Network

In fact, the human cerebral cortex (the structure responsible for most of human
 intelligence) is made up of six layers.<br>
These neural networks are called *feed-forward* networks.<br>
When S-shaped nonlinearities are used, the tanh neuron is often preferred over
 the sigmoid neuron because it is zero-centered.

## 2. Training Feed-Forward Neural Networks

Linear neurons aren't used very much in practice because they're constrained in
 what they can learn. And the moment we start using nonlinear neurons like the
 sigmoidal, tanh, or ReLU neurons, we can no longer set up a system of
 equations! Clearly we need a better strategy to tackle the training process.
 This algorithm is known as *gradient descent*.<br>
In addition to the weight parameters defined in our neural network, learning
 algorithms also require a couple of additional parameters to carry out the
 training process. One of these so-called *hyperparameters* is the *learning
 rate*.<br>
The idea behind *batch gradient descent* is that we use our entire dataset to
 compute the error surface and then follow the gradient to take the path of
 steepest descent. The error surface, however, has a flat region (also known as
 saddle point in high-dimensional spaces), and if we get unluckly, we might find
 ourselves getting stuck while performing gradient descent. Another potential
 approach is *stochastic gradient descent* (SGD), where at each iteration, our
 error surface is estimated only with respect to a single example. One way to
 combat this problem is using *mini-batch gradient descent*. This subset is
 called a *minibatch*, and in addition to the learning rate, minibatch size is
 another hyperparameter. Minibatches strike a balance between the efficiency of
 batch gradient descent and the local-minima avoidance afforded by stochastic
 gradient descent.<br>
[ConvNetJS][convnetjs]

Overfitting becomes an even more significant issue in deep learning, where our
 neural networks have large numbers of layers containing many neurons. The
 number of connections in these models is astronomical, reaching the millions.
 As a result, overfitting is commonplace. The machine learning engineer is
 always working with a direct trade-off between overfitting and model
 complexity.<br>
We divide our training process into *epochs*. An epoch is a single iteration
 over the entire training set. At the end of an epoch, the validation set will
 tell us how the model does on data it has yet to see. If the accuracy on the
 training set continues to increase while the accuracy on the validation set
 stays the same (or decreases), it's a good sign that it's time to stop training
 because we're overfitting.<br>
The validation set is also helpful as a proxy measure of accuracy during the
 process of *hyperparameter optimization*. One potential way to find the optimal
 setting of hyperparameters is by applying a *grid search*, where we pick a
 value for each hyperparameter from a finite set of options, and train the model
 with every possible permutation of hyperparameter choices. We elect the
 combination of hyperparameters with the best performance on the validation set,
 and report the accuracy of the model trained with best combination on the test
 set.

One method of combatting overfitting is called *regularization*. We change the
 objective function so that it becomes *Error* + *λf*(*θ*), where *f*(*θ*) grows
 larger as the components of *θ* grow larger, and *λ* is the regularization
 strength (another hyperparameter).<br>
The most common type of regularization in machine learning is *L2
 regularization*. For ever wight *w* in the neural network, we add
 ½*λw*<sup>2</sup> to the error function. Tis has the appealing property of
 encouraging the network to use all of tis inputs a little rather than using
 only some of its inputs a lot. Of particular note is that during the gradient
 descent update, using the L2 regularization ultimately means that every weight
 is decayed linearly to zero. Because of this phenomenon, L2 regularization is
 also commonly referred to as *weight decay*.<br>
Another common type of regularization is *L1 regularization*. Here, we add the
 term *λ*|*w*| for every weight *w* in the neural network. The L1 regularization
 has the intriguing property that it leads the weight vectors to become sparse
 during optimization (i.e., very close to exactly zero). In other words, neurons
 with L1 regularization end up using only a small subset of their most important
 inputs and become quite resistant to noise in the inputs. L1 regularization is
 very useful when you want to understand exactly which features are contributing
 to a decision. If thie level of feature analysis isn't necessary, we prefer to
 use L2 regularization because it empirically performs better.
*Max norm constraints* enforce an absolute upper bound on the magnitude of the
 incoming weight vector for every neuron and use projected gradient descent to
 enforce the constraint. In other words, any time a gradient descent step moves
 the incoming weight vector such that ‖*w*‖<sub>2</sub>>*c*, we project the
 vector back onto the ball (centered at the origin) with radius *c*. Typical
 values of *c* are 3 and 4.<br>
*Dropout* has become one of the most favored methods of preventing overfitting
 in deep neural networks. While training, dropout is implemented by only keeping
 a neuron active with some probability *p* (a hyperparameter), or setting it to
 zero otherwise. Intuitively, this forces the network to be accurate even in the
 absence of certain information. It prevents the network from becoming too
 dependent on any one (or any small combination) of neurons. Expressed more
 mathematically, it prevents overfitting by providing a way of approximately
 combining exponentially many different neural network architectures
 efficiently. Test-time performance is extremely critical to model evaluation,
 so it's always preferable to use *inverted dropout*, where the scaling occurs
 at training time instead of at test time. In inverted dropout, any neuron whose
 actiation hasn't been silenced has its output divided by *p* before the value
 is propagated to the next layer.

[convnetjs]: http://cs.stanford.edu/people/karpathy/convnetjs/

## 3. Implementing Neural Networks in TensorFlow

We can do this by running `tf.global_variables_initializer`
 ~~`tf.initialize_all_variables()`~~, which will trigger all of the `tf.assign`
 operations in our graph.<br>
An operation consists of one or more *kernels*, which represent device-specific
 implementations.<br>
Just as variables need to be initialized the first time the computation graph is
 built, placeholders need to be filled envery time the computation graph (or a
 subgraph) is run.<br>
By default, sharing is not allowed (just to be safe!,
 `tf.variable_scope("shared_variables").reuse_variables()`).<br>
`with tf.device('/gpu:2'):`,
 `tf.Session(config=tf.ConfigProto(allow_soft_placement=True, log_device_placement=True))`<br>
`tf.scalar_summary`, `tf.histogram_summary` → `tf.merge_all_summaries` →
 `tf.train.SummaryWriter`<br>
We also save the model parameters using the `tf.train.Saver` model. By default,
 the saver maintains the latest five checkpoints.<br>
The performance of deep neural networks very much depends on an effective
 initialization of its parameters. For ReLU units, a study published in 2015 by
 He et al. demonstrates that the variance of weights in a network should be
 2/*n*<sub>in</sub>, where *n*<sub>in</sub> is the number inputs coming into the
 neurons. Finally, for slightly better performance, we perform the softmax while
 computng the loss instead of during the inference phase of the network
 (`tf.nn.softmax_cross_entropy_with_logits`).

## 4. Beyond Gradient Descent

The first source of local minima is tied to a concept commonly referred to as
 *model identifiability*. Instead, local minima are only problematic when they
 are *spurious*. A spurious local minimum corresponds to a configuration of
 weights in a neural network that incurs a higher error than the configuration
 at the global minimum. If these kinds of local minima are common, we quickly
 run into significant problems while using gradient-based optimization methods
 because we can only take into account local structure. It seems that the true
 struggle of gradient descent isn't the existence of troublesome local minima,
 but instead, is that we have a tough time finding the appropriate direction to
 move in.<br>
More generally, given an arbitary function, a point at which the gradient is the
 zero vector is called a *critical point*. Critical points come in various
 flavors (local minima, local maxima, saddle points). It turns out that as our
 function has more and more dimensions (i.e., we have more and more parameters
 in our model), saddle points are exponentially more likely than local
 minima.

We realize that only when the contours are perfectly circular does the gradient
 always point in the direction of the local minimum. However, if the contours
 are extremely elliptical (as is usually the case for the error surfaces of deep
 networks), the gradient can be as inaccurate as 90 degrees away from the
 correct direction! More generally, we can quantify how the gradient changes
 under our feet as we move in a certain direction by computing second
 derivatives. We can compile this information into a special matrix known as the
 *Hessian matrix* (*H*). And when describing an error surface where the gradient
 changes underneath our feet as we move in the direction of steepest descent,
 this matrix is said to be *ill-conditioned*. It turns out, however, that
 computing the Hessian matrix exactly is a difficult task. We'll describe
 optimization breakthroughs that tackle ill-conditioning without directly
 computing the Hessian matrix.<br>
We can do this by keeping track of an *exponentially weighted decay* of past
 gradients. In other words, we use the momentum hyperparameter *m* to determine
 what fraction of the previous velocity to retain in the new update, and add
 this "memory" of past gradients to our current gradient. This approach is
 commonly referred to as *momentum*. Because the momentum term increases the
 stpe size we take, using momentum may require a reduced learning rate compared
 to vanilla stochastic gradient descent. Recently, mroe work has been done
 exploring how the classical momentum technique can be improved. Sutskever et
 al. in 2013 proposed an alternative called Nesterov momentum, which computes
 the gradient on error surface at *θ* + *v*<sub>*i*-1</sub> during the velocity
 update instead of at *θ*. This subtle difference seems to allow Nesterov
 momentum to change its velocity in a more responsive way. Support for Nerestov
 momentum is not yet available out of the box in TensorFlow as of the writing of
 this text.<br>
The first is conjugate gradient descent. The conjugate direction is chosen by
 using an indirect approximation of the Hessian to linearly combine the gradient
 and our previous direction. An alternative optimization algorithm known as the
 *Broyden-Fletcher-Goldfarb-Shanno (BFGS) algorithm* attempts to compute the
 inverse of the Hessian matrix iteratively and use the inverse Hessian to more
 effectively optimize the parameter vector. In its original form, BFGS has a
 significant memory footprint, but recent work has produced a more
 memory-efficient version known as *L-BFGS*. TensorFlow does not currently
 support these methods at the time of writing this text, although these features
 seem to be in the development pipeline.

One of the major breakthroughs in modern deep network optimization was the advent of learning rate adaption.<br>
The first algorithm we'll discuss is AdaGrad, which attempts to adapt the global
 learning rate over time using an accumulation of the historical gradients. This
 learning rate is inversely scaled with respect to the square root of the sum of
 the squares (root mean square) of all the parameter's historical gradients.
 Flat regions may force AdaGrad to decrease the learning rate before it reaches
 a minimum.<br>
Compared to naive accumulation, exponentially weighted moving averages also
 enable us to "toss out" measurements that we made a long time ago. Plugging
 this modification into AdaGrad gives rise to the RMSProp learning algorithm. As
 the template suggests, we can utilize RMSProp with momentum (specifically
 Nerestov momentum). Overall, RMSProp has been shown to be a highly effective
 optimizer for deep neural networks, and is a default choice for many seasoned
 practitioners.<br>
Recently, Adam has gained popularity because of its corrective measures against
 the zero initilization bias (a weakness of RMSProp) and its ability to combine
 the core concepts behind RMSProp with momentum more effectively. The default
 hyperparameter settings for Adam for TensorFlow generally perform quite well,
 but Adam is also generally robust to choices in hyperparameters. The only
 exception is that the learning rate may beed to be modified in certain cases
 from the default value of 0.001.<br>
One important point, however, is that for most deep learning practitioners, the
 best way to push the cutting edge of deep learning is not by building more
 advanced optimizers. Instead, the vast majority of breakthroughs in deep
 learning over the past several decades have been obtained by discovering
 architectures that are easier to train instead of trying to wrangle with nasty
 error surfaces.
 
## 5. Convolutional Neural Networks

This result is our *feature map*, and it indicates where we've found the feature
 we're lookinf for in the original image. This operation is called a
 convolution.<br>
Generally, it's wise to keep filter sizes small (size 3 x 3 or 5 x 5). Less
 commonly, larger sizes are used (7 x 7) but only in the first convolutional
 layer. Having more small filters is an easy way to achieve high
 representational powere while also incurring a smaller number of
 parameters.<br>
Setting the `padding` argument to `"SAME"` also selects the zero padding so that
 height and width are preserved by the convolutional layer.<br>
To aggressively reduce dimensionality of feature maps and sharpen the located
 features, we sometimes insert a *max pooling* layer after a convolutional
 layer. One interesting property of max pooling is that it is *locally
 invariant*. However, enforcing large amounts of local invariance can destroy
 our network's ability to carry important information. As a result, we usually
 keep the spatial extent of our pooling layers quite small. In fractional max
 polling, a pseudorandom number generator is used to generate tilings with
 noninteger lengths for pooling. Here, fractional max pooling functions as a
 strong regularizer, helping prevent overfitting in convolutional networks.
 Pooling operations are inherently destructive.<br>
As a practical note, deep convolutional networks can take up a significant
 amount of space, and most casual practitioners are usually bottlenecked by the
 memory capacity on their GPU. The VGGNet architecture, for example, takes
 approximately 90 MB of memory on the forward pass per image and more than 180
 MB of memory on the backward pass to update the parameters. Many deep networks
 make a compromise by using strides and spatial extents in the first
 convolutional layer that reduce the amount of information that needs to be
 propagated up the network.

`tf.image.per_image_whitening()` (subtracting out the mean and normalizing to
 unit 1 variance)<br>
Normalization of image inputs helps out the training process by making it more
 robust to variations. Batch normalization takes this a step further by
 normalizing inputs to every layer in our neural network. In addition to
 speeding up training by preventing significant shifts in the distribution of
 inputs to each layer, batch normalization also allows us to significantly
 increase the learning rate. Moreover, batch normalization acts as a regularizer
 and removes the need for dropout and (when used) L2 regularization. Although we
 don't leverage it here, the authors also claim that batch regularization
 largely removes the need for photometric distortions, and we can expose the
 network to more "real" images during the training process.

```python
def conv_batch_norm(x, n_out, phase_train):
    beta_init = tf.constant_initializer(value=0.0, dtype=tf.float32)
    gamma_init = tf.constant_initializer(value=1.0, dtype=tf.float32)
    beta = tf.get_variable("beta", [n_out], initializer=beta_init)
    gamma = tf.get_variable("gamma", [n_out], initilizer=gamma_init)

    batch_mean, batch_var = tf.nn.moments(x, [0,1,2], name='moments')
    ema = tf.train.ExponentialMovingAverage(decay=0.9)
    ema_apply_op = ema.apply([batch_mean, batch_var])
    ema_mean, ema_var = ema.average(batch_mean), ema.average(batch_var)
    def mean_var_with_update():
        with tf.control_dependencies([ema_apply_op]):
            return tf.identity(batch_mean), tf.identity(batch_var)
    mean, var = control_flow_ops.cond(phase_train,
        mean_var_with_update,
        lambda: (ema_mean, ema_var))

    # x_r = tf.reshape(x, [-1, 1, 1, n_out])
    normed = tf.nn.batch_norm_with_global_normalization(x,
             mean, var, beta, gamma, 1e-3, True)
    return normed
    # return tf.reshape(normed, [-1, n_out])
```

CIFAR-10 challenge (32 x 32 color images, 10 possible classes)<br>
t-Distirubted Stochastic Neighbor Embedding ([t-SNE][t_sne]) to compress it to a
 two-dimensional representation that we can [visualize][t_sne_visualization]<br>
neural style - compute the Gram matrices (which represents correlations between
 feature maps in a given layer) for the artiwork (*A*<sup>(*l*)</sup>) and the
 generated image (*G*<sup>(*l*)</sup>) to represent the error function<br>
Using five-dimensional tensors (including time as a dimension) and applying
 three-dimensional convolutions is an easy way to extend the convolutional
 paradigm to video. Convolutional filters have also been successfully used to
 analyze audiograms. Less intuitively, convolutional networks have also found
 some use in natural language processing. More exotic uses of convolutional
 networks include teach algorithms to play board games, and analyzing biological
 molecules for drug discovery.

[t_sne]: https://lvdmaaten.github.io/tsne/
[t_sne_visualization]: http://cs.stanford.edu/people/karpathy/cnnembed/

## 6. Embedding and Representation Learning

Our goal in this chapter will be to develop effective learning models in
 situations where labeled data is scarce but wild, unlabeled data is plentiful.
 We'll approach this problem by learning *embeddings*, or low-dimensional
 representations, in an unsupervised fashion. We'll also explore other
 applications of learning lower-dimensional representations, such as
 visualization and semantic hashing. We'll start by considering situations where
 all of the important information is already contained within the original input
 vector itself. In this case, learning embeddings is equivalent to developing an
 effective compression algorithm.<br>
autoencoder - input → encoder → embedding (or code) → decoder → reconstructed
 input<br>
*Denoising* improves the ability of the autoencoder to generate embeddings that
 are resistant to noise. This underlying, unifying geometric structure is known
 as a *manifold*. The manifold is the shape that we want to capture when we
 reduce the dimensionality of our data. The denoising objective enable our model
 to learn the manifold by learning to map corrupted data to uncorrupted data by
 minimizing the error between their representations.<br>
While deep models are generally more accurate, a lack of interpretability often
 hinders their adoption in highly valuable, but highly risky, applications. We
 can address one aspect of interpretability by exploring the characteristics of
 the output of an autoencoder. In general, an autoencoder's representations are
 dense, and this has implications with respect to how the representation changes
 as we make coherent modifications to the input. The ideal outcome for us is if
 we can build a representation where there is a 1-to-1 correspondence, or close
 to a 1-to-1 correspondence, between high-level features and individual
 components in the code. This is very similar to the rationale behine using
 regularization to prevent overfitting in simple neural networks. We'll achieve
 this by modifying the objective function with a sparsity penalty, which
 increases the cost of any representation that has a large number of nonzero
 components. A measure that is often used to this end is the Kullback-Leibler
 (often referred to as KL) divergence. More recently, the theoretical properties
 and empirical effectiveness of introducing an intermediate function before the
 code layer that zeroes out all but *k* of the maximum activations in the
 representation were investigated by Makhzani and Frey (2014). These *k-Sparse
 autoencoders* were shown to be just as effective as other mechanisms of
 sparsity despite being shockingly simple to implement and understand (as well
 as computationally more efficient).<br>
In other situations, we have input representations that say very little at all
 about the content that we are trying to capture. In these situations, our goal
 is not to extract information, but rather, to gather information from context
 to build useful representations. We can identify words with similar meanings
 based on their contexts. It turns out we can use the same principles we used
 when building the autoencoder to build a network that builds strong,
 distributed representations (word ↔ context).<br>
The first flavor of Word2Vec, a framework for generating word embeddings, was
 the *Continuous Bag of Words* (CBOW) model. The second flavor of Word2Vec is
 the *Skip-Gram model*. The Skip-Gram model does the inverse of CBOW, taking the
 target word as an input, and then attempting to predict one of the words in the
 context. The decoder is slightly trickier because we make some modifications
 for performance. To reduce the number of parameters, Mikolov et al. used a
 strategy for implementing the decoder known as noise-contrastive estimation
 (NCE). While Word2Vec is admittedly not a deep machine learning model, we
 discuss it here for many reasons. First, it thematically represents a strategy
 (finding embeddings using context) that generlizes to many deep learning
 models. Moreover, we'll find that using Word2Vec embeddings instead of one-hot
 vectors to represent words will yield far superior results. (This will allow us
 to reduce the number of parameters in our model and make learning faster.)

## 7. Models for Sequence Analysis

by Surya Bhupatiraju

The goal of *seq2seq* is to transform an input sequence into a corresponding
 output sequence. Other famous seq2seq problems include translating text between
 languages, text summarization, and transcribing speech to text.<br>
We'll refer to this subsequence as the *context window*.<br>
To make the problem more approachable, we instead reconsider the dependency
 parsing task as finding a sequence of valid "actions" that generates the
 correct dependency parse. This technique is known as the *arc-standard* system.
 We start by putting the first two words of the sentence in the stack and
 maintaining the remaining words in the buffer. Taken together, these ideas form
 the core for Google's SyntaxNet, the state-of-the-art open source
 implementation for dependency parsing. We refer the inspired reader to
 [the open source repository][syntaxnet_repository], which contains an
 implementation of Parsey McParseface, the most accurate publicly reported
 English language parser as of the publication of this text.<br>
The strategy was purely *greedy*, that is, we selected prediction with the
 highest probability without being concerned that we might potentially paint
 ourselves into a corner by making an early mistake. To remedy this shortcoming,
 we utilize a strategy known as *beam search*. The basic idea behind beam search
 is that instead of greedily selecting the most probable prediction at each
 step, we maintain a *beam* of the most likely hypothesis (up to a fixed *beam
 size* *b*) for the sequence of the first *k* actions and their associated
 probabilities. Beam searching can be broken up into two major phases: expansion
 and pruning. As Andor et al. describe in 2016, this process of *global
 normalization* provides both strong theoretical gurantees and clear performance
 gains relative to *local normalization* in practice. In a globally normalized
 network, instead of putting the scores through a softmax to generate a
 per-action probability distribution, we instead add up all the scores for a
 hypothesis action sequence.

But even still, the problem space was constrained to situations in which there
 was a one-to-one mapping between elements in the input sequence to elements in
 the output sequence. We want our model to maintain some sort of memory over the
 span of reading the input sequence. By the time it has reached the end of input
 sequence, the internal memory contain a "thought" that represents the key
 pieces of information, that is, the meaning, of the original input. We should
 then be able to use this thought vector to either produce a label for the
 original sequence or produce an appropriate output sequence (translation,
 description, abstractive summary, etc.).<br>
RNNs are different from feed-forward networks because they leverage a special
 type of neural layer, known as recurrent layers, that enable the network to
 maintain state between uses of the network. Unlike a feed-forward layer,
 recurrent layers also have recurrent connections, which propagate information
 between neurons of the same layer. A fully connected recurrent layer has
 information flow from every neuron to every other neuron in its layer
 (including itself). We can also now train the RNN by computing the gradient
 based on the unrolled version. We do run into one issue, however. In our
 unrolled network, we have sets of connections that all correspond to the same
 connection in the original RNN. We can circumvent this issue by averaging or
 summing the error derivatives over all the connections that belong to the same
 set.<br>
This issue is commonly referred to as the problem of *vanishing gradients*, and
 it severely impacts the learning capabilities of vanilla recurrent neural
 networks. In order to combat the problem of vanishing gradients, Sepp
 Hochreiter and Jürgen Schmidhuber introduced the *long short-term memory*
 (LSTM) architecture. The basic principle behind the architecture was that the
 network would be designed for the purpose of reliably transmitting important
 information many time steps into the future. One of the core components of the
 LSTM architecture is the *memory cell*. At every time step, the LSTM unit
 modifies the memory cell with new information with three different phases
 (keep gate, write gate and output gate).<br>
First, we have `tf.RNNCell` objects that represent either an RNN layer or an
 LSTM unit. The `tf.nn.rnn_cell.BasicRNNCell` abstraction represents a vanilla
 recurrent neuron layer. The `tf.nn.rnn_cell.BasicLSTMCell` represents a simple
 implementation of the LSTM unit, and the `tf.nn.rnn_cell.LSTMCell` represents
 an implementation with more configuration options (peephole structures,
 clipping of state values, etc.). The TensorFlow library also includes a
 variation of the LSTM unit known as the *Gated Recurrent Unit*, proposed in
 2014 by Yoshua Bengio's group. In addition to the primitives, there are several
 wrappers to add to our arsenal (`tf.nn.rnn_cell.MultiRNNCell` and
 `tf.nn.rnn_cell.DropoutWrapper`). Finally, we complete the RNN by wrapping
 everything into the appropriate TensorFlow RNN primitive:
 `outputs, state = tf.nn.dynamic_rnn(...)`<br>
But there are several seq2seq problems, such as translating between languages or
 creating a summary for a video, where long-term dependencies are crucial to the
 success of the model. The seq2seq model is composed of two separate networks.
 The first network is known as the *encoder* network. The goal of the encoder
 network is to generate a condensed understanding of the input and summarize it
 into a singular thought represented by the final state of the encoder network.
 Then we use a *decoder* network, whose starting state is initialized with the
 final state of the encoder network, to produce the target output sequence token
 by token. Kiros et al. in 2015 invented the notion of a *skip-thought vector*,
 which borrowed architectural characteristics from both the autoencoder
 framework and Skip-Gram model. The skip-thought vector was generated by
 dividing up a passage into a set of triplets consisting of consecutive
 sentences. The authors utilized a single encoder network and two decoder
 networks.<br>
This phenomenon of *attention* has yet to be captured by our approach to
 seq2seq. One way to give the decoder network some vision into the original
 sentence is by giving the decoder access to all of the outputs from the encoder
 network. The key realization here is that it's not enough to merely give the
 decoder access to all the outputs. Instead of directly using the raw outputs
 from the encoder network, we perform a weighting operation on the encoder's
 outputs. Anytime there are particular parts of the input that are highly
 correlated to correctly producing corresponding segments of the output,
 attentions can dramatically improve performance.<br>

[Sequence-to-Sequence Models tutorial][sequence_to_sequence_models_tutorial]<br>
For instance, a word-level tokenizer will ensure that the model produces words
 that are from some dictionary, but the size of the dictionary may be too large
 to efficiently choose from during decoding. On the other hand, the decoder
 using a character-level tokenization may not produce intelligible outputs, but
 the total dictionary that the decoder must choose from is much smaller, as it
 is simply the set of all printable ASCII characters.<br>
Using bucketing shows a considerable speedup during training and test time, and
 allows developers and frameworks to write very optimized code to leverage the
 fact that any sequence from a bucket will have the same size and pack the data
 together in ways that allow even further GPU efficiency. The last improvement
 we make in the data preparation side is that we reverse the source sequences.
 Researchers found that doing so improved performance, and this has become a
 standard trick to try when training neural machine translation models. In many
 language pairs, the beginning of sentences is harder to translate than the end
 of sentences, so this hack of reversing the sentence improves translation
 accuracy by giving the beginning of the sentence the last say on what final
 state is encoded.

[syntaxnet_repository]: https://github.com/tensorflow/models/tree/master/syntaxnet
[sequence_to_sequence_models_tutorial]: https://www.tensorflow.org/tutorials/seq2seq

## 8. Memory Augmented Neural Networks

by Mostafa Samir

It's theoretically proven that the RNN architecture is a universal functional
 representer; a more precise statement of the same result is that RNN are
 *Turing complete*.

## 9. Deep Reinforcement Learning

