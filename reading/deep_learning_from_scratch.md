# [밑바닥부터 시작하는 딥러닝][homepage], 사이토 고키 저, 개앞맵시(이복연) 역, 한빛미디어 (2017)

[source code][source_code], [용어표][terms]

[개발자의 앞길에 맵핵 시전][translator_facebook],
 [스카이넷도 딥러닝부터][from_deep_learning_to_skynet] [마인드맵][mindmaps]<br>
이 책은 이미지 인식을 주제로 합니다. 자연어 처리, 음성 인식 등은 이 책의 대상이
 아닙니다.<br>
[CS231n: Convolutional Neural Networks for Visual Recognition][cs231n] (Stanford) ([notes][cs231n_notes])<br>
([텐서플로 첫걸음][first_contact_with_tensorflow] [유료 세미나][first_contact_with_tensorflow_seminar], [소스 코드][first_contact_with_tensorflow_source_code], [텐서플로 블로그][first_contact_with_tensorflow_blog])

[homepage]: http://www.hanbit.co.kr/store/books/look.php?p_code=B8475831198
[source_code]: https://github.com/WegraLee/deep-learning-from-scratch
[terms]: https://docs.google.com/spreadsheets/d/1ccwGiC01X-gs3PPcXPUz67W9rS6l994LD4AL18KF1_0/edit#gid=0
[translator_facebook]: https://www.facebook.com/dev.loadmap
[from_deep_learning_to_skynet]: https://www.mindmeister.com/ko/812276967/_
[mindmaps]: https://www.mindmeister.com/ko/users/channel/wegra
[cs231n]: http://cs231n.stanford.edu/
[cs231n_notes]: http://cs231n.github.io
[first_contact_with_tensorflow]: http://www.hanbit.co.kr/store/books/look.php?p_code=B3286570432
[first_contact_with_tensorflow_seminar]: http://www.hanbit.co.kr/store/education/edu_view.html?p_code=C8472219402
[first_contact_with_tensorflow_source_code]: https://github.com/rickiepark/ml-learn
[first_contact_with_tensorflow_blog]: https://tensorflow.blog

## 1. 헬로 파이썬

```python
>>> X = X.flatten()
>>> X[np.array([0, 2, 4])]
array([51, 14, 0])
>>> X > 15
array([ True, True, False, True, False, False], dtype=bool)
>>> X[X>15]
array([51, 55, 19])

import matplotlib.pyplot as plt
x = np.arrange(0, 6, 0.1)
y2 = np.cos(x)
plt.plot(x, t2, linestyle="--", label="cos")
plt.xlabel("x")
plt.ylabel("y")
plt.title('sin & cos')
plt.legend()
plt.show()

from matplotlib.image import imread
img = imread('lena.png')
plt.imshow(img)
plt.show()
```

[Scipy Lecture Notes][scipy_lecture_notes]

[scipy_lecture_notes]: http://www.scipy-lectures.org/

## 2. 퍼셉트론

앞 절에서 말한 퍼셉트론의 한계는 정확히 말하면 "단층 퍼셉트론 single-layer
 perceptron으로는 XOR 게이트를 표현할 수 없다" 또는 "단층 퍼셉트론으로는 비선형
 영역을 분리할 수 없다"가 됩니다. 사실 퍼셉트론의 아름다움은 '층을 쌓아' 다층
 퍼셉트론 multi-layer perceptron을 만들 수 있다는데 있습니다.<br>
[The Elements of Computing Systems: Building a Modern Computer from First Principles][elements_of_computing_systems]
 (The MIT Press, 2005, [companion][elements_of_computing_systems_companion],
 [old][elements_of_computing_systems_old])<br>
그 답은 "이론상 2층 퍼셉트론이면 컴퓨터를 만들 수 있다"입니다. 말도 안 되는 소리
 같지만, 2층 퍼셉트론, 정확히는 비선형인 시그모이드 함수를 활성화 함수로
 이용하면 임의의 함수를 표현할 수 있다는 사실이 증명되었습니다. 그러나 2층
 퍼셉트론 구조에서 가중치를 적절히 설정하여 컴퓨터를 만들기란 너무 어렵습니다.
 그래서 퍼셉트론으로 표현하는 컴퓨터도 여러 층을 다시 층층이 겹친 구조로 만드는
 방향이 자연스러운 흐름입니다.

[elements_of_computing_systems]: https://mitpress.mit.edu/books/elements-computing-systems
[elements_of_computing_systems_companion]: http://www.nand2tetris.org/course.php
[elements_of_computing_systems_old]: http://www1.idc.ac.il/tecs/plan.html

## 3. 신경망

입력층, 출력층, 은닉층<br>
활성화 함수 *h*() activation function - 계단 함수 step function, 시그모이드 함수
 sigmoid function ('S자 모양'이라는 뜻), ReLU 함수 Rectified Linear Unit
 (렐루)<br>
뉴런 = 노드

```python
def step_function(x):
    y = x > 0
    return y.astype(np.int) # or np.array(x > 0, dtype=np.int)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def relu(x):
    return np.maximum(0, x)

plt.ylim(-0.1, 1.1)

np.dot(A, B) # 내적, 스칼라곱(scalar product), 점곱(dot product)

A1 = np.dot(X, W1) + B1
Z1 = sigmoid(A1)
A2 = np.dot(Z1, W2) + B2
Z2 = sigmoid(A2)
A3 = np.dot(Z2, W3) + B3
Y = identity_function(A3)
```

시그모이드 함수의 이 매끈함이 신경망 학습에서 아주 중요한 역할을 하게
 됩니다.<br>
선형 함수의 문제는 층을 아무리 깊게 해도 '은닉층이 없는 네트워크'로도 똑같은
 기능을 할 수 있다는 데 있습니다.<br>
*w*<sup>(1)</sup><sub>12</sub> (층, 다음 층 순서, 앞 층 순서),
 *b*<sup>(1)</sup><sub>1</sub> (층, 다음 층 순서)<br>
출력층의 활성화 함수 *σ*()는 풀고자 하는 문제의 성질에 맞게 정합니다. 예를 들어
 회귀에는 항등 함수를, 2클래스 분류에는 시그모이드 함수를, 다중 클래스 분류에는
 소프트맥스 함수를 사용하는 것이 일반적입니다.<br>
기계학습 문제는 **분류**(classification)와 **회귀**(regression)으로 나뉩니다.
 분류는 데이터가 어느 클래스(class)에 속하느냐는 문제입니다. 한편, 회귀문제는
 입력 데이터에서 (연속적인) 수치를 예측하는 문제입니다. (19세기 후반 영국의
 우생학자 프랜시스 골턴 경은 사람과 완두콩 등을 대상으로 그 키(크기)를
 측정했습니다. 관찰 결과 키가 큰 부모의 자식은 부모보다 작고 작은 부모의 자식은
 부모보다 큰, 즉 평균으로 회귀(regression)하는 경향이 있음을 알았습니다. 그
 사이에는 선형 관계가 있어 부모의 키로부터 자식의 키를 예측할 수 있고, 그 예측
 결괏값이 연속적인 수치인 것이죠.)

```python
def softmax(a):
    #exp_a = np.exp(a)
    c = np.max(a)
    exp_a = np.exp(a - c) # 오버플로 대책
    sum_exp_a = np.sum(exp_a)
    y = exp_A / sum_exp_a

    return y
```

출력 총합이 1이 된다는 점은 소프트맥스 함수의 중요한 성질입니다. 이 성질 덕분에
 소프트맥스 함수의 출력을 '확률'로 해석할 수 있습니다. 신경망의 분류에서는
 일반적으로 가장 큰 출력을 내는 뉴런에 해당하는 클래스로만 인식합니다. 그리고
 소프트맥스 함수를 적용해도 출력이 가장 큰 뉴런의 위치는 달라지지 않습니다.
 결과적으로 신경망을 분류할 때는 출력층의 소프트맥스 함수를 생략해도 됩니다.
 현업에서도 지수 함수 계산에 드는 자원 낭비를 줄이고자 출력층의 소프트맥스
 함수는 생략하는 것이 일반적입니다. 기계학습의 문제 풀이는 **학습**과
 **추론**(inference)의 두 단계를 거쳐 이뤄집니다. 방금 설명한 대로, 추론
 단계에서는 출력층의 소프트맥스 함수를 생략하는 것이 일반적입니다. 한편,
 신경망을 학습시킬 때는 출력층에서 소프트맥스 함수를 사용합니다.<br>
추론 과정을 신경망의 **순전파**(forward propagation)라고도 합니다.<br>
MNIST 데이터셋 - 훈련 이미지 60,000장, 시험 이미지 10,000장, 28×28 크기의 회색조 이미지(1채널, 0~255)

```python
sys.path.append(os.pardir)
from dataset.mnist import load_mnist
(x_train, t_train), (x_test, t_test) = load_mnist(flatten=True, normalize=False)
# 2번째 이후의 읽기 시 pickle을 이용합니다.
# normalize: True (0.0~1.0), False (0~255)
# flatten: True (784개 원소로 이뤄진 1차원 배열), False (3차원 배열 1×28×28)
# one_hot_label: True (one-hot encoding), False (숫자 형태의 레이블)

from PIL import Image
img = img.reshape(28, 28)
pil_img = Image.fromarray(np.uint8(img))
pil_img.show()

p = np.argmax(y)
p = np.argmax(y_batch, axis=1) # second axis, per row
```

**전처리**(pre-processing) - **정규화**(normalization), 전체 데이터를 균일하게
 분포시키는 데이터 **백색화**(whitening)

## 4. 신경망 학습

2장의 퍼셉트론도 직선으로 분리할 수 있는(선형 분리 가능) 문제라면 데이터로부터
 자동으로 학습할 수 있습니다. 선형 분리 가능 문제는 유한 번의 학습을 통해 풀 수
 있다는 사실이 **퍼셉트론 수렴 정리**(perceptron convergence theorem)로 증명되어 있습니다. 하지만 비선형 분리 문제는 (자동으로) 학습할 수 없습니다.<br>
인식하는 알고리즘을 밑바닥부터 '설계하는' 대신, 주어진 데이터를 잘 활용해서
 해결하고 싶어질 겁니다. 그런 방법의 하나로, 이미지에서 **특징**(feature)을
 추출하고 그 특징의 패턴을 기계학습 기술로 학습하는 방법이 있습니다. 여기서
 말하는 특징은 입력 데이터(입력 이미지)에서 본질적인 데이터(중요한 데이터)를
 정확하게 추출할 수 있도록 설계된 변환기를 가리킵니다. 이미지의 특징은 보통
 벡터로 기술하고, 컴퓨터 비전 분야에서는 SIFT, SURF, HOG 등의 특징을 많이
 사용합니다. 이런 특징을 사용하여 이미지 데이터를 벤터로 변환하고, 변환된 벡터를 가지고 지도 학습 방식의 대표 분류 기법인 SVM, KNN 등으로 학습할 수 있습니다.
 다만, 이미지를 벡터로 변환할 때 사용하는 특징은 여전히 '사람'이 설계하는 것임에
 주의해야 합니다.<br>
신경망은 이미지에 포함된 중요한 특징까지도 '기계'가 스스로 학습할 것입니다.
 딥러닝을 **종단간 기계학습**(end-to-end machine learning)이라고도 합니다.
 여기서 종단간은 '처음부터 끝까지'라는 의미로, 데이터(입력)에서 목표한
 결과(출력)를 얻는다는 뜻을 담고 있죠. 신경망의 이점은 모든 문제를 같은 맥락에서 풀 수 있는 점에 있습니다. 세부사항과 관계없이 신경망은 주어진 데이터를 온전히
 학습하고, 주어진 문제의 패턴을 발견하려 시도합니다.<br>
우리가 범용적으로 사용할 수 있는 모델을 원하기 때문입니다. 이 **범용 능력**을
 제대로 평가하기 위해 훈련 데이터와 **시험 데이터**를 분리하는 것이죠. 범용
 능력은 아직 보지 못한 데이터(훈련 데이터에 포함되지 않는 데이터)로도 문제를
 올바르게 풀어내는 능력입니다. 이 범용 능력을 획득하는 것이 기계학습의 최종
 목표고 말이죠. 참고로 한 데이터셋에만 지나치게 최적화된 상태를
 **오버피팅**(overfitting, 과적화, 과학습, 과적응)이라고 합니다.<br>
신경망 학습에서 사용하는 지표는 **손실 함수**(loss function, 비용 함수(cost
 function))라고 합니다. 이 손실 함수는 입의의 함수를 사용할 수도 있지만
 일반적으로는 평균 제곱 오차(nean squared error, MSE)와 교차 엔트로피 오차(cross
 entropy error, CEE)를 사용합니다.

```python
def mean_squared_error(y, t):
    return 0.5 * np.sum((y-t)**2)

def cross_entropy_error(y, t):
    delta = 1e-7
    return -np.sum(t * np.log(y + delta))

def cross_entropy_error(y, t):
    if y.ndim == 1:
        t = t.reshape(1, t.size)
        y = y.reshape(1, y.size)

    batch_size = y.shape[0]
    return -np.sum(t * np.log(y)) / batch_size
    # return -np.sum(np.log(y[np.arange(batch_size), t])) / batch_size

batch_mask = np.random.choice(train_size, batch_size)
```

신경망 학습에서도 훈련 데이터로부터 일부만 골라 학습을 수행합니다. 이 일부를
 **미니배치**(mini-batch)라고 하죠. 가령 60,000장의 훈련 데이터 중에서 100장을
 무작위로 뽑아 그 100장을 사용하여 학습하는 것입니다. 이러한 학습 방법을
 **미니배치 학습**이라고 합니다.<br>
신경망을 학습할 때 손실 함수 대신 정확도를 지표로 삼아서는 안 된다. 정확도를
 지표로 하면 매개변수의 미분이 대부분의 장소에서 0이 되기 때문이다. 정확도는
 매개변수의 미소한 변화에는 거의 반응을 보이지 않고, 반응이 있더라도 그 값이
 불연속적으로 갑자기 변화합니다. 이는 '계단 함수'를 활성화 함수로 사용하지 않는
 이유와도 들어맞습니다. 만약 활성화 함수로 계단 함수를 사용하면 지금까지 설명한
 것과 같은 이유로 신경망 학습이 잘 이뤄지지 않습니다.<br>
오차를 줄이기 위해 (*x* + *h*)와 (*x* - *h*)일 때의 함수 *f*의 차분을 계산하는
 방법을 쓰기도 합니다. 이 차분은 *x*를 중심으로 그 전후의 차분을 계산한다는
 의미에서 **중심 차분** 혹은 **중앙 차분**이라 합니다(한편, (*x* + *h*)와 *x*의
 차분은 **전방 차분**이라 합니다). 여기에서 하는 것처럼 아주 작은 차분으로
 미분을 구하는 것을 **수치 미분**이라 합니다. 한편, 수식을 전개해 미분을 구하는
 것을 **해석적**(analytic)이라는 말을 이용하여 '해석적 해' 혹은 '해석적으로
 미분을 구하다' 등으로 표현합니다.

```python
def numerical_diff(f, x):
    h = 1e-4 # 0.0001
    return (f(x+h) - f(x-h)) / (2*h)

def numerical_gradient(f, x):
    h = 1e-4 # 0.0001
    grad = np.zeros_like(x) # x와 형상이 같은 배열을 생성

    for idx in range(x.size):
        tmp_val = x[idx]
        # f(x+h) 계산
        x[idx] = tmp_val + h
        fxh1 = f(x)

        # f(x-h) 계산
        x[idx] = tmp_val - h
        fxh2 = f(x)

        grad[idx] = (fxh1 - fxh2) / (2*h)
        x[idx] = tmp_val # 값 복원

    return grad
```

모든 변수의 편미분을 벡터로 정리한 것을 **기울기**(gradient)라고 합니다.<br>
기울기가 가리키는 쪽은 각 장소에서 함수의 출력 값을 가장 줄이는 방향입니다.<br>
기울기를 잘 이용해 함수의 최솟값(또는 가능한 한 작은 값)을 찾으려는 것이
 **경사법**(gradient method)입니다. 경사법은 기계학습의 최적화 문제에서 흔히
 쓰는 방법입니다. 특히 신경망 학습에는 경사법을 많이 사용합니다. 경사법은
 기울기가 0인 장소를 찾지만 그것이 반드시 최솟값이라고는 할 수
 없습니다(극솟값이나 안장점(saddle point)일 가능성이 있습니다). 또, 복잡하고
 찌그러진 모양의 함수라면 (대부분) 평평한 곳으로 파고들면서 고원(plateau,
 플래토)이라 하는, 학습이 진행되지 않는 정체기에 빠질 수 있습니다. 경사법은
 최솟값을 찾느냐, 최댓값을 찾느냐에 따라 이름이 다릅니다. 전자를 경사
 하강법(gradient descent method), 후자를 경사 상승법(gradient ascent
 method)이라고 하죠.<br>
갱신하는 양을 신경망 학습에서는 **학습률**(learning rate)이라고 합니다. 학습률
 같은 매개변수를 **하이퍼파라미터**(hyper parameter, 초매개변수)라고 합니다.
 신경망의 가중치 매개변수는 훈련 데이터와 학습 알고리즘에 의해서 '자동'으로
 획듣되는 매개변수인 반면, 학습률 같은 하이퍼파라미터는 사람이 직접 설정해야
 하는 매개변수인 것이죠.

```python
def gradient_descent(f, init_x, lr=0.01, step_num=100):
    x = init_x

    for i in range(step_num):
        grad = numerical_gradient(f, x)
        x -= lr * grad
    return x

self.W = np.random.randn(2,3) # 정규분포로 초기화
```

데이터를 미니배치로 무작위로 선정하기 때문에 **확률적 경사 하강법**(stochastic
 gradient descent, SGD)이라고 부릅니다. 대부분의 딥러닝 프레임워크는 확률적 경사
 하강법의 영어 머리글자를 딴 **SGD**라는 함수로 이 기능을 구현하고 있습니다.<br>
에폭(epoch)은 하나의 단위입니다. 1에폭은 학습에서 룬련 데이터를 모두 소진했을
 때의 횟수에 해당합니다. 예컨대 훈련 데이터 10,000개를 100개의 미니배치로 학습할
 경우, 확률적 경사 하강법을 100회 반복하면 모든 훈련 데이터를 '소진'한 게
 됩니다. 이 경우 100회가 1에폭이 됩니다.<br>
오버피팅이 시작되는 순간부터 시험 데이터에 대한 정확도가 점차 떨어지기
 시작한다는 뜻입니다. 여기서 중요한 인사이트! 이 순간을 포작해 학습을 중단하면
 오버피팅을 효과적으로 예방할 수 있겠죠? 이 기법을 조기 종료(early stopping)라
 하며, "6.4 바른 핛브을 위해"에서 살펴볼 '가중치 감소', '드롭아웃'과 함께
 대표적인 오버피팅 예방법입니다.

## 5. 오차역전파법

오차역전파법(역전파법, backpropagation)<br>
[Hacker's guide to Neural Networks][hackers_guide_to_neural_networks]<br>
순전파(forward propagation), 역전파(backward propagation, 미분을 계산하는
 역할)<br>
연쇄법칙(chain rule, 합성 함수의 미분에 대한 성질, 계산 그래프 상의 역전파와
 같다) - 합성 함수의 미분은 합성 함수를 구성하는 각 함수의 미분의 곱으로 나타낼
 수 있다<br>
곱셈 노드 역전파는 상류의 값에 순전파 때의 입력 신호들을 '서로 바꾼 값'을 곱해서
 하루로 보냅니다.

```python
class Relu:
    def __init__(self):
        self.mask = None

    def forward(self, x):
        self.mask = (x <= 0)
        out = x.copy()
        out[self.mask] = 0

        return out

    def backward(self, dout):
        dout[self.mask] = 0
        dx = dout

        return dx

class Sigmoid:
    def __init__(self):
        self.out = None

    def forward(self, x):
        out = 1 / (1 + np.exp(-x))
        self.out = out

        reutnr out

    def backward(self, dout):
        dx = dout * (1.0 - self.out) * self.out

        return dx
```

신경망의 순전파 때 수행하는 행렬의 내적은 기하학에서 **어파인 변환**(affine
 transformation)이라고 합니다. 그래서 이 책에서는 어피인 변환을 수행하는 처리를
 'Affine 계층'이라는 이름으로 구현합니다.

```python
class Affine:
    def __init__(self, W, b):
        self.W = W
        self.b = b
        self.x = None
        self.dW = None
        self.db = None

    def forward(self, x):
        self.x = x
        out = np.dot(x, self.W) + self.b

        return out

    def backward(self, dout):
        dx = np.dot(dout, self.W.T)
        self.dW = np.dot(self.x.T, dout)
        self.db = np.sum(dout, axis=0)

        return dx

class SoftwaxWithLoss:
    def __init__(self):
        self.loss = None # 손실
        self.y = None    # softmax의 출력
        self.t = None    # 정답 레이블(원-핫 벡터)

    def forward(self, x, t):
        self.t = t
        self.y = softmax(x)
        self.loss = cross_entropy_error(self.y, self.t)
        reutrn self.loss

    def backward(self, dout=1):
        batch_size = self.t.shape[0]
        dx = (self.y - self.t) / batch_size

        return dx
```

'소프트맥스 함수'의 손실 함수로 '교차 엔트로피 오차'를 사용하니 역전파가 (*y* -
 *t*)로 말끔히 떨어집니다. 사실 이런 말끔한 결과는 우연이 아니라 교차 엔트로피
 오차라는 함수가 그렇게 설계되었기 때문입니다. 또, '항등 함수'의 손실 함수로
 '평균 제곱 오차'를 사용하면 역전파 결과가 (*y* - *t*)로 말끔히 떨어집니다.<br>
신경망 학습 순서 - 미니배치 -> 기울기 산출 -> 매개변수 갱신 -> 반복<br>
수치 미분은 구현하기는 쉽지만 게산이 오래 걸렸습니다. 오차역전파법을 이용하면
 느린 수치 미분과 달리 기울기를 효율적이고 빠르게 구할 수 있습니다.<br>
수치 미분의 이점은 구현하기 쉽다는 것이죠. 그래서 수치 미분의 구현에는 버그가
 숨어 있기 어려운 반면, 오차역전파법은 구현하기 복잡해서 종종 실수를 하곤
 합니다. 그래서 수치 미분의 결과와 오차역전파법의 결과를 비교하여 오차역전파법을
 제대로 구현했는지 검증하곤 한답니다. 이처럼 두 방식으로 구한 기울기가
 일치함(엄밀히 말하면 거의 같음)을 확인하는 작업을 **기울기 확인**(gradient
 check)이라고 합니다.

[hackers_guide_to_neural_networks]: http://karpathy.github.io/neuralnets/

## 6. 학습 관련 기술들

대부분의 딥러닝 프레임워크는 다양한 최적화 기법을 구현해 제공하며, 원하는
 기법으로 쉽게 바꿀 수 있는 구조로 되어 있습니다. 예를 들어 Lasagne이라는 딥러닝 프레임워크는 다양한 최적화 기법을 구현해 [updates.py][updates_py] 파일에 함수로 정리해 두었습니다.<br>
SGD(확률적 경사 하강법)의 단점은 비등방성(anisotropy) 함수(방향에 따라 성질, 즉
 여기에서는 기울기가 달라지는 함수)에서는 탐색 경로가 비효율적이라는 것입니다.
 또한, SGD가 지그재그로 탐색하는 근본 원인은 기울어진 방향이 본래의 최솟값과
 다른 방향을 가리켜서라는 점도 생각해볼 필요가 있습니다. 이제부터 SGD의 이러한
 단점을 개선해주는 모멘텀, AdaGrad, Adam이라는 세 방법을 소개할 겁니다.

```python
class SGD:
    def __init__(self, lr=0.01):
        self.lr = lr

    def update(self, params, grads):
        for key in params.keys():
            params[key] -= self.lr * grads[key]

class Momentum:
    def __init__(self, lr=0.01, momentum=0.9):
        self.lr = lr
        self.momentum = momentum
        self.v = None

    def update(self, params, grads):
        if self.v is None:
            self.v = {}
            for key, val in params.items():
                self.v[key] = np.zeros_like(val)

        for key in params.keys():
            self.v[key] = self.momentum*self.v[key] - self.lr*grads[key]
            params[key] += self.v[key]

class AdaGrad:
    def __init__(self, lr=0.01):
        self.lr = lr
        self.h = None

    def update(self, params, grads):
        if self.h is None:
            self.h = {}
            for key, val in params.items():
                self.h[key] = np.zeros_like(val)

        for key in params.keys():
            self.h[key] += grads[key] * grads[key]
            params[key] -= self.lr * grads[key] / (np.sqrt(self.h[key]) + 1e-7)
```

학습률을 정하는 효과적 기술로 **학습률 감소**(learning rate decay)가 있습니다.
 학습률을 서서히 낮추는 가장 간단한 방법은 매개변수 '전체'의 학습률 값을
 일괄적으로 낮추는 것이겠죠. 이를 더욱 발전시킨 것이 AdaGrad입니다. AdaGrad는
 개별 매개변수에 적응적으로(adaptive) 학습률을 조정하면서 학습을 진행합니다.
 매개변수의 원소 중에서 많이 움직인(크게 갱신된) 원소는 학습률이 낮아집니다.<br>
AdaGrad는 과거의 기울기를 제곱하여 계속 더해갑니다. 그래서 학습을 진행할수록
 갱신 강도가 약해집니다. 실제로 무한히 계속 학습한다면 어느 순간 갱신량이 0이
 되어 전혀 갱신되지 않게 되죠. 이 문제를 개선한 기법으로서 RMSProp이라는 방법이
 있습니다. RMSProp은 과거의 모든 기울기를 균일하게 더해가는 것이 아니라, 먼
 과거의 기울기는 서서히 잊고 새로운 기울기 정보를 크게 반영합니다. 이를
 **지수이동평균**(Exponential Moving Average, EMA)이라 하여, 과거 기울기의 반영
 규모를 기하급수적으로 감소시킵니다.<br>
모멘텀과 AdaGrad 기법을 융합한다는 생각을 기초로 만든 기법이 바로 Adam입니다.
 또, 하이퍼파라미터의 '편향 보정'이 진행된다는 점도 Adam의 특징입니다. Adam은
 하이퍼파라미터를 3개 설정합니다. 하나는 지금까지의 학습률(논문에서는 *α*로
 등장), 나머지 두 개는 일차 모멘텀용 계수 *β*<sub>1</sub>과 이차 모멘텀용 계수
 *β*<sub>2</sub>입니다.

오버피팅을 억제해 범용 성능을 높이는 테크닉인 **가중치 감소**(weight decay)
 기법을 소개하려 합니다. 그렇다면 가중치의 초깃값을 모두 0으로 설정하면
 어떨까요? 정확히는 가중치를 균일한 값으로 설정해서는 안 됩니다. 그 이유는 바로
 오차역전파법에서 모든 가중치의 값이 똑같이 갱신되기 때문입니다. 이 '가중치가
 고르게 되어버리는 상황'을 막으려면 (정확히는 가중치의 대칭적인 구조를
 무너뜨리려면) 초깃값을 무작위로 설정해야 합니다.
여기에서 사용하는 시그모이드 함수는 그 출력이 0에 가까워지자(또는 1에
 가까워지자) 그 미분은 0에 다가갑니다. 그래서 데이터가 0과 1에 치우쳐 분포하게
 되면 역전파의 기울기 값이 점점 작아지다가 사라집니다. 이것은 **기울기
 소실**(gradient vanishing)이라 알려진 문제입니다. 층을 깊게 하는 딥러닝에서는
 기울기 소실은 더 심각한 문제가 될 수 있습니다. 활성화값(활성화 함수의 출력
 데이터)들이 치우치면 **표현력을 제한**한다는 관점에서 문제가 됩니다.
 이 상황에서는 다수의 뉴런이 거의 같은 값을 출력하고 있으니 뉴련을 여러 개 둔
 의미가 없어진다는 뜻입니다. 각층의 활성화값은 적당히 고루 분포되어야 합니다.
 층과 층 사이에 적당하게 다양한 데이터가 흐르게 해야 신경망 학습이 효율적으로
 이뤄지기 때문입니다. 반대로 치우친 데이터가 흐르면 기울기 소실이나 표현력 제한
 문제에 빠져서 학습이 잘 이뤄지지 않는 경우가 생깁니다.<br>
사비에르 글로로트(Xavier Glorot)와 요슈아 벤지오(Yoshua Bengio)의 논문에서
 권장하는 가중치 초깃값인, 일면 Xavier 초깃값을 써보겠습니다. 현재 Xavier
 초깃값은 일반적인 딥러닝 프레임워크들이 표준적으로 이용하고 있습니다. [그림
 6-13]은 오른쪽으로 갈수록 약간씩 일그러지고 있습니다. 이 일ㄹ그러짐은 sigmoid
 함수 대신 tanh 함수(쌍곡선 함수)를 이용하면 개선됩니다. 실제로 tanh 함수를
 이용하면 말끔한 종 모양으로 분포됩니다. tanh 함수도 sigmoid 함수와 같은 'S'자
 모양 곡선 함수입니다. 다만 tanh 함수가 원점(0, 0)에서 대칭인 S 곡선인 반면,
 sigmoid 함수는 (x, y) = (0, 0.5)에서 대칭인 S 곡석이죠. 활성화 함수용으로는
 원점에서 대칭인 함수가 바람직하다고 알려져 있습니다.<br>
ReLU에 특화된 초깃값을 찾아낸 카이밍 히(Kaiming He)의 이름을 따 **He
 초깃값**이라 합니다. He 초깃값은 앞 계층의 노드가 n개일 때, 표준편차가
 sqrt(2/n)인 정규분포를 사용합니다. Xavier 초깃값이 sqrt(1/n)이었죠? ReLU는 음의
 영역이 0이라서 더 넓게 분포시키기 위해 2배의 계쑤가 필요하다고 (직감적으로)
 해석할 수 있겠습니다. 활성화 함수로 ReLU를 사용할 때는 He 초깃값을, sigmoid나
 tanh 등의 S자 모양 곡선일 때는 Xavier 초깃값을 쓰겠습니다. 이것이 현재의 모범
 사례라고 할 수 있습니다.

```python
for i, a in activations.items():
    plt.subplot(1, len(activations), i+1)
    plt.title(str(i+1) + "-layer")
    plt.hist(a.flatten(), 30, range=(0,1))
plt.show()
```

**배치 정규화**(Batch Normalization)는 2015년에 제안된 방법입니다. 배치 정규화의
 기본 아이디어는 각 층에서의 활성화값이 적당히 분포되도록 조정하는 것입니다.
 그래서 데이터 분포를 정규화하는 '배치 정규화(Batch Norm) 계층'을 신경망에
 (Affine과 ReLU 계층 사이에) 삽입합니다. 배치 정규화는 아직 세상에 나온 지 얼마
 안 된 기법임에도 많은 연구자와 기술자가 즐겨 사용하고 있습니다. 실제로 기계학습
 콘테스트의 결과를 보면 이 배치 정규화를 사용하여 뛰어난 결과를 달성한 예가
 많습니다. 배치 정규화가 주목받는 이유는 다음과 같습니다.
* 학습을 빨리 진행할 수 있다(학습 속도 개선).
* 초깃값에 크게 의존하지 않는다(골치 아픈 초깃값 선택 장애여 안녕!)
* 오버피팅을 억제한다(드롭아웃 등의 필요성 감소).

[Understanding the backward pass through Batch Normalization Layer][understanding_backward_pass_through_batch_normalization_layer]

오버피팅은 주로 다음의 두 경우에 일어납니다.
* 매개변수가 많고 표현력이 높은 모델
* 훈련 데이터가 적음

오버피팅 억제용으로 예로부터 많이 이용해온 방법 중 가중치 감소라는 것이
 있습니다. 이는 학습 과정에서 큰 가중치에 대해서는 그에 상응하는 큰 페널티를
 부과하여 오버피팅을 억제하는 방법입니다. 원래 오버피팅은 가중치 매개변수의 값이
 커서 발생하는 경우가 많기 때문이죠. 가중치 가소는 모든 가중치 각각의 손실
 함수에 λW<sup>2</sup>/2을 더합니다. 따라서 가중치의 기울기를 구하는 계산에서는
 그동안의 오차역전파법에 따른 결과에 정규화 항을 미분한 λW를 더합니다.<br>
L2 법칙(가중치의 제곱 법칙)은 각 원소의 제곱들을 더한 것에 해당합니다. L2 법칙
 외에 L1 법칙과 L∞ 법칙도 있습니다. L1 법칙은 절댓값의 합에 해당합니다. L∞
 법칙은 Max 법칙이라고도 하며, 각 원소의 절댓값 중 가장 큰 것에 해당합니다.
 정규화 항으로 L2 법칙, L1 법칙, L∞ 법칙 중 어떤 것도 사용할 수 있습니다. 각자
 특징이 있는데, 이 책에서는 일반적으로 자주 쓰는 L2 법칙만 구현합니다.

그러나 신경망 모델이 복잡해지면 가중치 감소만으로는 대응하기 어려워집니다. 이럴
 때는 흔히 **드롭아웃**(Dropout)이라는 기법을 이용합니다. 훈련 때 은닉층의
 뉴런을 무작위로 골라 삭제합니다. 훈련 때는 데이터를 흘릴 때마다 삭제할 뉴런을
 무작위로 선택하고, 시험 때는 모든 뉴런에 신호를 전달합니다. 단, 시험 때는 각
 뉴런의 출력에 훈련 때 삭제한 비율을 곱하여 출력합니다. 삭제한 비율은 곱하지
 않아도 좋습니다. 실제 딥러닝 프레임워크들도 비율을 곱하지 않습니다.<br>
[체이너(Chainer) 프레임워크][chainer]<br>
기계학습에서는 **앙상블 학습**(ensemble learning)을 애용합니다. 앙상블 학습은
 개별적으로 학습스킨 여러 모델의 출력을 평균 내어 추론하는 방식입니다. (역주,
 평균 말고도 투표(voting) 등 다른 방법도 사용합니다.) 앙상블 학습을 수행하면
 신경망의 정확도가 몇% 정도 개선된다는 것이 실험적으로 알려져 있습니다. 앙상블
 학습은 드롭아웃과 밀접합니다. 드롭아웃이 학습 때 뉴런을 무작위로 삭제하는
 행위를 매번 다른 모델을 학습시키는 것으로 해석할 수 있기 때문이죠. 즉,
 드롭아웃은 앙상블 학습과 같은 효과를 (대략) 하나의 네트워크로 구현했다고 생각할
 수 있습니다.

```python
class Dropout:
    def __init__(self, dropout_ratio=0.5):
        self.dropout_ratio = dropout_ratio
        self.mask = None

    def forward(self, x, train_flg=True):
        if train_flg:
            self.mask = np.random.rand(*x.shape) > slef.dropout_ratio
            return x * self.mask
        else:
            return x * (1.0 - self.dropout_ratio)

    def backward(self, dout):
        return dout * self.mask
```

주의할 점은 하이퍼파라미터의 성능을 평가할 때는 시험 데이터를 사용해서는 안
 된다는 것입니다. 시험 데이터를 사용하여 하이퍼파라미터를 조정하면
 하이퍼파라미터 값이 시험 데이터에 오버피팅되기 때문입니다. 그래서
 하이퍼파라미터를 조정할 때는 하이퍼파라미터 전용 확인 데이터가 필요합니다.
 하이퍼파라미터 조정용 데이터를 일반적으로 **검증 데이터**(validation data)라고
 부릅니다. 하이퍼파라미터를 최적화할 때의 핵심은 하이퍼파라미터의 '최적 값'이
 존재하는 범위를 조금씩 줄여간다는 것입니다. 신경망의 하이퍼파라미터
 최적화에서는 그리드 서치(grid search) 같은 규칙적인 탐색보다는 무작위로
 샘플랭해 탐색하는 편이 좋은 결과를 낸다고 알려져 있습니다. 하이퍼파라미터를
 최적화할 때는 딥러닝 학습에는 오랜 시간(예컨대 매칠이나 몇 주 이상)이 걸린다는
 점을 기억해야 합니다. 따라서 나쁠 듯한 값은 일찍 포기하는 게 좋습니다.<br>
여기에서 설명한 하이퍼파라미터 최적화 방법은 실용적인 방법입니다. 하지만
 과학이라기보다는 다분히 수행자의 '지헤'와 '직관'에 의존한다는 느낌이 들죠.
 더 세련된 기법을 원한다면 **베이즈 최적화**(Bayesian optimization)를 소개할 수
 있겠네요. 베이츠 최적화는 베이즈 정리(Bayes' theorem)를 중심으로 한 수학 이론을
 구사하여 더 엄밀하고 효율적으로 최적화를 수행합니다. 자세한 내용은 〈Practical
 Bayesian Optimization of Machine Learning Algorithm〉 논문 등을 참고하세요.

[updates_py]: https://github.com/Lasagne/Lasagne/blob/master/lasagne/updates.py
[understanding_backward_pass_through_batch_normalization_layer]: http://kratzert.github.io/2016/02/12/understanding-the-gradient-flow-through-the-batch-normalization-layer.html
[chainer]: http://chainer.org

## 7. 합성곱 신경망(CNN)

**합성곱 신경망**(convolutional neural network, CNN)은 이미지 인식과 음성 인식
 등 다양한 곳에서 사용되는데, 특히 이미지 인식 분야에서 딥러닝을 활용한 기법은
 거의 다 CNN을 기초로 하죠. **합성곱 계층**(convolutional layer)과 **풀링
 계층**(pooling layer)이 새롭게 등장합니다.<br>
지금까지 본 신경망은 인접하는 계층의 모든 뉴런과 결합되어 있었습니다. 이를
 **완전연결**(fully-connected, 전결합)이라고 하며, 완전히 연결된 계층을 **Affine
 계층**이라는 이름으로 구현했습니다. 완전연결 계층의 문제점은 '데이터의 형상이
 무시'된다는 사실입니다.
지금까지의 'Affine-ReLU' 연결이 'Conv-ReLU-(Pooing)'으로 바뀌었다고 생각할 수
 있겠죠. 주목할 또 다른 점은 출력에 가까운 층에서는 지금까지의 'Affine-ReLU'
 구성을 사용할 수 있다는 점입니다. 또, 마지막 출력 계층에서는 'Affine-Softmax'
 조합을 그대로 사용합니다. 이상은 일반적인 CNN에서 흔히 볼 수 있는
 구성입니다.<br>
CNN에서는 합성곱 계층의 입출력 데이터를 **특징 맵**(feature map)이라고도 합니다.
 합성곱 계층의 입력 데이터를 **입력 특징 맵**(input feature map), 출력 데이터를
 **출력 특징 맵**(output feature map)이라고 하는 식이죠. 합성곱 연산은 이미지
 처리에서 말하는 **필터 연산**에 해당하죠. 문헌에 따라 필터를 **커널**이라
 칭하기도 합니다. 합성곱 연산은 필터의 **윈도우**(window)를 일정 간격으로
 이동해가며 입력 데이터에 적용합니다. 입력과 필터에서 대응하는 원소끼리 곱한 후
 그 총합을 구합니다(이 계산을 **단일 곱셈-누산**(fused multiply-add, FMA)이라
 합니다). 편향은 필터를 적용한 후의 데이터에 더해집니다. 그리고 편향은 항상
 하나(1×1)만 존재합니다. 그 하나의 값을 필터를 적용한 모든 원소에 더하는 것이죠.
 합성곱 연산을 수행하기 전에 입력 데이터 주변을 특정 값(예컨대 0)으로 채우기도
 합니다. 이를 **패딩**(padding)이라 하며, 합성곱 연산에서 자주 이용하는
 기법입니다. 패딩은 주로 출력 크기를 조정할 목적으로 사용합니다. 예를 들어 (4,
 4) 입력 데이터에 (3, 3) 필터를 적용하면 출력은 (2, 2)가 되어, 입력보다 2만큼
 줄어듭니다. 이는 합성곱 연산을 몇 번이나 되풀이하는 심층 신경망에서는 문제가 될
 수 있습니다. 필터를 적용하는 위치의 간격을 **스트라이드**(stride)라고
 합니다.<br>
[움직이는 데모][cs231n_convolution_demo]<br>
3차원 합성곱 연산에서 주의할 점은 입력 데이터의 채널 수와 필터의 채널 수가
 같아야 한다는 것입니다. 여기에 1) 필터의 수와 2) 한 덩어로로 묶어 배치로 처리할
 입력 데이터 수가 차원으로 추가됩니다.<br>
폴링은 세로·가로 방향의 공간을 줄이는 연산입니다. 풀링에는 **최대 풀링**(max
 pooling)과 **평균 풀링**(average pooling)이 있습니다. 이미지 인식 분야에서는
 주로 최대 풀링을 사용합니다. 참고로, 풀링의 윈도우 크기와 스트라이드는 같은
 값으로 설정하는 것이 보통입니다. 풀링 계층은 입력의 변화에 영향을 적게
 받는다(강건하다).

넘파이에서는 원소에 접근할 때 for 문을 사용하지 않는 것이 바람직합니다. im2col은
 입력 데이터를 필터링(가중치 계산)하기 좋게 전재하는(펼치는) 함수입니다. 문제를
 행렬 계산으로 만들면 선형 대수 라이브러리를 활용해 효율을 높일 수 있습니다.
 (im2col은 'image to column', 즉 '이미지에서 행렬로'라는 뜻입니다. 카페(Caffe)와
 체이너(Chainer) 등의 딥러닝 프레임워크는 im2col이라는 이름의 함수를 만들어
 합성곱 계층을 구현할 때 이용하고 있습니다.) im2col로 입력 데이터를 전개한
 다음에는 합성곱 계층의 필터(가중치)를 1열로 전개하고, 두 행렬의 내적을 계산하면
 됩니다. 이는 완전연결 계층의 Affine 계층에서 한 것과 거의 같습니다. 이 책이
 제공하는 col2im을 사용한다는 점을 제외하면 합성곱 게층의 역전파는 Affine 계층과 똑같습니다.

```python
class Convolution:
    def __init__(self, W, b, stride=1, pad=0):
        self.W = W
        self.b = b
        self.stride = stride
        self.pad = pad

    def forward(self, x):
        FN, C, FH, FW = self.W.shape
        N, C, H, W = x.shape
        out_h = int(1 + (H + 2*self.pad - FH) / self.stride)
        out_w = int(1 + (W + 2*self.pad - FW) / self.stride)

        col = im2col(x, FH, FW, self.stride, self.pad)
        col_W = self.W.reshape(FN, -1).T
        out = np.dot(col, col_W) + self.b

        out = out.reshape(N, out_h, out_w, -1).transpose(0, 3, 1, 2)

        return out

    def backward(self, dout):
        # 생략
        # https://github.com/WegraLee/deep-learning-from-scratch/blob/master/common/layers.py

class Pooling:
    def __init__(self, pool_h, pool_w, stride=1, pad=0):
        self.pool_h = pool_h
        self.pool_w = pool_w
        self.stride = stride
        self.pad = pad

    def forward(self, x):
        N, C, H, W = x.shape
        out_h = int(1 + (H + 2*self.pad - FH) / self.stride)
        out_w = int(1 + (W + 2*self.pad - FW) / self.stride)

        col = im2col(x, self.pool_h, self.pool_w, self.stride, self.pad)
        col = col.reshape(-1, self.pool_h*self.pool_w)

        out = np.max(col, axis=1)

        out = out.reshape(N, out_h, out_w, C).transpose(0, 3, 1, 2)

        return out

    def backward(self, dout):
        # 생략
```

딥러닝 시각화에 관한 연구에 따르면, 계층이 깊어질수록 추출되는 정보(정확히는
 강하게 반응하는 뉴런)는 더 추상화된다는 것을 알 수 있습니다. 1번 층은
 에지(edge)와 블롭(blob), 3번째 층은 텍스처, 5번째 층은 사물의 일부, 마지막
 완전연결계층은 사물의 클래스(개, 자동차 등)에 뉴런이 반응한다. 즉, 층이
 깊어지면서 뉴런이 반응하는 대상이 단순한 모양에서 '고급' 정보로 변화해갑니다.
 다시 말하면 사물의 '의미'를 이해하도록 변화하는 것입니다.<br>
LeNet은 손글씨 숫자를 인식하는 네트워크로, 1998년에 제안되었습니다. LeNet은
 시그모이드 함수를 사용하는 데 반해, 현재는 주로 ReLU를 사용합니다. 또, 원래의
 LeNet은 서브샘플링을 하여 중간 데이터의 크기가 작아지지만 현재는 최대 풀링이
 주류입니다.<br>
2012년에 발표된 AlexNet은 딥러닝 열풍을 일으키는 데 큰 역할을 했습니다.
 LeNet에서 큰 구조는 바뀌지 않습니다만, AlexNet에서는 다음과 같은 변화를
 주었습니다. GPU와 빅 데이터는 이런 문제에 해결책을 던졌다고 말할 수 있습니다.
* 활성화 함수로 ReLU를 이용한다.
* LRN(Local Response Normalization)이라는 국소적 정규화를 실시하는 계층을
  이용한다.
* 드롭아웃을 사용한다.

[cs231n_convolution_demo]: http://cs231n.github.io/convolutional-networks/

## 8. 딥러닝

여기에서 사용하는 합성곱 계층은 모두 3×3 크기의 작은 필터로, 층이 깊어지면서
 채널 수가 더 늘어나는 것이 특징입니다. 또 풀링 계층을 추가하여 중간 데이터의
 공간 크기를 점차 줄여갑니다. 그리고 마지막 단의 완전연결 계층에서는 드롭아웃
 계층을 사용합니다.<br>
[What is the class of this iamge?][what_is_the_class_of_this_image]<br>
**데이터 확장**(data augmentation)은 입력 이미지(훈련 이미지)를 알고리즘을
 동원해 '인위적'으로 확장합니다. 입력 이미지를 회전하거나 세로로 이동하는 등
 미세한 변화를 주어 이미지의 개수를 늘리는 것이죠. 이는 데이터가 몇 개 없을 때
 특히 효과적인 수단입니다. 예를 들어 이미지 일부를 잘라내는 **crop**이나
 (이미지의 대칭성을 고려하지 않아도 되는 경우에) 좌우를 뒤집는 **flip** 등이
 있겠죠. 일반적인 이미지에는 밝기 등의 외형 변화나 확대·축소 등의 스케일 변화도
 효과적입니다.

층을 깊게 할 때의 이점을 설명하겠습니다. 그 이점 하나는 신경망의 매개변수 수가
 줄어든다는 것입니다. 층을 깊게 한 신경망은 깊지 않은 경우보다 적은 매개변수로
 같은 (혹은 그 이상) 수준의 표현력을 달성할 수 있습니다. 5×5의 합성곱 연산 1회는
 3×3의 합성곱 연산을 2회 수행하여 대체할 수 있습니다. 게다가 전자의 매개변수
 수가 25개인 반면, 후자는 총 18개(2×3×3)이며, 매개변수 수는 층을 반복할수록
 적어집니다. 작은 필터를 겹쳐 신경망을 깊게 할 때의 장점은 매개변수 수를 줄여
 넓은 **수용 영역**(receptive field)을 소화할 수 있다는 데 있습니다(수용 영역은
 뉴런에 변화를 일으키는 국소적인 공간 영역입니다). 게다가 층을 거듭하면서 ReLU
 등의 활성화 함수를 합성곱 계층 사이에 끼움으로써 신경망의 표현력이 개선됩니다.
 이는 활성화 함수가 신경망에 '비선형' 힘을 가하고, 비선형 함수가 겹치면서 더
 복잡한 것도 표현할 수 있게 되기 때문입니다.<br>
학습의 효율성도 층을 깊게 하는 것의 이점입니다. 층을 깊게 함으로써 학습 데이터의
 양을 줄여 학습을 고속으로 수행할 수 있다는 뜻입니다. 신경망을 깊게 하면
 학습해야 할 문제를 계층적으로 분해할 수 있습니다. 각 층이 학습해야 할 문제를 더
 단순한 문제로 대체할 수 있는 것이죠. 예를 들어 처음 층은 에지 학습에 전념하여
 적은 학습 데이터로 효율적으로 학습할 수 있습니다. 개가 등장하는 이미지보다
 에지를 포함한 이미지가 많고, 에지의 패턴은 개라는 패턴보다 구조가 훨씬 간단하기
 때문이죠. 또, 층을 깊게 하면 정보를 계층적으로 전달할 수 있다는 점도
 중요합니다.

딥러닝이 지금처럼 큰 주목을 받게 된 계기는 이미지 인식 기술을 겨루는 장인
 ILSVRC(ImageNet Large Scale Visual Recognition Challenge)의 2012년 대회입니다.
 그해의 대회에서 딥러닝에 기초한 기법, 일명 AlexNet이 압도적인 성적으로
 우승하면서 그동안의 이미지 인식에 대한 접근법을 뿌리부터 뒤흔들었습니다.
 이미지넷(ImageNet)은 100만 장이 넘는 이미지를 담고 있는 데이터셋입니다. ILSVRC
 대회에는 시험 항목이 몇 가지 있는데, 그중 하나가
 **분류**(classification)입니다. 분류 부분에서는 1,000개의 클래스를 제대로
 분류하는지를 겨룹니다. **톱-5 오류**(top-5 error)란 확률이 가장 높다고 생각하는
 후보 클래스 5개 안에 정답이 포함되지 않은, 즉 5개 모두가 틀린 비율입니다.
 2015년에는 150층이 넘는 심층 신경망인 ResNet이 오류율을 3.5%까지 낮췄습니다.
 덧붙여서, 이 결과는 일반적인 인간의 인식 능력을 넘어섰다고 합니다.<br>
VGG에서 주목할 점은 3×3의 작은 필터를 사용한 합성곱 계층을 연속으로 거친다는
 것입니다. 합성곱 계층을 2~4회 연속으로 풀링 계층을 두어 크기를 절반으로 줄이는
 처리를 반복합니다. 성능 면에서는 1위인 GoogLeNet에 뒤지지만, VGG는 구성이
 간단하여 응용하기 좋습니다.<br>
GoogLeNet에는 가로 방향에 '폭'이 있습니다. 이를 인셉션 구조라 하며, 인셉션
 구조는 크기가 다른 필터(와 풀링)를 여러 개 적용하여 그 결과를 결합합니다. 또
 GoogLeNet에서는 1×1 크기의 필터를 사용한 합성곱 계층을 많은 곳에서 사용합니다.
 이 1×1의 합성곱 연산은 채널 쪽으로 크기를 줄이는 것으로, 매개변수 제거와 고속
 처리에 기여합니다.<br>
지금까지 층을 깊게 하는 것이 성능 향상에 중요하다는 건 알고 있었습니다. 그러나
 딥러닝의 학습에서는 층이 지나치게 깊으면 학습이 잘 되지 않고, 오히려 성능이
 떨어지는 경우도 많습니다. ResNet에서는 그런 문제를 해결하기 위해서 **스킵
 연결**(skip connection)을 도입합니다. 이 구조가 층의 깊이에 비례해 성능을
 향상시킬 수 있게 한 핵심입니다(물론 층을 깊게 하는 데는 여전히 한계가
 있습니다). 스킵 연결이난 입력 데이터를 합성곱 계층을 건너뛰어 출력에 바로
 더하는 구조를 말합니다. 스킵 연결은 층이 깊어져도 학습을 효율적으로 할 수
 있도록 해주는데, 이는 역전파 때 스킵 연결이 신호 감쇠를 막아주기 때문입니다.
 스킵 연결은 입력 데이터를 '그대로' 흘리는 것으로, 역전파 때도 상류의 기울기를
 '그대로' 하류로 보냅니다. 그래서 스킵 연결로 기울기가 작아지너가 지나치게 커질
 걱정 없이 앞 층에 '의미 있는 기울기'가 전해지리라 기대할 수 있습니다. 층을 깊게
 할수록 기울기가 작아지는 소실 문제를 줄여줍니다. ResNet은 합성곱 계층을 2개
 층마다 건너뛰면서 층을 깊게 합니다.<br>
이미지넷이 제공하는 거대한 데이터셋으로 학습한 가중치 값들은 실제 제품에
 활용해도 효과적이고, 또 많이들 그렇게 이용하고 있습니다. 이를 **전이
 학습**(transfer learning)이라고 해서, 학습된 가중치(혹은 그 일부)를 다른
 신경망에 복사한 다음, 그 상태로 재학습(fine tuning)을 수행합니다. 전이 학습은
 보유한 데이터셋이 적을 때 특히 융요한 방법입니다.

딥러닝에서 대부분의 시간을 합성곱 계층에서 소모합니다.<br>
그중에서도 구글의 텐서플로와 마이크로소프트의 CNTK(Computational Network Toolkit)는 분산 학습에 역점을 두고 개발하고 있습니다.<br>
컴퓨터에서 소수를 표현하는 방식으로 32비트 단정밀도(single-precision)와 64비트
 배정밀도(double-precision) 부동소수점 등의 포맷이 있지만, 지금까지의 실험으로는
 딥러닝은 16비트 반정밀도(half-precision)만 사용해도 학습에 문제가 없다고 알려져
 있습니다. 실제로 엔비디아의 최신 CPU인 파스칼(Pascal) 아키텍처는 이 포맷을
 지원하여, 이제는 반정밀도 부동소수점이 표준적으로 이용되리라 생각합니다.
 엔비디아의 맥스웰(Maxwell) 세대 GPU는 반정밀도 부동소수점 수를 스토리지로
 지원하고 있었지만, 연산 자체는 16비트로 수행하지 않았습니다. 이것이 파스칼
 세대에 와서 연산 역시 16비트로 하기 시작하여, 이전 세대보다 2배 정도 빨라질
 것으로 기대합니다. 딥러닝의 비트 수를 줄이는 연구가 몇 가지 진행되고 있습니다.
 최근에는 가중치와 중간 데이터를 1비트로 표현하는 ⟨Binarized Neural
 Networks⟩라는 방법도 등장했습니다. 딥러닝을 고속화하기 위해 비트를 줄이는
 기술은 앞으로 주시해야 할 분야이며, 특히 딥러닝을 임베디드용으로 이용할 때
 중요한 주제입니다. (역주, AMD GPU 역시 베가(VEGA) 모델부터는 반정밀도 연산을
 지원합니다. 또한 게임 콘솔인 PS4 Pro뿐 아니라,
 ⟨[애플의 자체 제작 GPU 살펴보기][inside_apples_custom_gpu]⟩에 따르면 애플이
 아이폰7에 처음 사용한 A10 퓨전 칩의 GPU도 반정밀도 연산을 지원합니다. 딥러닝이
 생활 깊숙이 파고들면서 모바일 기기까지도 본격적인 딥러닝 대응을 시작했다는
 방증일 것입니다.

CNN을 이용하여 사물 검출을 수행하는 방식은 몇 가지가 있는데, 그중에서도
 **R-CNN**(Regions with Convolutional Neural Network)이 유명합니다. 먼저 사물이
 위치한 영역을 (논문에서는 Selective Search 기법으로) 찾아내고, 추출한 각 영역에
 CNN을 적용하여 클래스를 분류합니다. 최근에는 이 후보 영역 추출까지 CNN으로
 처리하는 **Faster R-CNN** 기법도 등장했습니다. Faster R-CNN은 모든 일을 하나의
 CNN에서 처리하기 때문에 아주 빠릅니다.<br>
**분할**(segmentation)이란 이미지를 픽셀 수준에서 분류하는 문제입니다. 신경망을
 이용해 분할하는 가장 단순한 방법은 모든 픽셀 각각을 추론하는 것입니다. 이러한
 낭비를 줄여주는 기법으로 FCN(Fully Convolutional Network)이 고안되었습니다.
 이는 단 한 번의 forward 처리로 모든 픽셀의 클래스를 분류해주는 놀라운
 기법입니다. 일반적인 CNN이 완전연결 계층을 이용하는 반면, FCN은 이 완전연결
 계층을 '같은 기능을 하는 합성곱 계층'으로 바꿉니다. FCN의 마지막에 수행하는
 확대는 이중 선형 보간(bilinear interpolation)에 의한 선형 확대입니다. FCN에서는
 이 선형 확대를 역합성곱(deconvolution) 연산으로 구현해내고 있습니다.<br>
딥러닝으로 사진 캡션을 생성하는 방법으로는 NIC(Neural Image Caption) 모델이
 대표적입니다. NIC는 심층 CNN과 자연어를 다루는 **순환 신경망**(Recurrent Neural
 Network, RNN, 재귀 신경망)으로 구성됩니다. RNN은 순환적 관계를 갖는 신경망으로
 자연어나 시계열 데이터 등의 연속된 데이터를 다룰 때 많이 활용합니다. (이
 순환적인 구조로 인해 이전에 생성한 정보에 영향을 받는(바꾸어 말하면, 과거의
 정보를 기억하는) 점이 RNN의 특징입니다.) NIC는 CNN으로 사진에서 특징을
 추출하고, 그 특징을 RNN에 넘깁니다. RNN은 CNN이 추출한 특징을 초깃값으로 해서
 텍스트를 '순환적'으로 생성합니다.

이 기술은 네트워크의 중간 데이터가 콘텐츠 이미지의 중간 데이터와 비슷해지도록
 학습합니다. 이렇게 하면 입력 이미지를 콘텐츠 이미지의 형태로 흉내 낼 수
 있습니다. 또, 스타일 이미지의 화풍을 흡수하기 위해 '스타일 행렬'이라는 개념을
 도입합니다. 그 스타일 행렬의 오차를 줄이도록 학습하여 입력 이미지를 고흐의
 화풍과 비슷해지게 만들 수 있는 것입니다. (역주, [Prisma][prisma] 앱)<br>
**DCGAN**(Deep Convolutional Generative Adversarial Network) 기술의 핵심은
 생성자(Generator)와 식별자(Discriminator)로 불리는 2개의 신경망을 이용한다는
 점입니다. 생성자가 진짜와 똑같은 이미지를 생성하고 식별자는 그것이
 진짜인지(생성자가 생성한 이미지인지, 아니면 실제로 촬영된 이미지인지)를
 판정합니다. 이렇게 둘의 능력을 부지런히 갈고닦게 한다는 개념이
 **GAN**(Generative Adversarial Network) 기술의 재미난 점입니다. 이전까지 살펴본
 기계학습 문제는 **지도 학습**(supervised learning)이라는 유형의 문제였습니다.
 그러나 이는 지도 없이 스스로 학습하는 **자율 학습**(unsupervised learning)
 문제입니다. 자율 학습은 비교적 오래전부터 연구된 분야지만(**Deep Belief
 Network**와 **Deep Boltzmann Machine**이 대표적입니다) 최근에는 그다지 활발하게
 연구되지는 않는다는 느낌입니다.<br>
**[SegNet][segnet]**이라는 CNN 기반 신경망은 자율 주행시 주변 환경을 정확하게 인식해냅니다.<br>
**강화학습**(reinforcement learning)은 사람이 시행착오를 겪으며 배우듯 컴퓨터도
 시행착오 과정에서 스스로 학습하게 하려는 분야입니다. 강화학습에서는
 에이전트라는 것이 환경에 맞게 행동을 선택하고, 그 행동에 의해서 환경이 변한다는
 게 기본적인 틀입니다. 강화학습의 목적은 에이전트의 행동 지침을 더 나은 보상을
 받는 쪽으로 바로잡는 것입니다. 딥러닝을 사용한 강화학습 중 **Deep
 Q-Network**(일명 DQN)라는 방법이 있습니다. 강화학습 알고리즘을 기초로 하는
 Q학습에서는 최적 행동 가치 함수로 최적인 행동을 정합니다. 이 함수를
 딥러닝(CNN)으로 비슷하게 흉내 내어 사용하는 것이 DQN입니다. 그동안의 비디오
 게임 학습에서는 게임의 상태(캐릭터 위치 등)는 미리 추출하는 것이
 보통이었습니다. 그러나 DQN에서는 입력 데이터는 비디오 게임의 영상뿐입니다.
 게임마다 설정을 바꿀 필요없이 단순히 DQN에 게임 영상을 보여주기만 하면 되기
 때문에 응용 가능성이 현격히 높습니다. 알파고와 DQN은 모두 구글이 인수한
 딥마인드(Deep Mind)가 진행한 연구입니다. (역주, 블리자드가 2017년 1분기에
 이미지 기반 AI API와 스크립트 기반 AI API를 공식 공개하기로 했으니, 관심 있는
 분은 주시하시기 바랍니다.)

[what_is_the_class_of_this_image]: http://rodrigob.github.io/are_we_there_yet/build/classification_datasets_results.html
[inside_pples_custom_gpu]: http://macnews.tistory.com/5200
[prisma]: http://prisma-ai.com
[segnet]: http://mi.eng.cam.ac.uk/projects/segnet/

## A. Softmax-with-Loss 계층의 계산 그래프

