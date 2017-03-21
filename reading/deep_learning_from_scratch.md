# [밑바닥부터 시작하는 딥러닝][homepage], 사이토 고키 저, 개앞맵시(이복연) 역, 한빛미디어 (2017)

[source code][source_code], [용어표][terms]

[개발자의 앞길에 맵핵 시전][translator_facebook],
 [스카이넷도 딥러닝부터][from_deep_learning_to_skynet] [마인드맵][mindmaps]<br>
이 책은 이미지 인식을 주제로 합니다. 자연어 처리, 음성 인식 등은 이 책의 대상이
 아닙니다.<br>
[CS231n: Convolutional Neural Networks for Visual Recognition][cs231n] (Stanford)

[homepage]: http://www.hanbit.co.kr/store/books/look.php?p_code=B8475831198
[source_code]: https://github.com/WegraLee/deep-learning-from-scratch
[terms]: https://docs.google.com/spreadsheets/d/1ccwGiC01X-gs3PPcXPUz67W9rS6l994LD4AL18KF1_0/edit#gid=0
[translator_facebook]: https://www.facebook.com/dev.loadmap
[from_deep_learning_to_skynet]: https://www.mindmeister.com/ko/812276967/_
[mindmaps]: https://www.mindmeister.com/ko/users/channel/wegra
[cs231n]: http://cs231n.stanford.edu/

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

    batch_Size = y.shape[0]
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

## 6. 학습 관련 기술들

## 7. 합성곱 신경망(CNN)

## 8. 딥러닝

## A. Softmax-with-Loss 계층의 계산 그래프

