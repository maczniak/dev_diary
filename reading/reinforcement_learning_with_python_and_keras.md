# [파이썬과 케라스로 배우는 강화학습: 내 손으로 직접 구현하는 게임 인공지능][homepage], 이웅원, 양혁렬, 김건무, 이영무, 이의령 저, 위키북스 (2017)

[소스 코드][source_code]<br>
[RLCode][rlcode], [Fundamental of Reinforcement Learning][fundamental_of_reinforcement_learning] (GitBook)<br>
[모두를 위한 RL강좌][reinforcement_learning_lecture_for_everyone]

[homepage]: http://wikibook.co.kr/reinforcement-learning/
[source_code]: https://github.com/rlcode/reinforcement-learning-kr
[rlcode]: https://github.com/rlcode
[fundamental_of_reinforcement_learning]: https://www.gitbook.com/book/dnddnjs/rl/details
[reinforcement_learning_lecture_for_everyone]: https://www.youtube.com/watch?v=dZ4vw6v3LcA&list=PLlMkM4tgfjnKsCWav-Z2F-MMFRx-2gMGG&index=1

## 1부 강화학습 소개

### 1장: 강화학습 개요

머신러닝은 지도학습과 비지도학습, 강화학습으로 나뉩니다. 강화학습은
 "보상(Reward)"을 통해 학습합니다. 보상은 컴퓨터가 선택한 행동(Action)에 대한
 환경의 반응입니다. 앞으로 강화학습을 통해 스스로 학습하는 컴퓨터를
 에이전트(agent)라고 할 것입니다. 에이전트는 환경에 대해 사전지식이 없는
 상태에서 학습을 합니다.<br>
강화학습은 다른 머신러닝 분야와 다르게 순차적으로 행동을 결정해야 하는 문제를
 다룹니다. 뒤에서 배울 다이내믹 프로그래밍(Dynamic Programming) 또한 순차적으로
 행동을 결정해야 하는 문제에 적용할 수 있습니다. 다이내믹 프로그래밍 외에도 진화
 알고리즘(Evolutionary Algorithm) 또한 이러한 문제를 푸는 데 적용할 수 있습니다.
 순차적으로 행동을 결정하는 문제를 정의할 때 사용하는 방법이 MDP(Markov Decision
 Process)입니다. MDP는 순차적 행동 결정 문제를 수학적으로 정의해서 에이전트가
 순차적 행동 결정 문제에 접근할 수 있게 합니다.
* 상태(state) - 엄밀히 말하면 상태보다는 "관찰"이라는 것이 정확한 표현입니다.
* 행동(action)
* 보상(reward)
* 정책(policy) - 순차적 행동 결정 문제에서 구해야 할 답은 바로 정책입니다.
  에이전트가 보상을 얻으려면 행동을 해야 하는데 특정 상태가 아닌 모든 상태에
  대해 어떤 행동을 해야 할지 알아야 합니다. 이렇게 모든 상태에 대해 에이전트가
  어떤 행동을 해야 하는지 정해놓은 것이 정책입니다.

[PR2 로봇][pr2_robot],
 [Playing Atari with Deep Reinforcement Learning][playing_atari_with_deep_reinforcement_learning]<br>
행동이 얼마나 좋은지가 행동의 가치가 되고 이것을 큐함수(Q Function)라고 합니다.
 이 문제에 사용한 인공신경망을 DQN(Deep Q-Network)이라고 합니다.<br>
사람은 하나를 학습하면 다른 곳에도 그 학습이 영향을 미칩니다. 하지만 현재
 강화학습 에이전트는 각 학습을 다 별개로 취급해서 항상 바닥부터 학습해야 합니다.

[pr2_robot]: http://robotics.usc.edu/resl/robots/4/
[playing_atari_with_deep_reinforcement_learning]: http://www.cs.toronto.edu/~vmnih/docs/dqn.pdf

## 2부 강화학습 기초

### 2장: 강화학습 기초 1 - MDP와 벨만 방정식

MDP - 상태(***S***), 행동(***A***), 보상함수(R<sup>a</sup><sub>s</sub> =
 E[R<sub>t+1</sub> | S<sub>t</sub> = s, A<sub>t<sub> = a]), 상태 변환 확률(state
 transition probability, 환경의 모델, P<sup>a</sup><sub>ss′</sub> =
 E[S<sub>t+1</sub> = s′ | S<sub>t</sub> = s, A<sub>t<sub> = a]),
 감가율(discount factor, γ∈[0,1])

하지만 보상 (+1)을 받았다고 해서 파란색 동그라미에 도착했기 때문에 좋은 보상을
 받는 것이 아닙니다. 에이전트는 파란색 동그라미로 가는 "좋은 행동"을 했기 때문에
 보상을 받는 것입니다.<br>
감가율을 고려한 미래 보상의 현재 가치 γ<sup>k-1</sup>R<sub>t+k</sub><br>
정책 π(a | s) = P[A<sub>t</sub> = a | S<sub>t</sub> = s], 최적 정책<br>
반환값(return) G<sub>t</sub> = R<sub>t+1</sub> + γR<sub>t+2</sub> +
 γ<sup>2</sup>R<sub>t+3</sub> = ...<br>
가치함수(value function, 보상에 대한 기댓값, 확률변수가 아니라 특정 양을
 나타내는 값) v(s) = E[G<sub>t</sub> | S<sub>t</sub> = s] = E[R<sub>t+1</sub> +
 γv(S<sub>t+1</sub>) | S<sub>t</sub> = s]<br>
큐함수(Q function, 어떤 상태에서 어떤 행동이 얼마나 좋은지 알려주는 행동
 가치함수) q<sub>π</sub>(s,a) = E<sub>π</sub>[R<sub>t+1</sub> +
 γq<sub>π</sub>(S<sub>t+1</sub>,A<sub>t+1</sub>) | S<sub>t</sub> = s,
 A<sub>t</sub> = a]<br>
벨만 기대 방정식(Bellman expectation equation) v<sub>π</sub>(s) =
 E<sub>π</sub>[R<sub>t+1</sub> + γv<sub>π</sub>(S<sub>t+1</sub>) | S<sub>t</sub>
 = s] = ∑<sub>a∈A</sub>π(a | s)q<sub>π</sub>(s,a) =
 ∑<sub>a∈A</sub>π(a | s)(R<sub>t+1</sub> +
 γ∑<sub>s′∈S</sub>P<sup>a</sup><sub>ss′</sub>v<sub>π</sub>(s′) = (if
 P<sup>a</sup><sub>ss′</sub> = 1) ∑<sub>a∈A</sub>π(a | s)(R<sub>t+1</sub> +
 γv<sub>π</sub>(s′)<br>
참 가치함수는 "어떤 정책"을 따라서 움직였을 경우에 (수렴) 받게 되는 보상에 대한
 참값입니다. 하지만 최적의 가치함수(optimal value function)는 수많은 정책 중에서
 가장 높은 보상을 주는 가치함수입니다.<br>
(계산 가능한 형태) v<sub>k+1</sub>(s) =
 ∑<sub>a∈A</sub>π(a | s)(R<sup>a</sup><sub>s</sub> + γv<sub>k</sub>(s′)<br>
최적 정책 π<sub>∗</sub>(s,a) = 1 if a = argmax<sub>a∈A</sub>q<sub>∗</sub>(s,a),
 0 otherwise<br>
최적의 가치함수 v<sub>∗</sub>(s) = max<sub>π</sub>[v<sub>π</sub>(s)] =
 max<sub>a</sub>[q<sub>∗</sub>(s,a) | S<sub>t</sub> = s, A<sub>t</sub> = a] =
 max<sub>a</sub>E[R<sub>t+1</sub> + γv<sub>∗</sub>(S<sub>t+1</sub>) |
 S<sub>t</sub> = s, A<sub>t</sub> = a] 벨만 최적 방정식(Bellman optimality
 equation)<br>
최적의 큐함수 q<sub>∗</sub>(s,a) = max<sub>π</sub>[q<sub>π</sub>(s,a)] =
 E[R<sub>t+1</sub> + γmax<sub>a′</sub>q<sub>∗</sub>(S<sub>t+1</sub>,a′) |
 S<sub>t</sub> = s, A<sub>t</sub> = a] 벨만 최적 방정식

### 3장: 강화학습 기초 2 - 그리드월드와 다이내믹 프로그래밍

다이내믹 프로그래밍으로 벨만 기대 방정식을 푸는 것이 정책 이터레이션(policy
 iteration)이며 벨만 최적 방정식을 푸는 것이 가치 이터레이션(value
 iteration)입니다.<br>
다이내믹 프로그래밍을 처음 제시한 사람 또한 벨만 방정식을 만든
 [리처드 벨만][richard_e_bellman]입니다. 벨만 방정식은 다이내믹 프로그래밍
 방정식이라고도 불리며 최적화에 관련된 방정식입니다. 리처드 벨만은 1953년에
 다이내믹 프로그래밍을 처음 소개했습니다.<br>
정책 이터레이션에서는 평가를 정책 평가(policy evaluation)라고 하며, 발전을 정책
 발전(policy improvement)이라고 합니다.<br>
탐욕 정책 발전(greedy policy improvement)으로 얻은 새로운 정책 π′(s) =
 argmax<sub>a∈A</sub>q<sub>π</sub>(s,a)

가치 이터레이션을 설명할 때 정책 이터레이션에서처럼 정책의 발전을 설명하지 않고
 오직 가치함수의 업데이트만을 이야기했습니다. 가치함수 안에 정책이
 내재돼(implicit) 있으므로 가치함수를 업데이트하면 자동으로 정책 또한 발전되는
 것입니다. 정책 이터레이션과의 차이는 다음 상태들을 다 고려해서 업데이트하는
 것이 아니라 제일 높은 값을 가지는 값으로만 업데이트한다는 것입니다. 정책
 이터레이션에서 정책 평가와 정책 발전으로 단계가 나누어져 있다면 가치
 이터레이션에서는 그렇지 않습니다.<br>
하지만 다이내믹 프로그래밍은 계산을 빠르게 하는 것이지 "학습"을 하는 것은
 아닙니다. 다이내믹 프로그래밍의 한계는 크게 세 가지입니다. 1) 계산 복잡도
 O(n<sup>3</sup>) 2) 차원의 저주(curse of dimentionality) 3) 환경에 대한 완벽한
 정보가 필요

[richard_e_bellman]: https://en.wikipedia.org/wiki/Richard_E._Bellman

### 4장: 강화학습 기초 3 - 그리드월드와 큐러닝

에이전트는 환경과의 상호작용을 통해 주어진 정책에 대한 가치함수를 학습할 수
 있는데, 이를 예측(prediction)이라고 합니다. 또한 가치함수를 토대로 정책을
 끊임없이 발전시켜 나가서 최적 정책을 학습하려는 것이 제어(control)입니다.
 예측에는 몬테카를로 예측(Monte-Carlo prediction)과 시간차
 예측(temporal-difference prediction)이 있으며, 제어에는 시간차 제어인 살사가
 있습니다. 그리고 살사의 한계를 극복하기 위한 오프롤리시 제어인 큐러닝이
 있습니다. 강화학습의 고전 알고리즘들은 이제 잘 사용되지는 않지만 수많은
 강화학습 알고리즘의 토대가 됐습니다.<br>
강화학습에서는 계산을 통해서 가치함수를 알아내는 것이 아니라 에이전트가 겪은
 경험으로부터 가치함수를 업데이트하는 것입니다.

v<sub>π</sub>(s) ~ 1/N(s) ∑<sup>N(s)</sup><sub>i=1</sub>G<sub>i</sub>(s)<br>
V<sub>n+1</sub> = 1/n ∑<sup>n</sup><sub>i=1</sub>G<sub>i</sub> = (전개하면)
 V<sub>n</sub> + 1/n (G<sub>n</sub> - V<sub>n</sub>)<br>
V(s) ← V(s) + 1/n (G(s) 업데이트의 목표 - V(s)) = V(s) + α(G(s) - V(s)),
 업데이트의 크기 = 스텝사이즈 × 오차

몬테카를로 예측에서와 같이 에피소드마다가 아니라 타임스텝마다 가치함수를
 업데이트하는 방법이 시간차 예측입니다. 다른 상태의 (정확하지 않은) 가치함수
 예측값을 통해 지금 상태의 가치함수를 예측하는 이러한 방식을
 부트스트랩(bootstrap)이라고 합니다. 에이전트는 현재 상태에서 행동을 한 번 하고
 다음 상태를 알게 되면 바로 이전 상태의 가치함수를 업데이트할 수 있습니다.
 시간차 예측은 충분히 많은 샘플링을 통해 업데이트하면 참 가치함수에 수렴하며
 많은 경우에 몬테카를로 예측보다 더 효율적으로 빠른 시간 안에 참 가치함수에
 근접합니다.<br>
V(S<sub>t</sub>) ← V(S<sub>t</sub>) + α(R + γV(S<sub>t+1</sub>) -
 V(S<sub>t</sub>))

정책 평가 과정에서 참 가치함수에 수렴할 때까지 계산하지 않아도 정책 평가와 정책
 발전을 한 번씩 번갈아 가면서 실행하면 참 가치함수에 수렴할 때까지 계산했을 때와
 같이 최적 가치함수에 가치함수가 수렴한다. 이러한 정책 이터레이션을
 GPI(generalized policy iteration)라고 합니다.<br>
시간차 예측과 탐욕 정책이 합쳐진 것을 시간차 제어(temporal-difference
 control)라고 합니다.
π(s) = argmax<sub>a∈A</sub>Q(s,a)<br>
Q(S<sub>t</sub>,A<sub>t</sub>) ← Q(S<sub>t</sub>,A<sub>t</sub>) + α(R +
 γQ(S<sub>t+1</sub>,A<sub>t+1</sub>) - Q(S<sub>t</sub>,A<sub>t</sub>))<br>
에이전트는 샘플인 상태 S<sub>t</sub>에서 탐욕 정책에 따라 행동 A<sub>t</sub>를
 선택하고 그 행동으로 환경에서 한 타임스텝을 진행합니다. 그러면 환경은
 에이전트에게 보상 R<sub>t+1</sub>을 주고 다음 상태 S<sub>t+1</sub>을
 알려줍니다. 여기서 한 번 더 에이전트는 탐욕 정책에 따라 행동 A<sub>t+1</sub>을
 선택하고 하나의 샘플
 [S<sub>t</sub>,A<sub>t</sub>,R<sub>t+1</sub>,S<sub>t+1</sub>,A<sub>t+1</sub>]이
 생성되면 그 샘플로 큐함수를 업데이트합니다. 이것이 시간차 제어의 과정입니다.
 샘플 때문에 시간차 제어를 다른 말로 살사(SARSA)라고 부릅니다.<br>
하지만 초기의 에이전트에게 탐욕 정책은 잘못된 학습으로 가게 할 가능성이 큽니다.
 즉, 충분한 경험을 통해 에이전트가 보유하고 있는 큐함수들이 최적에 가까워지게
 하는 것이 필요합니다. 이 문제는 강화학습의 중요한 문제로서 탐험(exploration)의
 문제입니다. 에이전트로 하여금 더 탐험하게 할 방법이 필요한 것입니다. 그 대안이
 ε-탐욕 정책입니다. ε만큼의 확률로 탐욕적이지 않은 행동을 선택하게 하는
 것입니다. 학습을 진행함에 따라 ε의 값을 감소시키는 방법도 사용합니다. 하지만
 살사와 큐러닝에서는 ε의 값이 일정한 ε-탐욕 정책을 사용합니다.<br>
π(s) = a<sup>∗</sup> = argmax<sub>a∈A</sub>Q(s,a), 1 - ε / a ≠ a<sup>∗</sup>, ε

살사는 온폴리시 시간차 제어(on-policy temporal-difference control), 즉 자신이
 행동하는 대로 학습하는 시간차 제어입니다. 탐험을 위해 선택한 ε-탐욕 정책 때문에
 에이저트는 오히려 최적 정책을 학습하지 못하고 잘못된 정책을 학습합니다. 하지만
 강화학습에 있어서 탐험은 절대적으로 필요한 부분입니다. 이러한 딜레마를 해결하기
 위해 사용하는 것이 바로 오프폴리시 시간차 제어입니다(off-policy
 temporal-difference control). 다른 말로는 큐러닝(Q-learning)이라고 합니다.<br>
큐러닝에서는 에이전트가 다음 상태 s′를 일단 알게 되면 그 상태에서 가장 큰
 큐함수를 현재 큐함수의 업데이트에 사용합니다. 따라서 현재 상태의 큐함수를
 업데이트하기 위해 필요한 샘플은 <s,a,r,s′>입니다.<br>
Q(S<sub>t</sub>,A<sub>t</sub>) ← Q(S<sub>t</sub>,A<sub>t</sub>) +
 α(R<sub>t+1</sub> + γmax<sub>a′</sub>Q(S<sub>t+1</sub>,a′) -
 Q(S<sub>t</sub>,A<sub>t</sub>))<br>
살사에서는 큐함수를 업데이트하기 위해 벨만 기대 방정식을 사용하고 큐러닝에서는
 큐함수를 업데이트하기 위해 벨만 최적 방정식을 사용한다.

## 3부 강화학습 심화

### 5장: 강화학습 심화 1 - 그리드월드와 근사함수

선형 함수의 경우 층을 넓히고 쌓아봤자 결국 층이 하나인 것과 다름없습니다. 하지만
 비선형 함수의 경우, 층을 넓히고 쌓을수록 함수는 새로운 형태로 변형됩니다.<br>
경사하강법(gradient descent)에는 SGD, RMSprop, Adam과 같은 방법이 있습니다. 모든
 경사하강법은 학습속도(learning rate)라는 변수를 공통으로 가지고 있습니다.<br>
전체 학습 데이터에 대해 한 번 모델을 업데이트하는 것이 아니고 전체 학습 데이터를
 작은 단위로 쪼개서 여러 번에 걸쳐서 모델을 업데이트하는 미니배치(mini-batch)
 방식이 있습니다.

```python
from keras.layers import Dense
from keras.models import Sequential

model = Sequential()
model.add(Dense(12, input_dim=5, activation='sigmoid'))
model.add(Dense(30, activation='sigmoid'))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='mse', optimizer='RMSProp')

model.fit(x_train, y_train, batch_size=32, epoch=1)
```

딥살사의 오차함수 MSE = (정답 - 예측)<sup>2</sup> = (R<sub>t+1</sub> +
 γQ(S<sub>t+1</sub>,A<sub>t+1</sub>) -
 Q(S<sub>t</sub>,A<sub>t</sub>))<sup>2</sup><br>
살사와 큐러닝과는 다르게 딥살사에서 ε-탐욕정책에 사용되는 ε은 시간에 따라서
 감소시킵니다.

지금까지 다룬 강화학습 알고리즘을 한 번에 묶어서 '가치 기반 강화학습(value-based
 reinforcement learning)'이라고 지칭합니다. 하지만 다른 방향에서 순차적 행동
 결정 문제에 접근하는 방법이 있습니다. 바로 '정책 기반 강화학습(policy-based
 reinforcement learning)'입니다. 정책 기반 강화학습은 가치함수를 토대로 행동을
 선택하지 않고 상태에 따라 바로 행동을 선택합니다. 딥살사에서는 인공신경망이
 큐함수를 근사했지만 정책 기반 강화학습에서는 인공신경망이 정책을 근사합니다.
 이제부터 정책 기반 강화학습에서 정책을 근사하는 인공신경망을 정책신경망이라고
 부르겠습니다.<br>
정책신경망에서는 활성함수로 Softmax 함수를 사용합니다. Softmax 함수를 사용할
 때는 인공신경망의 출력층에서 나오는 출력을 다 합해서 1이 나오기를 원할
 때입니다.<br>
이제 정책신경망으로 정책을 대체하기 때문에 θ라는 정책 신경망의 가중치값이 정책을
 표현할 수 있습니다. 목표가 오류함수를 최소화하는 것이 아니라 목표함수를
 최대화하는 것이므로 경사상승법(gradient ascent)이라고 부릅니다. 가치함수를
 미분할 때 등장하는 것이 폴리시 그레이디언트 정리(policy gradient
 theorem)입니다. ([논문][policy_gradient_theorem_paper]) /
 d<sub>π<sub>θ</sub></sub>(s)은 상태 분포(state distribution)라는 말로도
 표현하는데 간단히 말하면 s라는 상태에 에이전트가 있을 확률입니다. / 현재
 에이전트는 정책만 가지고 있고 가치함수 혹은 큐함수를 구할 수 없습니다.
 목표함수의 미분값을 어떻게 잘 근사할 것인가에 대한 방법은 여러 가지가 있습니다.
 가장 고전적인 방법 중 하나는 큐함수를 반환값 G<sub>t</sub>로 대체하는 것입니다.
 큐함수를 반환값으로 대체하는 것이 REINFORCE 알고리즘입니다. 에피소드가 끝날
 때까지 기다리면 에피소드 동안 지나온 상태에 대해 각각의 반환값을 구할 수
 있습니다. REINFORCE 알고리즘은 에피소드마다 실제로 얻은 보상으로 학습하는
 폴리시 그레이디언트라고 할 수 있습니다. 그래서 몬테카를로 폴리시
 그레이디언트라고도 부릅니다.<br>
∇<sub>θ</sub>J(θ) = ∇<sub>θ</sub>v<sub>π<sub>θ</sub></sub>(s<sub>0</sub>)<br>
 = ∑<sub>s</sub>d<sub>π<sub>θ</sub></sub>(s)∑<sub>a</sub>∇<sub>θ</sub>π<sub>θ</sub>(a
 | s)q<sub>π</sub>(s,a)<br>
 = ∑<sub>s</sub>d<sub>π</sub>(s)∑<sub>a</sub>π<sub>θ</sub>(a | s) ×
 ∇<sub>θ</sub>logπ<sub>θ</sub>(a | s)q<sub>π</sub>(s,a)<br>
 = E<sub>π<sub>θ</sub></sub>[∇<sub>θ</sub>logπ<sub>θ</sub>(a |
 s)q<sub>π</sub>(s,a)]<br>
 = E<sub>π<sub>θ</sub></sub>[∇<sub>θ</sub>logπ<sub>θ</sub>(a |
 s)G<sub>t</sub>]<br>
케라스는 텐서플로우의 많은 기능을 포함하지만 반환값을 이용해 그레이디언트를
 구하는 것은 지원하지 않는다는 것입니다. 따라서 사용자가 텐서플로우를 가지고
 변형해서 사용해야 합니다.<br>
크로스 엔트로피는(cross entropy)는 MSE와 같이 지도학습에서 많이 사용되는
 오류함수입니다. 크로스 엔트로피는 엔트로피를 약간 변형해서
 -∑<sub>i</sub>y<sub>i</sub>logp<sub>i</sub>와 같이 표현할 수 있습니다. 두 값이
 가까워질수록 전체 식의 값은 줄어들어서 y<sub>i</sub>와 p<sub>i</sub>가 같아지면
 식의 값은 최소가 됩니다. 지도학습에서 보통 y<sub>i</sub>는 정답을 사용하기
 때문에 현재 예측 값이 얼마나 정답과 가까운지를 나타내게 됩니다.<br>
REINFORCE는 ε-탐욕 정책을 사용하지 않기 때문에 지속적인 탐험을 에이전트가 하기
 어렵습니다. 초반에 에이전트는 초록색 세모에 많이 부딪히는데, 이때 에이전트는
 초록색 세모에 부딪히지 않도록 학습합니다. 따라서 에이전트는 시작점에서
 움직이지 않게 되고 목표였던 파란색 동그라미로 갈 방법이 없어집니다. 이러한
 문제를 해결하고자 타임스텝마다 (-0.1)의 보상을 에이전트에게 주었습니다. 따라서
 에이전트는 가만히 시작점에 머무는 행동이 좋은 행동이 아닌 것임을 알게 되고
 에이전트를 끈내줄 파란색 동그라미를 찾아 탐험하게 됩니다.

[policy_gradient_theorem_paper]: http://web.eecs.umich.edu/~baveja/Papers/PolicyGradientNIPS99.pdf

### 6장: 강화학습 심화 2 - 카트폴

DQN이라는 알고리즈은 2013년 딥마인드가 "Playing Atari with Deep Reinforcement
 Learning"이라는 논문에서 소개했습니다. 딥살사에서는 온폴리시 알고리즘인 살사를
 이용해 학습했습니다. 오프폴리시 알고리즘인 큐러닝과 인공신경망을 함께
 사용하려면 경험 리플레이(experience replay)가 필요합니다. 경험 리플레이라는
 아이디어는 에이전트가 환경에서 탐험하며 얻은 샘플 (s,a,r,s′)을 메모리에
 저장한다는 것입니다. 샘플을 저장하는 메모리는 리플레이 메모리(replay
 memory)라고 합니다. 에이전트가 학습할 때는 리플레이 메모리에서 여러 개의 샘플을
 무작위로 뽑아서 뽑은 샘플에 대해 인공신경망을 업데이트합니다. 경험 리플레이를
 이용하면 샘플 간의 상관관계를 없앨 수 있습니다.<br>
DQN의 또 한 가지 특징은 타깃신경망(target network)을 사용한다는 것입니다.
 부트스트랩의 문제점은 업데이트의 목표가 되는 정답이 계속 변한다는 것입니다.
 이를 방지하기 위해 정답을 만들어내는 인공신경망을 일정 시간 동안 유지합니다.
 오류 함수 수식에서는 이를 구분하기 위해 타깃신경망은 θ<sup>-</sup>를 매개변수로
 갖는 것으로 표현하고 인공신경망은 θ를 매개변수로 갖는 것으로 표현합니다.

REINFORCE 알고리즘은 일종의 몬테카를로 폴리시 그레이디언트로서 에피소드마다만
 학습할 수 있다는 단점이 있습니다. 이 단점을 극복하고 매 타임스텝마다 학습할 수
 있도록 한 것이 액터-크리틱(actor-critic)입니다. 액터-크리틱은 리처드 서튼의
 책에 소개돼 있습니다.<br>
반환값을 사용하지 않고 큐함수 또한 근사하는 방법을 사용합니다. 정책을
 인공신경망으로 근사했는데 인공신경망을 하나 더 만들어서 큐함수를 근사하는
 것입니다. 이 네트워크를 앞으로 가치신경망이라 하겠습니다. 가치신경망의 역할은
 정책을 평가하는 것이기 때문에 크리틱(critic)이라는 이름을 붙었습니다.<br>
액터-크리틱의 업데이트 식 θ<sub>t+1</sub> ≈ θ<sub>t</sub> +
 α[[∇<sub>θ</sub>logπ<sub>θ</sub>(a | s)Q<sub>w</sub>(s,a)] ≈ θ<sub>t</sub> +
 α[[∇<sub>θ</sub>logπ<sub>θ</sub>(a | s)δ<sub>v</sub>]<br>
액터-크리틱의 오류함수 = 정책 신경망 출력의 크로스 엔트로피 × 큐함수(가치신경망
 출력) ≈ δ<sup>2</sup><br>
이 경우 큐함수의 값에 따라 오류함수의 값이 많이 변화하게 됩니다(다른 말로는
 분산이 큽니다). 따라서 큐함수의 변화 정도를 줄여주기 위해 베이스라인(baseline)을 사용합니다. 베이스라인은 특정 상태에서 행동에 따라 값이 변하지 않아야 하는데 액터-크리틱에서는 그래서 가치함수를 베이스라인으로 사용합니다. 가치함수 또한 근사해야 하는데 새로운 v라는 변수를 사용해서 근사할 수 있습니다. 가치함수를 베이스라인으로 큐함수에서 빼준 것을 어드밴티지(advantage) 함수라고 합니다. / 그러나 큐함수와 베이스라인인 가치함수를 따로 근사하기 때문에 비효율적입니다. 따라서 큐함수를 가치함수를 사용해서 표현하면 가치함수만 근사해도 어드밴치지 함수를 정의할 수 있습니다. 가치함수만 근사해서 정의한 어드밴티지 함수는 형태가 시간차 에너와 같으므로 δ<sub>v</sub>라고 정의합니다.<br>
A(S<sub>t</sub>,A<sub>t</sub>) = Q<sub>w</sub>(S<sub>t</sub>,A<sub>t</sub>) -
 V<sub>v</sub>(S<sub>t</sub>) / δ<sub>v</sub> = R<sub>t+1</sub> +
 γV<sub>v</sub>(S<sub>t+1</sub>) - V<sub>v</sub>(S<sub>t</sub>)<br>
샘플을 통해 시간차 에러를 구하고 어드밴티지 함수를 구함 → 시간차 에러로
 가치신경망을, 어드밴티지 함수로 정책신경만을 업데이트<br>
이렇게 액터-크리틱이 어드밴티지를 사용하기 때문에 다른 이름으로는 A2C(advantage
 actor-critic)이라고도 합니다.<br>
액터-크리틱은 크리틱을 업데이트하는 데 살사 방식을 사용하기 때문에 리플레이
 메모리와 같이 현재 정책이 아닌 다른 정책에 의해 생성된 샘플로 학습할 수
 없습니다. 하지만 현재로서는 학습 속도와 샘플들 사이의 연관성 때문에 학습 성능이
 떨어집니다. 이러한 액터-크리틱의 단점을 극복한 것이 A3C(asynchronous advantage
 actor-critic)입니다.

### 7장: 강화학습 심화 3 - 아타리

[컨볼루션 신경망 논문][gradient_based_learning_applied_to_document_recognition]<br>
후버로스(Huber loss)는 1과 -1 사이의 구간에서는 2차함수이며 그 밖의 구간에서는
 1차함수인 오류함수입니다. 이렇게 하면 -1과 1을 넘어가는 큰 오류에 대해 민감하게
 반응하지 않아도 된다는 장점이 있어서 학습이 더 안정적으로 진행됩니다.<br>
지도학습의 경우는 정답이 정해져 있기 때문에 학습이 정상적으로 이뤄진다면 정답에
 맞게 학습되기 때문에 오류함수는 감소합니다. 하지만 강화학습에서는 학습의 목표인
 타킷이 정해져 있지 않습니다. 따라서 지도학습에서와 같이 시간에 따라 단조롭게
 감소하는 양상을 보이지 않습니다. 이 같은 이유로 지도학습에서는 오류함수로
 모델이 어떻게 학습하고 있는지 확인할 수 있지만 강화학습에서는 오류함수만으로는
 모델이 잘 학습하고 있는지 확인할 수 없습니다. 강화학습에서 에이전트의 목표는
 결국 보상의 최대화입니다.

하지만 딥마인드의 DQN에서는 리플레이 메모리로 1,000,000 크기의 메모리를
 사용합니다. 따라서 컴퓨터의 메모리를 많이 차지하며 느린 학습 속도의 원인이
 됩니다. 또한 리플레이 메모리를 통해 학습한다는 것은 지금 정책이 아닌 이전
 정책을 통해 모은 샘플로 학습한다는 것입니다. 따라서 리플레이 메모리를 사용할
 때는 오프폴리시 강화학습을 사용해야만 한다는 단점이 있습니다. DQN은 그중에서
 큐러닝을 사용한 것입니다.<br>
이 문제에 DQN과 다르게 접근한 방법이 있습니다. 2016년 딥마인드의 논문인
 "Asynchronous Methods for Deep Reinforcement Learning"에 소개된 A3C
 알고리즘입니다. DQN과 같이 메모리에 많은 샘플을 쌓아서 샘플 사이의 연관성을
 깨는 것이 아니라 아예 에이전트를 여러 개 사용하는 것입니다. 샘플을 모으는 각
 에이전트는 액터러너(actor-learner)라고도 합니다. 각 액터러너는 각기 다른
 환경에서 학습을 합니다. 액터러너가 일정 타임스텝 동안 모은 샘플을 통해
 클로벌신경망을 업데이트하고 자신을 글로벌신경망으로 업데이트합니다. 여러 개의
 액터러너가 이 과정을 비동기적으로 진행하므로 A3C라는 이름이 붙은 것입니다.<br>
`_make_predict_function` 함수는 케라스에 내장돼 있는 함수로서 학습과는 상관이
 없고 멀티스레딩을 케라스에서 이용할 때 발생하는 에러를 제거하기 위한
 함수입니다.<br>
A3C에서는 반환값 대신 어드밴티지 함수를 이용해 행동이 좋은 행동인지 나쁜
 행동인지를 판단합니다. 1-타임스텝이 아니라 k-타임스텝이 지난 후에 어드밴티지를
 계산하는 것을 멀티스텝 시간차학습(multi-step temporal-difference
 learning)이라고도 하며, 살사와 몬테카를로 중간이라고 볼 수 있습니다. A3C는 이
 밖에 엔트로피가 오류함수에 추가됩니다. 엔트로피를 최소화하는 방향은 모든 행동의
 확률을 동등하게 하려는 방향이라는 것을 알 수 있습니다. 다르게 해석하자면
 액터러너로 하여금 더 탐험을 하게 하는 오류함수라고 할 수 있습니다.<br>
점수로는 DQN과 별 차이가 없더라도 A3C의 장점은 바로 빠른 학습 속도입니다.
 최근에는 이 A3C를 GPU에 최적화해서 더 빠른 시간 안에 학습을 하기도 합니다. 이를
 통해 A3C가 기존의 DQN에서 사용했던 경험 리플레이를 대체할 방법이라는 것이
 증명되는 것입니다.

[gradient_based_learning_applied_to_document_recognition]: http://yann.lecun.com/exdb/publis/pdf/lecun-01a.pdf

### 부록 A: 학습 결과 업로드

[OpenAI Gym][openai_gym]

```python
from gym import wrappers

env = gym.make('CartPole-v1')
env = wrappers.Monitor(env, '/tmp/cartpole_upload', force=True)

env.close()
gym.scoreboard.api_key = "your api key"
gym.upload('/tmp/cartpole_upload')
```

[openai_gym]: https://gym.openai.com/

### 참고문헌

Richard S. Sutton and Andrew G. Barto "Reinforcement Learning: An Introduction"<br>
David Silver, [UCL Course on Reinforcement Learning 강의][ucl_course_on_rl]<br>
[maze_env.py][maze_env_py], [breakout_dqn.py][breakout_dqn_py],
 [breakout_a3c.py][breakout_a3c_py]

[ucl_course_on_rl]: http://www0.cs.ucl.ac.uk/staff/d.silver/web/Teaching.html
[maze_env_py]: https://github.com/MorvanZhou/Reinforcement-learning-with-tensorflow/blob/master/contents/3_Sarsa_maze/maze_env.py
[breakout_dqn_py]: https://github.com/tokb23/dqn
[breakout_a3c_py]: https://github.com/jaara/AI-blog

