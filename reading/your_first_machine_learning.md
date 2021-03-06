# [처음 배우는 머신러닝][homepage], 김승연, 정용주 저, 한빛미디어 (2017)

[소스 코드][source_code]<br>
[한국통계학회 용어집][statistics_terminology]

[homepage]: http://www.hanbit.co.kr/media/books/book_view.html?p_code=B8660115730
[source_code]: https://github.com/your-first-ml-book
[statistics_terminology]: http://www.kss.or.kr/bbs/board.php?bo_table=psd_sec

## Part 1 머신러닝 기초 지식

### CHAPTER 1 머신러닝 시작하기

많은 머신러닝 이론이 성능 보장을 위해 개발되었습니다. 이에 해당하는 대표적인
 기법으로는 스펙트럴 러닝(spectral learning), 서포트 벡터 머신(SVM), 볼록
 최적화(convex optimization)가 있습니다.

* 지도학습 - 회귀, 분류, 랭킹/추천
* 비지도학습 - 군집화/토픽 모델링, 밀도 추정 (커널 밀도 추정(kernel density
  estimation)과 가우스 혼합 모델(Gaussian mixture model)이 대표적인 기법입니다),
  차원 축소
* 강화학습

### CHAPTER 2 머신러닝의 주요 개념

모델 → 손실함수 → 최적화 → 모델 평가

모델은 데이터를 어떻게 바라볼지에 대한 가정(통계학에서는 믿음)입니다.<br>
모델이 데이터를 예측할 때 생기는 오류가 편향의 제곱과 분산으로 쪼개진다. 즉,
 더 나은 성능을 내려면 편향을 줄이거나 분산을 줄여야 한다. 간단한 모델일수록
 가정이 강하기 때문에 표현력이 부족하므로 편향이 크게 나타난다. 일반적으로
 모델이 복잡할수록 학습할 때마다 나타나는 모델 편차가 크다. 편향이니 분산을
 직접적으로 줄이는 대표적인 예로 부스팅(간단한 모델을 여러 개 조합하여 편향을
 줄이는 방법)과 랜덤 포레스트(복잡한 모델인 결정 트리를 여러 개 조합하여 분산을
 줄이는 방법)를 들 수 있다.<br>
정규화는 모델의 표현식에 추가적으로 제약 조건을 걸어서 모델이 필요 이상으로
 복잡해지지 않도록 자동으로 조정해주는 기법이다.

절댓값 손실함수는 정답과 멀리 떨어져 있는 데이터(아웃라이어)가 많아서 손실값을
 제곱하면 그런 데이터가 전체 손실함수의 값을 좌지우지해 학습이 잘 이루어지지
 않는 경우에 종종 사용한다.<br>
∑<sub>*i*</sub>|*w*<sub>*i*</sub>| (*L*<sub>1</sub> 정규화 손실함수),
 ∑<sub>*i*</sub>*w*<sub>*i*</sub><sup>2</sup> (*L*<sub>2</sub> 정규화
 손실함수)<br>
이런 이유로 미분 가능한 손실함수를 많이 사용합니다. 예를 들어 모든 점에서 미분
 가능한 제곱 손실함수를 절댓값 손실함수보다 많이 씁니다.<br>
뉴턴 방법은 임의의 학습률을 사용하는 대신 1차 미분값(gradient)과 2차
 미분값(hessian)을 활용하여 업데이틀 수행합니다. 준 뉴턴 방식은 2차 미분을 직접
 계산하는 대신 1차 미분값을 활용해 2차 미분값을 근사해서 사용합니다. 대표적인
 방법으로 BFGS와 LBFGS가 있습니다.<br>
SGD는 경사하강법에 비해 적은 양의 데이터를 사용하여 1차 미분값을 계산하므로
 미분값이 분안정하고 결과적으로 학습률에 많은 영향을 받습니다. 그렇기 때문에
 SGD를 사용한다면 본격적으로 시스템을 완성하기 전에 미니 배치의 크기와 학습률을
 다양하게 시도해보고 적절한 값을 선택하는 것이 중요합니다.

모델 평가를 할 때는 학습 데이터뿐만 아니라 학습 데이터가 아닌 새로운 데이터가
 들어왔을 때도 잘 동작하는지 측정합니다. 이를 일반화(generalization)라고 하며,
 실제 머신러닝 시스템을 구축할 때 굉장히 중요한 요소입니다. 일반화가 얼마나
 잘되었는지 측정하는 에러를 일반화 에러라고 합니다.<br>
교차검증은 학습-평가 데이터 나누기를 한 번만 하는 것이 아니라 여러 번 반복해서
 좀 더 정확하게 일반화 에러를 평가하는 방법입니다. 교차검증은 학습-평가 데이터
 나누기만큼 유명하고 많이 쓰이는 방법으로, 여기서는 교차검증 방법 중에서 가장
 유명한 K겹 교차검증(K-fold cross-validation)에 대해 알아보겠습니다.<br>
정밀도(precision) = 양성으로 예측한 데이터 중 실제로 양성인 비율, 포괄성(recall)
 = 실제로 양성인 데이터 중 양성으로 예측한 비율, *F*<sub>1</sub> = 2 × 정밀도 ×
 포괄성 / (정밀도 + 포괄성), 또한 AUC(area under receiver operating
 characteristic curve, ROC 커브 밑면적)를 이용해서 정밀도와 포괄성을 한번에
 평가하는 방법도 있습니다.<br>
유명한 랭킹 평가 방식 2가지를 소개합니다. 정밀도@K(precision@K) 방식은 항목들의
 랭킹을 구한 후 앞에서부터 *K*번째까지의 결과 중에서 몇개가 올바른지 검사합니다.
 NDCG(Normalized Discounted Cumulative Gain)는 중요도를 순위에 따라 바꾸어가며
 평가합니다. 두 방법 모두 검색 엔진의 평가에 많이 쓰입니다.

## Part 2 머신러닝 주요 모델

### CHAPTER 3 데이터와 문제

텍스트 데이터와 비슷하게 데이터의 순서가 중요한 수치 데이터도 있습니다. 예를
 들면 주식 데이터를 생각해볼 수 있는데, 굉장히 노이즈가 많기 때문에 노이즈에
 강한 순차 모델, 예를 들면 칼만 필터와 같은 모델을 사용합니다.

이런 경우에는 학습이 잘되지 않는 부분을 머신러닝 시스템이 사람에게 질문하는
 방식으로 학습 성능을 높이는 액티브 러닝(active learning) 기법을 사용하는 것도
 좋습니다.<br>
이를 해결하려면 데이터양이 많은 레이블에서 임의로 데이터를 누락시켜(과소표집)
 데이터의 균형을 맞추거나, 데이터가 적은 경우에도 잘 동작하는 원샷러닝(one-shot
 learning, 또는 원샷학습) 등의 기법을 사용할 수 있습니다.<br>
데이터가 들어오는데 새로운 레이블이 끊임없이 생성되는 경우에는
 제로샷러닝(zero-shot learning, 또는 제로샷학습)을 사용합니다.

이럴 때는 각 데이터를 평균이 0, 표준편차가 1인 데이터로 바꾼 후 비교하면
 효과적입니다. 이런 식으로 데이터를 변환하는 방법을 z-접수 표준화(z-score
 standardization)라고 합니다. z-점수 표준화 외에 데이터를 변환하는 방법으로
 척도화와 벡터 정규화가 있습니다. 척도화(scaling)는 피처의 최댓값 및 최솟값을
 이용하여 피처값의 범위를 조정하는 방법입니다. 벡터 정규화는 한 데이터가 가지는
 피처 벡터의 노름(norm)이 1이 되게 하는 방법입니다.<br>
카테고리 데이터이면서 카테고리에 순서가 있는 데이터형을 서수(ordinal) 데이터라고
 합니다. 답변이나 별점을 1부터 5의 숫자로 표시하기는 하지만 가능한 별점이
 카테고리로 정해져 있으므로 이는 수치 데이터가 아닙니다. 하지만 각 카테고리가
 동등하지 않다는 점에서 카테고리 데이터도 아닙니다. 이러한 경우에는 일반적으로
 (*t* - 1/2) / *M* 식을 이용하여 0과 1 사이의 값으로 표준화합니다.

회귀 문제를 푸는 기법으로는 통계학에서도 널리 쓰이는 선형 회귀, 가우시안
 프로세스 회귀, 칼만 필터가 유명합니다.<br>
분류 문제를 풀 때는 기본적으로 로지스틱 회귀, 서포트 벡터 머신, 신경망 등을
 사용합니다. 그리고 입력이 시간에 따라 변화하는 데이터를 모델링하는 경우에는
 CRF나 RNN 등을 사용합니다.<br>
대표적인 군집화 기법으로는 K-평균 군집화(K-means clustering)와 평균이동
 군집화(mean shift)가 있습니다. 문서를 토픽에 따라 군집화하는 방법은 토픽
 모델링이라고 부르는데, LDA(latent dirichlet allocation, 잠재 디리클레 할당)라는
 기법이 유명합니다.<br>
표현형학습(임베딩학습)은 풀고자 하는 문제에 적합한 표현형(representation)을
 데이터로부터 추출하는 것을 말합니다. 표현형은 피처보다 훨씬 더
 간단합니다(표현형의 차원이 훨씬 더 낮습니다). 피처는 높은 차원을 가지는 경우가
 많기 때문에 많은 부분이 0으로 차 있습니다(희박하다고 표현합니다). 표현형은 훨씬
 낮은 차원으로서 데이터를 잘 설명해야 하기 때문에 밀집도가 높습니다. 대표적인
 임베딩 학습으로는 문서 처리에 강점을 보이는 word2vec 모델과 그 파생 모델, 행렬
 분해가 있습니다.

### CHAPTER 4 구매 이력 데이터를 이용한 사용자 그룹 만들기

크게 중심 기반 군집화(prototype-based clustering), 계층적 군집화(hierarchical
 clustering), 밀도 기반 군집화(density-based clustering)로 나눌 수 있습니다.
 대표적인 밀도 기반 군집화 알고리즘에는 평균이동 군집화와 DBSCAN(density-based
 spatial clustering of application with noise, 디비스캔)이 있습니다.<br>
민코스키 거리, 마할라노비스 거리(Mahalanobis distance, √(*x*<sub>*i*</sub> -
 *y*<sub>*i*</sub>)<sup>*T*</sup>*S*<sup>-1</sup>(*x*<sub>*i*</sub> -
 *y*<sub>*i*</sub>), 공분산행렬, *Cov*(*X*,*Y*) = *Cov*(*Y*,*X*))

### CHAPTER 5 문서 분석 시스템 만들기

참고로 단어 빈도나 카운팅 피처의 경우 백-오브-워즈(bag-of-words) 피처라고
 부르기도 합니다.<br>
TF-IDF(Term-Frequency Inverse-Document Frequency) 방법을 이용하면 단어의
 희귀도를 고려해서 피처를 생성할 수 있습니다. 단어 빈도 × 역문서
 빈도(log([1+]전체 문서 수/*i*번째 단어가 나타나는 문서 수))<br>
로지스틱 함수(1/(1 + *e*<sup>-*x*</sup>))는 선형 회귀의 연속적인 숫자값을
 확률값으로 변환합니다. 좀 더 복잡한 로지스틱 회귀에는 다항 로지스틱 회귀가
 있습니다. 이를 소프트맥스 회귀라고도 부릅니다.<br>
SVM 모델은 데이터와 경계선(결정 경계선, decision boundary) 사이의 최소
 간격(margin)을 최대화하는 경계선을 고른다.

[LDA][lda]는, 문서가 어떤 퇵을 가질 확률, 각 우치의 단어가 어떤 토픽에
 해당할지의 확률, 그 토픽에 따라 단어가 어떤 확률로 생성될지 정의해서 문서를
 확률 모델로 설명한다. 문서 데이터와 비슷한 양상을 보이는 데이터가 있다면 토픽
 모델링을 적용해서 내용을 간추리는 데 도움이 될 수도 있습니다. 예를 들어 한
 사용자의 쇼핑 정보를 문서로 간주하고 구매 항목을 단어로 취급하면 여러 상품을
 토픽처럼 묶어서 분석할 수 있습니다.

이 절에서는 두 가지 대표적인 문법 분석 방법에 대해 알아보겠습니다. 첫 번째는
 문서 내의 각 단어의 품사를 파악하는 품사 태깅(POS, Part of Speech, Tagging)
 문제고, 두 번째는 각 단어가 어떤 의미 단위에 속하는지(예를 들어 장소인지
 사람인지) 파악하는 고유명사 추출(NER, Named Entity Recognition)입니다.<br>
실제로 사용할 때는 구조가 훨씬 더 복잡해집니다. 예를 들어 망각 게이트나 입력
 게이트가 상태와 입력만 고려하는 것이 아니라 전 단계의 출력값까지 고려하거나,
 아니면 몇 단계 전의 입력을 고려하기도 합니다. 또한 입력과 출력 사이에 LSTM
 박스를 하나만 두는 것이 아니라 박스를 층층이 쌓아서 더 복잡한 학습에
 사용합니다(보통 딥러닝은 이렇게 복잡한 구조를 지칭합니다). 반면 LSTM의 망각과
 입력 게이트를 묶어서 망각하는 만큼 기억을 하는 GRU(Gated Recurrent Unit) 같은
 약간 더 간단한 모델도 있습니다.<br>
word2vec의 목적 함수로는 스킵-그램과 COW가 있습니다.<br>
스킵-그램(skip-gram) : 단어 하나(*x*<sub>*t*</sub>)를 받아서 그 주변에 같이
 나타날 확률이 높은 단어들(context)을 구합니다. 즉,
 *p*(*context*|*x*<sub>*t*</sub>)를 구합니다.<br>
COW(continuous bag of words) : 주변 단어들을 받아서 그 단어들과 같이 나타날
 확률이 높은 단어를 구합니다. 즉, *p*(*x*<sub>*t*</sub>|*context*)를
 계산합니다.<br>
소프트맥스를 모든 단어에 대해 계산하면 시간이 굉장히 오래 걸립니다. 이를
 해결하는 방법으로 각 리프가 단어로 된 이진 탐색 트리를 만든 후 전체 단어가 아닌
 중간 노드의 소프트맥스 값을 계산하여 리프 노드의 소프트맥스를 추정하는 계층적
 소프트맥스나, 주변 단어와 주변에 속하지 않는 단어들을 이용하여 이항 로지스틱
 회귀를 학습하는 네거티브 샘플링이 있습니다.

[lda]: http://jmlr.csail.mit.edu/papers/v3/blei03a.html

### CHAPTER 6 영화 추천 시스템 만들기

이 장에서는 내용 기반 추천 시스템에서 유사도 계산에 사용하는 자카드 계수와
 코사인 유사도, 협업 필터링에 사용하는 수학적 기법인 행렬 분해에 대해
 알아봅니다.

코사인 유사도는 문서 안의 문자열의 빈도를 사용합니다. 코사인 유사도는 벡터의
 크기를 무시하고 각도를 이용하여 유사도를 계산합니다.<br>
편집 거리는 허용하는 연산에 따라 종류가 나뉘는데, 예를 들어 대체 연산만 허용하는
 해밍 거리와 삽입, 삭제, 대체 연산을 허용하는 레벤슈타인 거리 등이 있습니다.
 문자열 비교 시 가장 자주 사용하는 거리는 레벤슈타인 거리(Levenshtein
 distance)입니다. 편집 거리는 이렇게 시리즈물의 제목을 찾는 데 유용합니다. 또한
 문서 안의 오탈자를 고려하여 유사도를 측정하는 데도 유용합니다.

두 필터링 방법 모두 상품, 사용자, 별점의 관계를 이용합니다. 이런 관계를 행렬로
 표시한 것을 유틸리티 행렬(utility matrix)이라고 합니다. 별점 예측 기법으로는
 사용자/상품 기반 협업 필터링과 잠재성 요인 모델이 있으며, 성능 평가에는 평균
 제곱근 편차를 사용합니다.<br>
따라서 상품에 별점과 같은 선호도 정보가 있을 때는 코사인 유사도를 계산하는 것이
 좋습니다. 계산 절차는 다음과 같습니다. 1. 열마다 평점 평균값 구하기 2. 각
 별점에서 평균값 빼기 3. 미지항을 0으로 채우기<br>
카테고리나 가격 같이 피처가 정해져 있는 상품은 유사도 계산이 더 쉬우므로
 실전에서는 사용자 기반 협업 필터링보다 상품 기반 협업 필터링을 더 자주
 씁니다.<br>
잠재성 요인 모델(latent factor model)은 유틸리티 행렬이 두 행렬(사용자 요인
 행렬, 별점 요인 행렬)의 곱으로 이루어져 있다고 가정합니다. 이 절에서는 잠재성
 요인 모델에서 행렬을 분해하는 한 방법인 특잇값 분해에 대해 살펴보겠습니다. 이
 방법에는 여러 가지가 있지만, 그중에서 가장 널리 사용되는 방법은 특잇값
 분해(Singular Value Decomposition, SVD)와 7.3.2절 '코너'에서 공부할 고유벡터
 분해(eigen vector decomposition)입니다. *m*×*m* *m*×*n* *n*×*n*<br>
특잇값 분해는 계산에 시간이 많이 걸리기 때문에 일반적으로 고윳값이 큰 고유벡터
 *k*개ㅏㄴ으로 *U*와 *V*를 만듭니다. 이를 절단 특잇값 분해(truncated SVD)라고
 합니다. 적은 수의 고유벡터를 사용하여 만들어진 *U*와 *V*의 곱은 원래의 행렬과
 완전히 같지는 않지만 원래의 행렬에 가까운 행렬(근사 행렬)을 만들어낼 수
 있습니다. 따라서 절단 특잇값 분해는 차원 축소에 이용되기도 합니다. *m*×*k*
 *k*×*k* *k*×*n*<br>
여기서는 자주 쓰이는 교대 최소제곱법(alternating least squares, ALS)과
 경사하강법에 대해 설명하겠습니다.

내용 기반 추천 시스템은 추천 결과를 사람이 이해하기 쉽고, 한 사용자의 구매
 이력에만 의존하므로 다른 사용자의 정보를 필요로 하지 않습니다. 또한 새로 추가된
 항목에 별점이나 리뷰가 없어도 추천할 수 있습니다. 반면 협업 필터링은 다른
 사용자의 정보도 같이 이용하므로 더 다양한 범위의 추천을 할 수 있습니다. 또한
 행렬 분해 방법을 통해 잠재적 구매 원인 분석도 가능합니다. 하지만 이 방법은 다른
 사용자의 정보가 없거나 별점 정보 혹은 구매 정보가 없으면 사용하기 어렵습니다.
 이를 콜드-스타트 문제(cold-start problem)라고 합니다. 따라서 실무에서는 두
 방법을 병행해서 이용합니다.

### CHAPTER 7 이미지 인식 시스템 만들기

### CHAPTER 8 머신러닝의 다양한 문제점 해결하기

## Part 3 머신러닝 시스템 구현

### CHAPTER 9 머신러닝 소프트웨어 소개

### CHAPTER 10 구매 이력 데이터를 이용한 사용자 그룹 만들기 - 실전

### CHAPTER 11 문서 분석 시스템 만들기 - 실전

### CHAPTER 12 영화 추천 시스템 만들기 - 실전

### CHAPTER 13 이미지 인식 시스템 만들기 - 실전

