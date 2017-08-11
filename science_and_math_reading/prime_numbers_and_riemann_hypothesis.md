# [소수와 리만 가설: 질서와 패턴을 찾고자 하는 이들의 궁극적 도전 대상][homepage], 배리 메이저, (SageMath의) 윌리엄 스타인 저, 권혜승 역, 승산 (2017)

[원서 출판사 페이지][on_the_cambridge_university_press_homepage], [저자 페이지][on_the_author_homepage], [강의 비디오][lecture_videos]

[homepage]: http://blog.naver.com/seungsan_b/221040972866
[on_the_cambridge_university_press_homepage]:https://www.cambridge.org/core/books/prime-numbers-and-the-riemann-hypothesis/37616412CC94D2349D63ED18E108E39A
[on_the_author_homepage]: http://wstein.org/rh/
[lecture_videos]: http://library.fora.tv/2014/04/25/Riemann_Hypothesis_The_Million_Dollar_Challenge

## 1부 리만 가설

### 1. 고대, 중세, 현대의 수에 관한 생각들

### 2. 수소란 무엇인가?

### 3. "이름이 붙은" 소수

메르센 소수(Mersenne prime) - *M*=2<sup>*n*</sup>-1 형태의 어떤 수가 소수라면,
 그 지수 *n*도 소수<br>
페르마 소수(Fermat prime) - *M*=2<sup>*n*</sup>+1 형태의 어떤 수가 소수라면, 그
 지수 *n*이 2의 거듭제곱

### 4. 체(sieves)

### 5. 누구라도 물을 수 있는 소수에 관한 질문들

그런 소수들의 순서쌍이 무한히 많다는 추측은 쌍둥이 소수 추측(Twin Prime
 Conjecture)으로 알려져 있다.

### 6. 소수에 관한 더 많은 질문들

짝숫곱 수는 짝수 개수의 소수들의 곱으로 표현되는 수이고, 홀숫곱 수는 홀수 개수의 소수들의 곱으로 표현되는 수이다.

### 7. 얼마나 많은 소수가 존재하는가?

실제 소수들의 개수를 나타내는 묘하게 뷸규칙한 곡선이 소수의 계단(staircase of
 primes)이다. *X*까지의 소수들의 누적 개수를 π(*X*)라 한다.

### 8. 멀리서 바라본 소수들

어떤 매끈한 곡선이 소수의 계단에 잘 맞는 그럴듯한 근사 곡선인가?

### 9. 순수 수학과 응용 수학

소수의 근본을 이루고, 소수보다 더 기본적인 수학적인 개념들이 존재하는가? 소수의
 본성이 지닌 명백한 복잡성을 설명할 개념들이 존재하는가?

### 10. 최초의 확률적 추측

가우스는 주어진 대략적인 크기인 *X*까지의 구간에 속한 소수들의 밀도가 평균적으로
 1/log(*X*)처럼 보인다는 사실을 매우 이른 시기인 1792년 혹은 1793년에 이미
 알았다고 주장했다. 가우스는 *X*까지의 소수의 개수의 기댓값이, 2부터 *X*까지
 1/log(*X*)의 그래프 아래 부분의 넓이에 의해 근사된다고 추측했다.

### 11. "좋은 근사"란 무엇인가?

### 12. 제곱근 오차와 임의보행(random walk)

(충분히 많이) 이런 식으로 왔다 갔다 한 후, 기준점에서 떨어져 있을 평균 거리가
 √*X*에 비례한다는 것이다.(사실, 평균은 √(2/π)⋅√*X*이다).<br>
수를 세는 동안 주기적으로 그런 오차가 생기고, 더 많게 세거나 더 적게 셀 가능성이
 똑같이 임의적이라면, 위원들의 계산 정확도를 임의보행으로 모델링할 수 있다. 이는
 제곱근 오차를 따를 것이다.
 
### 13. 리만 가설이란 무엇인가? (첫 번째 공식화)

π(*X*)의 근삿값이 종종 Li(*X*)라 나타내는, 2부터 *X*까지 1/log(*X*)의 그래프
 아래 영역의 넓이일 거라고 추측하게 만든다. "Li('라이'라고 발음)"는 로그
 적분(Logarithmic integral)의 약자다.<br>
|Li(*X*)-π(*X*)|를 다음과 같은 규칙을 따르면서 대략 *X* 걸음을 가는 임의보행의
 결과로 생각할 수 있다. *N*이 소수가 아니라면 1/log(*N*)피트 거리만큼 동쪽으로
 가고, *N*이 소수라면 1-1/log(*N*)피트 거리만큼 서쪽으로 간다. 그러면 *X*걸음
 후에 기준점으로부터의 거리는 대략 |Li(*X*)-π(*X*)|피트이다.<br>
**리만 가설 (첫 번째 공식화)** 임의의 실수 *X*에 대하여 *X*보다 작은 소수들의
 개수는 대략 Li(*X*)이다. 그리고 이는 본질적으로 제곱근 정확도를 가진
 근삿값이다.

### 14. 미스터리는 오차항으로 옮겨간다

리만 가설의 첫 번째 공식화가 오차항 |Li(*X*)-π(*X*)|을 집중적으로 조명하므로,
 이를 면밀히 조사할 필요가 있다. 왜냐하면 결국 우리는 소수의 개수를 세는 것에만
 관심이 있는 게 아니라, 그 구조에 대하여 가능한 한 많은 것을 이해하고 싶기
 때문이다.

### 15. 세자로 스무딩(Cesàro Smoothing)

### 16. |Li(*X*)-π(*X*)| 보기

Li(*X*)=π(*X*)가 성립하는 그 "첫 번째" *X*를 (리틀우드의 학생이었던) 남아프리카
 수학자 스탠리 스큐스의 이름을 따서
 [스큐스 수(Skewes Number)][skewes_number]라고 부른다. 그는 1933년에 (리만
 가설을 조건부로!) 그 수에 대한 최초의 (무시무시하게 큰) 상한값을 제시했다.
 계속해서 꾸준히 그 상한값이 개선되고 있긴 하지만, 현재까지 알아낸 바로는 그저
 스큐스 수가 다음 범위 안에 있다는 정도뿐이다. 10<sup>14</sup>≤스큐스
 수<10<sup>317</sup>

[skewes_number]: https://en.wikipedia.org/wiki/Skewes'_number

### 17. 소수 정리

정확성은 좀 떨어져도 이 명제의 장점은 현재 그것이 사실임이 밝혀졌다는 것이다.
 실제로 입증된지 백 년도 넘은 이 명제는 다음과 같은 이름으로 알려져 있다.<br>
**소수 정리(The Prime Number Theorem** Li(*X*)와 π(*X*)는 같은 속도로 무한대로
 간다. Li(*X*)와 *X*/log(*X*)는 같은 속도(비가 1)로 무한대로 가기 때문에, "같은"
 정리를  *X*/log(*X*)와 π(*X*)는 같은 속도로 무한대로 간다고 표현할 수 있다.<br>
소수 정리 증명의 역사에서 획기적 사건은 *X*/log(*X*)와 π(*X*)가 비슷한 속도(비가 상수)로 무한대로 간다는 사실을 보인 파프누티 르보비치 체비셰프의 이전
 연구였다([Chebyshev function][chebyshev_function] 참고).<br>
현재까지의 해석에 의하면, 리만 가설이 이 주제의 다양한 분야들에서 중요한 핵심
 열쇠로 자꾸자꾸 나타난다. 당신이 리만 가설을 가설로 받아들이면, 자유자재로 쓸
 수 있는 엄청나게 강력한 수단을 가진다. 그러나 리만 가설은 또한 경이로울 정도로
 변화무쌍한 성질을 지니고 있다. 리만 가설을 수학적으로 표현하는 방식은 무수히
 많고, 그 표현들은 모두 서로 동치이다.

[chebyshev_function]: https://en.wikipedia.org/wiki/Chebyshev_function

### 18. 소수의 계단에 담긴 정보

### 19. 소수의 계단 손보기

우선 정확히 *x*=1과 모든 소수의 거듭제곱 *x*=*p*<sup>*n*</sup> (*n*≥1)에서 한
 칸씩 올라가는 계단을 만든다. 우리의 계단은 *x*=0일 때 바닥에서 시작하고,
 *x*=1일 때 올라가는 계단 한 칸의 높이는 log(2π)다. *x*=*p*<sup>*n*</sup>에서
 올라가는 계단의 높이는 log *p*이다.<br>
**리만 가설 (두 번째 공식화)** 함수 *ψ*(*X*)는 함수 *f*(*X*)=*X*에 가까운,
 본질적인 제곱근 근사다.

### 20. 도대체 컴퓨터 음악 파일과 데이터 압축, 소수가 서로 무슨 상관이 있을까?

*[Music: a Mathematical Offering][music_a_mathematical_offering]*, Cambridge
 University Press (2006)<br>
푸리에 변환

[music_a_mathematical_offering]: http://homepages.abdn.ac.uk/mth192/pages/html/maths-music.html

### 21. "스펙트럼(Spectrum)"이라는 단어

오늘날 스펙트럼의 의미는 대부분, 분석하고자 하는 것의 구성 성분을 명확히 보게 해
 주는 어떤 절차나 분석과 관련 있다.

### 22. 스펙트럼과 삼각함수들의 합

### 23. 스펙트럼과 소수의 계단

### 24. 1부의 독자들에게

## 2부 초함수(Distribution)

### 25. 미적분학은 기울기가 없는 그래프의 기울기를 어떻게 구할 수 있을까?

### 26. 초함수: 무한대로 보내더라도 근사함수 뾰족하게 만들기

일반화된 함수나 초함수(distribution)는 1) 진ㅊ짜 함수가 아니라면 모든 실수에서
 "유한한 값"을 갖지 않을 수도 있다. 2) 그럼에도 불구하고 "그래프" 아래 넓이가 잘
 정의된다. 사실, 일반화된 실변수 함수 *D*(*t*)란 (형식적으로는) 임의의 유한한
 구간에서 마치 어떤 함수의 적분과 비슷하게 행동하는 수학적인 값을 정한 규칙일
 따름이다.

### 27. 푸리에 변환: 두 번째 방문

*f*ˆ(*θ*)=∫<sub>-∞</sub><sup>+∞</sup>*f*(*t*)cos(-*θt*)*dt*

### 28. 델타 함수의 푸리에 변환은 무엇인가?

*δ*ˆ<sub>0</sub>(*θ*)=1<br>
*d*<sub>*x*</sub>(*t*)=(*δ*<sub>*x*</sub>(*t*)+*δ*<sub>-*x*</sub>(*t*))/2<br>
*d̂*<sub>*x*</sub>(*θ*)=cos(*xθ*)

### 29. 삼각급수

삼각급수(trigonometric series)
 *F*(*θ*)=∑<sub>*k*=1</sub><sup>∞</sup>*a*<sub>*k*</sub>cos(*s*<sub>*k*</sub>⋅*θ*)

### 30. 3부에 대한 간단한 개요

첫 번째 삼각급수의 항들의 진동수가 두 번째 급수의 스파이크 값들을 주고, 또 그
 반대로 두 번째 급수의 진동수들이 첫 번째 급수의 스파이크 값들을 주는 방식으로,
 둘은 서로서로 연관되어 있는 듯하다. 이는 푸리에 변환 이론에서처럼 일종의
 쌍대성(duality)을 보이고 있다.<br>
*F*(*t*) := -∑<sub>*p*<sup>*n*</sup></sub>(log(*p*)/*p*<sup>*n*/2</sup>)cos(*t*log(*p*<sup>*n*</sup>)) "점점 더 높아지는 정점"들을 리만 스펙트럼이라 부른다.<br>
*H*(*t*) := 1 + ∑<sub>*θ*</sub>cos(*θ*log(*s*)) 진동수는 앞에서 리만
 스펙트럼이라 부른 값으로 주어지고, 소수의 거듭제곱의 로그에 집중되는 "점점 더
 높아지는 정점"을 가질 것이다.<br>
이 쌍대성은 소수의 거듭제곱들의 종잡을 수 없는 분포에 내재된 정보가 리만
 스펙트럼 안에 어떤 식으로든 "들어가 있고", 또 반대로 그 미스테리한 수들의 수열
 안에 주어진 정보를 소수의 거듭제곱들의 수열로부터 얻을 수 있음을 강조한다.

## 3부 소수의 리만 스펙트럼

### 31. 정보를 잃지 않고서

이 시점에서 *ψ*(*X*)가 지닌 소중한 정보는 파괴하지 않으면서 *ψ*(*X*)를 조금 더
 변형해 보자. *ψ*(*X*)를 일반화된 함수, 즉 초함수로 대체할 것이다. 이를
 *Φ*(*t*)라고 나타내자. 이 함수는 소수의 로그값이 모든 양의 정수배에서
 받침(support)를 가지며, 이 이산 집합의 여집합에서는 0이다. 우리 책의 목적을
 생각할 때, *Φ*(*t*)의 구조에 대해 자세히 살펴보는 것보다는 (a) *Φ*(*t*)에
 *ψ*(*X*)의 귀중한 정보가 모두 포함되어 있음을 유의하고, (b) *Φ*(*t*)의 푸리에
 변환인 삼각급수의 스파이크 값들에 깊이 관심을 기울이는 것이 더 중요할 것이다.

### 32. 소수에서부터 리만 스펙트럼으로 가는 길

*Φ*(*t*) = *e*<sup>-*t*/2</sup>*ψ*′(*t*) :=
 2∑<sub>*p*<sup>*n*</sup></sub>*p*<sup>-*n*/2</sup>log(*p*)*d*<sub>*n*log(*p*)</sub>(*t*)<br>
*Φ*ˆ(*θ*) =
 2∑<sub>*p*<sup>*n*</sup></sub>*p*<sup>-*n*/2</sup>log(*p*)cos(*n*log(*p*)*θ*)<br>
*s*=½+*iθ*에 대하여 ∑<sub>*m*=2</sub><sup>∞</sup>*Λ*(*m*)*m*<sup>-*s*</sup> +
 ∑<sub>*m*=2</sub><sup>∞</sup>*Λ*(*m*)*m*<sup>-*s̄*</sup>라 타나낼 수 있다. 이때
 *Λ*(*m*)은 폰-망골트 함수(von-Mangoldt function)다.<br>
[The lowest zeros of Riemann's zeta are in front of your eyes][lowest_zeros_of_riemanns_zeta_are_in_front_of_your_eyes]

[lowest_zeros_of_riemanns_zeta_are_in_front_of_your_eyes]: http://www.dam.brown.edu/people/mumford/blog/2014/RiemannZeta.html

### 33. 얼마나 많은 *θ*<sub>*i*</sub>들이 존재할까?

*θ*<sub>1</sub>에서 한 계단, *θ*<sub>2</sub>에서 또 한 계단 위로 올라가는 식으로
 계단을 만들어 보자. 실제로 리만 가설이 맞다고 가정하면, 계단은 곡선
 *T*/(2π)log(*T*/(2π*e*))와 아주 비슷함(오차항이 어떤 상수 곱하기 log*T*에 의해
 유계이다)을 알 수 있다.<br>
[Andrew Odlyzko: Tables of zeros of the Riemann zeta function][tables_of_zeros_of_the_reimann_zeta_function]

[tables_of_zeros_of_the_reimann_zeta_function]: http://www.dtc.umn.edu/~odlyzko/zeta_tables/

### 34. 리만 스펙트럼에 대한 추가 질문들

[Mathematical Constants and computation][mathematical_constants_and_computation]<br>
리만 스펙트럼에서도 인접한 *θ*들 사이의 쌍 상관 함수(pair correlation function)
 및 간격의 통계를 계산할 수 있다. 다이슨이 지적했듯이, 리만 스펙트럼으로부터
 얻는 분포는 확률 유니터리 행렬(random unitary matrix)의 고윳값들의 분포와
 유사하다. 이로부터 정수론 및 다른 수학분야에서 추측의 강력한 원천으로 활용되는
 확률 행렬 발견법(random matrix heuristics)이라는 이론이 탄생하였다.
 [힐베르트-포여 추측][hilbert_polya_conjecture](Hilbert–Pólya conjecture)이라
 알려진 이론의 관점에서 볼 때, 리만 스펙트럼과 행렬의 구윳값 사이에 어떤 관계가
 있다면 리만 가설을 이해하려 할 때 정말 흥미진진할 것이다.

[mathematical_constants_and_computation]: http://numbers.computation.free.fr/Constants/constants.html
[hilbert_polya_conjecture]: https://en.wikipedia.org/wiki/Hilbert–Pólya_conjecture

### 35. 리만 스펙트럼에서부터 소수로 가기

## 4부 리만으로 돌아가다

### 36. 스펙트럼으로부터 어떻게 π(*X*)를 만들까?

### 37. 리만의 예견대로 제타 함수가 소수의 계단을 리만 스펙트럼과 연결하다

### 38. 제타 함수의 동반자들

