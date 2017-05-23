# [자바 성능 튜닝][homepage], 스캇 오크스 저, 최가인 역, 비제이퍼블릭 (2016)

[source code][source_code]<br>
17년 상반기 우편통신(SDP)교육, 번역이 부자연스럽다

[homepage]: http://www.bjpublic.co.kr/skin12/product_list.php?boardT=v&goods_data=aWR4PTE5MyZzdGFydFBhZ2U9MCZsaXN0Tm89MTQwJnRhYmxlPXJlZF9nb29kcyZwYWdlX2lkeD05JnNlYXJjaF9pdGVtPQ==||
[source_code]: https://github.com/ScottOaks/JavaPerformanceTuning

## 1. 서론

## 2. 성능 테스트 접근법

JVM의 많은 요소는 머신 내의 모든 리소스를 사용 가능하다는 가정하에서 디폴트로
 설정되며, JVM 단독으로 테스트하면 잘 동작할 것이다. ... 그 성능은 홀로 동작할
 때와는 다르게 측정될 것이다.<br>
이와 같은 요청에 관련된 코드는 전통적인 마이크로벤치마크에 비해 많다. 여기에는
 소켓 관리 코드, 요청을 읽어들이는 코드, JSP를 찾는 (그리고 가능하게 컴파일하는)
 코드, 응답을 읽는 코드 등이 있다. 그러나 이 종류의 테스트는 매크로벤치마크도
 아니다. 보안 체계도 없고(예, 사용자는 애플리케이션에 로그인하지 않는다) 세션
 관리도 없고 다른 자바 EE 기능들을 이용하지도 않는다. 실제로는 애플리케이션의
 일부일 뿐이므로 두 벤치마크의 중간 즈음에 자리매김하며 이를
 메조벤치마크(Mesobenchmarks)라고 한다. 실제 작업 일부를 수행하지만 완전한
 형태를 갖추지 않은 애플리케이션인 벤치마크를 가리킬 때 사용하는 용어다.<br>
응답 시간 테스트와 처리율 테스트의 차이점은 응답 시간 테스트의 클라이언트
 스레드가 동작 사이의 일정 기간 동안 대기(sleep)한다는 점이다. 이 시간은 사고
 시간(think time)이라고 일컬어진다.<br>
[Faban][faban] 부하 생성기 `fhb -W 1000 -r 300/300/60 -c 25 http://host:port/StockServlet?stock=SD0`<br>
회귀 테스트에서 기존 코드는 베이스라인(baseline)으로 알려져 있고 신규 코드는
 표본(specimen)이라고 부른다.<br>
두 테스트 결과가 다른지를 정확하게 결정하는 데는 감지된 차이가 임의의 결과가
 아니라고 확신시켜줄 통계 분석이 필요하다. 이를 철저히 하기 위한 방법은 결과를
 비교하는 데 스튜던트 t 테스트(Student's t-test)를 이용하는 것이다.<br>
모든 머신과 프로그램에서 가능한 통계 자료를 전부 수집하는 자동 테스트 시스템은
 성능 저하 요인에 대한 필요한 단서를 제공할 것이다.

[faban][faban]: http://faban.org/

## 3. 자바 성능 도구 상자

(UNIX) `sar`, `vmstat 1`, `iostat -xm 5`, `prstat`, (Windows)
 `typeperf -si 1 "\System\Processor Queue Length`<br>
network - `netstat`, `nicstat 5`, `typeperf`, Windows resource monitor<br>
`jcmd`, `jconsole`, `jhat`, `jmap`, `jinfo`, `jstack`, `jstat`, `jvisualvm`<br>
`jcmd pid` - `VM.uptime`, `VM.system_properties` (`jinfo -sysprops`),
 `VM.version`, `VM.command_line`, `VM.flags [-all]`
 (`java -XX:+PrintFlagsFinal`, `jinfo -flags`, `jinfo -flag PrintGCDetails`,
 `jinfo -flag -PrintGCDetails`)<br>
`jstack` (`jcmd Thread.print`)<br>
장착형 프로파일러는 (호출 수를 세기 위한 코드를 삽입해서) 로드된 클래스의 바이트
 코드 순서를 변경시키면서 동작한다. 예를 들어 JVM은 작은 메서드를 인라인으로
 만들므로 작은 메서드 코드가 실행될 때 호출될 필요가 없다. 컴파일러는 코드의
 크기를 바탕으로 결정을 내린다. 코드가 장착되는 방식에 따라 인라인 대상이 되지
 않을 수도 있다. 이로 인해 장착형 프로파일러가 특정 메서드의 기여도를 과대
 평가할 수 있다.<br>
`ImmutableMap.get()` 메서드가 샘플링 프로파일이 아닌 장착형 프로파일에 나타난
 기술적인 이유도 흥미롭다. 자바에서 샘플링 프로파일러는 스레드가
 세이프포인트(safepoint)에 있을 때만--근본적으로 메모리에 할당되어 있을 때마다
 스레드의 샘플을 뜰 수 있다. `get()` 메서드는 절대로 세이프포인트에 도달할 수
 없으므로 샘플로 얻을 수 없다. 이건 샘플링 프로파일이 프로그램 실행에 있어서
 해당 메서드의 기여도를 과소 평가할 수 있다는 점을 의미한다.<br>
Oracle Solaris Studio<br>
네이티브 프로파일러로 얻는 정보 중 핵심 요소는 애플리케이션이 GC에서 쓰고 있는
 시간의 양이다. 자바 프로파일링 도구에서 GC 스레드의 영향은 어디에서도 찾아볼 수
 없다. (CPU 제약을 받는 머신에서 테스트가 실행되지 않는 한, 컴파일러 내에서
 시간을 많이 쓴다 해도 문제가 되지 않을 것이다. 그 스레드는 머신상의 CPU 한 개를
 많이 소비하지만 컴파일은 백그라운드에서 일어나므로 가용 CPU가 더 있는 동안에는
 애플리케이션 자체는 영향을 받지 않는다.)<br>
자바 미션 컨트롤(Java Mission Control, `jmc`)은 자바의 오픈 소스는 아니고 상용
 라이선스로만 사용할 수 있다. 자바 미션 컨트롤의 주요 기능은 자바 플라이트
 레코더(Java Flight Recorder, JFR)다. 원형 버퍼의 크기, 이벤트의 종류, 저장 위치
 등--이 모든 요소는 프로그램이 실행되는 동안 `jcmd` 명령어, 자바 미션 컨트롤
 GUI와 JVM 내의 다양한 인자를 통해 조정된다. 오라클 JVM 상용 버전에는 처음에
 JFR이 비활성화되어 있다. 활성화시키기 위해서는 애플리케이션의 커맨드 라인에
 `-XX:+UnlockCommercialFeatures` `-XX:+FlightRecorder` 플래그를 추가해야 한다.
 이렇게 하면 JFR을 사용할 수 있게는 되지만, 레코딩 프로세스 자체를 활성화시키기
 전까지는 레코딩이 안될 것이다.<br>
`XX:+FlightRecorderOptions=options_list`, `jcmd pid JFR.start [options_list]`,
 `jcmd pid JFR.dump [options_list]`, `jcmd pid JFR.check [verbose]`,
 `jcmd pid JFR.stop [options_list]`

## 4. JIT 컴파일러로 작업하기

JVM(과 다른 도구) 개발자들은 흔히 컴파일러를 `C1`(컴파일러 1, 클라이언트
 컴파일러)과 `C2`(컴파일러 2, 서버 컴파일러)라는 이름으로 부른다. 두 컴파일러
 간의 주요 차이점은 코드 컴파일에 있어서의 적극성이다. 클라이언트 컴파일러는
 서버 컴파일러보다 먼저 컴파일하기 시작한다. JVM이 시작할 때는 클라이언트
 컴파일러를 쓰고, 그다음에 코드가 많이 호출되면 서버 컴파일러를 사용할 수
 없을까? 이 기법이 **티어드 컴파일**이라고 불린다 (`-XX:+TieredCompilation`).
 자바 7에서는 티어드 컴파일에 몇 가지 불안정한 점이 있기 때문에 디폴트 설정이
 아니다. 특히 JVM 코드 캐시 크기를 초과하기 쉬워서 코드가 최적으로 컴파일되지
 못하게 할 수 있다. 자바 8에서는 티어드 컴파일을 기본적으로 사용할 수 있다.<br>
`-client` (32-bit 클라이언트 버전), `-server` (32-bit 서버 버전), `-d64` (64-bit
 서버 버전), 단 64-bit라면 모두 64-bit 서버 컴파일러를 사용<br>
여러분의 힙 크기가 약 3GB보다 작다면 32bit 버전의 자바는 더 빠르고 더 작은
 공간을 차지할 것이다. 매우 예외적인 경우라도 CPU의 64bit 레지스터를 이용할 수
 없기 때문에 `long`이나 `double` 변수를 광범위하게 사용하는 프로그램은 32bit
 JVM에서 더 느릴 것이다.<br>
루프가 실행되고 있을 동안 루프의 컴파일된 버전을 실행시킬 수 있는 JVM 기능을
 **스택상의 교체**(on-stack replacement, OSR)라고 부른다. 메서드와 루프가 실행될
 때마다 카운터 값이 증가하더라도 시간이 지남에 따라 주기적으로 (명확하게는 JVM이
 세이프포인트(safepoint)에 이르렀을 때) 각 카운터의 값이 감소하기 때문에 컴파일
 임계치에 도달하지 않을 수 있다.<br>
`-XX:+PrintCompilation` 컴파일 로그 (timestamp compilation_id attributes
 (tiered_level) method_name bytecode_size deopt)<br>
attributes:
* `%`: OSR 컴파일이다.
* `s`: 메서드가 동기화된다.
* `!`: 메서드는 예외 핸들러를 가지고 있다.
* `b`: 블로킹 모드에서 컴파일된다.
* `n`: 네이티브 메서드에 대한 래퍼를 위해 컴파일된다.

일반적으로 서버 컴파일러는 final 메서드가 아닌 일반 메서드도 공격적으로
 인라이닝하기도 한다. 서브 클래스가 로드되고 메서드가 오버라이드될 때 컴파일된
 코드는 호출에 응할 수 없다. 이런 코드를 "not entrant"(진입 불가)라고 정의한다.
 이처럼 최적화시켰다가 다시 최적화를 푸는 걸 "Deoptimization"(역최적화)이라고
 한다. 무효화된 채 남아있는 경우는 "made zombie"(좀비화)라 한다. 코드에 진입하지
 못하게 만드는 요소는 두 개가 있다. 하나는 인터페이스를 구현한 클래스가 변경되는
 경우이고, 다른 하나는 티어드 컴파일에서 클라이언트 컴파일러가 컴파일한 코드를
 서버 컴파일러가 컴파일한 코드를 교체할 때다.<br>
`jstat -compiler pid`, `jstat -printcompilation pid interval`<br>
다른 컴파일 로그 (timestamp compile_id COMPILE SKIPPED: reason)
* Code cache filled: `ReserveCodeCache` 플래그를 이용해서 코드 캐시 크기를 늘릴
  필요가 있다.
* Concurrent classloading: 컴파일되면서 클래스가 변경됐다. JVM은 이후에 다시
  컴파일할 것이다.

컴파일 큐에서 호출 카운터가 더 높은 메서드의 우선순위가 높다. 컴파일러 스레드
 개수는 `-X:CICompilerCount=N` 플래그를 설정해서 (앞 표에 주어진 디폴트 값으로)
 조정할 수 있다. 티어드 컴파일에서 이들의 1/3(그러나 최소 한 개)은 클라이언트
 컴파일러 큐를 처리하는 데 이용되고, 나머지 스레드(그리고 역시나 최소 한 개)는
 서버 컴파일러 큐를 처리하는 데 사용된다. 컴파일 스레드에 적용하는 또 다른
 설정은 `-XX:+BackgroundCompilation` 플래그의 값으로 디폴트는 `true`다. 이
 설정은 방금 설명한 대로 큐가 비동기 방식으로 처리된다는 것을 의미한다.
 백그라운드 컴파일도 `-XBatch`가 명시되면 비활성화된다.<br>
사실 자바 초창기 때 이 모든 메서드 호출이 성능에 미치는 영향 때문에 성능상
 정확히 이 종류의 캡슐화(encapsulation)에 대해 반대 의견이 있었다. 다행스럽게도
 JVM은 이제 이런 종류의 메서드들을 기계쩍으로 인라인으로 만든다. 실제로는 반드시
 사용해야 할 정도로 성능을 매우 향상시키긴 하지만 `-XX:-Inline` 플래그를
 사용하지 않을 수도 있다. (소스로 JVM을 컴파일한다면 `-XX:+PrintInlining`
 플래그를 포함해서 디버그 버전을 만들 수 있다.) 자주 호출된 메서드가 인라인화의
 대상이 되면 바이트 코드 크기가 325바이트보다 작아져야만(아니면
 `-XX:MaxFreqInlineSize=N` 플래그로 명시되면) 인라인될 것이다. 그렇지 않으면
 35바이트보다 작아야만(아니면 `-XX:MaxInlineSize=N` 플래그로 명시되면) 인라인
 대상이 된다.<br>
탈출 분석(Escape Analysis)이 활성화되어 있으면 (`-XX:+DoEscapeAnalysis`,
 디폴트는 `true`) 서버 컴파일러는 매우 적극적인 최적화를 몇 가지 수행한다. 탈출
 분석은 컴파일러가 수행할 수 있는 최적화 중에서 가장 복잡하다. 이건
 마이크로벤치마크가 흔히 실패하게 하는 형태의 최적화다. 탈출 분석은 때로
 부적절하게 동기화된 코드에 "버그"를 넣을 수 있다.<br>
성능에 있어 가장 좋은 상황은 메서드가 예상한 대로 티어 0 (인터프리트된 코드) →
 티어 3 (전체 C1 컴파일된 코드) → 티어 4 (C2 컴파일된 코드)<br>
`final` 키워드의 유무가 애플리케이션의 성능에 영향을 주진 않을 것이다.

## 5. 가비지 컬렉션 입문

코드를 다시 작성하는 방법을 제외하고 가비지 컬렉터 튜닝은 자바 애플리케이션의
 성능을 개선하기 위해 할 수 있는 일 중 가장 중요하다. 현재의 JVM에서 사용할 수
 있는 주요 가비지 컬렉터는 (단일 CPU 머신에서 사용하는) 시리얼 컬렉터(serial
 collector), 처리율 (throughput) (병렬) 컬렉터(parallel collector), 동시
 병렬(CMS) 컬렉터(concurrent collector)와 G1 컬렉터가 있다.<br>
상세 내역은 다소 다르지만 모든 가비지 컬렉터는 힙을 별도의 제너레이션으로 나눠서
 작업한다. 이건 올드(또는 종신(tenured)) 제너레이션, 영 제너레이션이라고 불린다.
 (때로 에덴은 전체 영 제너레이션을 참조하는 데 부정확하게 사용되기도 하지만) 영
 제너레이션은 에던(eden)과 서바이버 스페이스(survivor space)로 알려진 구간으로
 더 나뉜다. 별도의 제너레이션을 나누는 근거는 아주 단기간 동안 많은 객체가
 이용된다는 것이다. 영 제너레이션이 가득 차면 가비지 컬렉터는 모든 애플리케이션
 스레드를 멈추고 영 제너레이션을 비울 것이다. 이 동작은 마이너(minor) GC라고
 불린다. 객체가 올드 제너레이션으로 이동되면서 사실상 가득 차게 되고, JVM은 올드
 제너레이션 내에서 더 이상 사용되지 않는 객체를 찾아서 폐기시킬 필요가 있을
 것이다. 이 과정은 풀(full) GC라고 불리고 일반적으로 애플리케이션 스레드가 오래
 중지된다. 미사용 객체를 훑어보는 동안 애플리케이션 스레드를 멈추지 않고도
 처리할 수 있으므로 CMS와 G1은 동시 병렬 컬렉터(concurrent collector)라고
 불린다. 모든 애플리케이션 스레드를 멈출 필요를 최소화시키기 때문에 이들은
 저중단(low pause, 때로 정확하지 않지만 무중단) 컬렉터라고도 불린다.<br>
애플리케이션이 클라이언트 클래스 머신(단일 프로세서 머신이나 윈도우의 32비트
 JVM)에서 수행되고 있다면 이건 디폴트 컬렉터다. 시리얼 컬렉터는
 `-XX:+UseSerialGC` 플래그를 이용해서 사용할 수 있다. 대부분의 JVM 플래그와 달리
 시리얼 컬렉터는 `+` 부호를 `-` 부호로 바꾸면(예 `-XX:-UseSerialGC`라고 명시)
 비활성이 된다.<br>
서버 클래스 머신(다중 CPU 유닉스 머신과 64비트 JVM)에서 디폴트다. 처리율
 컬렉터는 올드 제너레이션을 처리할 때도 여러 개의 스레드를 이용할 수 있다. 이건
 JDK 7u4과 이후의 릴리즈 버전에서 디폴트 기능이다. 필요할 때 이용하려면
 `-XX:+UseParallelGC` `-XX:+UseParallelOldGC` 플래그를 사용하자.<br>
CMS는 마이너 GC 동안 애플리케이션 스레드를 전부 중지시키고 역시 여려 개의
 스레드로 수행한다. 그렇지만 CMS는 처리율 컬렉터를 사용하는
 것보다(`-XX:+UseParallelGC`) 영 제너리이션을 수집하는 데 (`-XX:+UseParNewGC`)
 다른 알고리즘을 사용한다. CMS는 풀 GC동안 애플리케이션 스레드를 멈추지 않고
 주기적으로 올드 제너레이션을 통해 살피고, 미사용 개체를 폐기하는 데 하나 이상의
 백그라운드 스레드를 사용한다. CMS 백그라운드 스레드가 작업을 완료하는 데 충분한
 CPU가 없거나 힙이 객체를 할당하는 데 단편화가 너무 많이 된다면 CMS는 시리얼
 컬렉터의 동작을 되돌린다. CMS는 `-XX:+UseConcMarkSweepGC` `-XX:+UseParNewGC`
 플래그(둘 다 디폴트는 `false`)를 명시하면 사용할 수 있다.<br>
G1 (또는 Garbage First) 컬렉터는 최소한으로 중지시키며 (약 4GB 이상) 큰 힙을
 처리하도록 설계됐다. 힙을 여러 개의 영역으로 나누지만 여전히 제너레이션 기반의
 컬렉터다. 이 영역의 일부는 영 제너레이션으로 구성하고, 영 제너레이션은 여전히
 모든 애플리케이션 스레드를 멈추고 올드 제너레이션이나 서바이버 스페이스로
 살아있는 객체 전부를 이동하면서 수집된다. 올드 제너레이션은 여러 영역으로
 나뉘기 때문에 G1은 한 영역에서 다른 데로 복사해서 올드 제너레이션에서 객체를
 치울 수 있으며, 일반적인 처리를 하는 동안 힙을 (적어도 부준적으로) 압축할 수
 있다. G1은 `-XX:+UseG1GC` 플래그를 명시하면 사용할 수 있게 된다(디폴트는
 `false`다). 이건 CMS보다 풀 GC를 경험할 가능성이 낮도록 설계되어 있다. G1은
 자바 7u4까지 시험 버전으로 여겨졌고 튜닝 기능 중 일부는 자바 7u10까지 사용할 수
 없었다. G1은 자바 7의 초기 릴리즈 버전과 비교해서 자바 8에 두드러진 성능상의
 이점이 있고, G1에서의 향후 작업도 CMS와 비교해서 더 작은 힙에서의 성능이
 개선되리라 예상할 수 있다.<br>
`System.gc()` 호출은 항상 풀 GC를 촉발시키므로 애플리케이션 스레드는 비교적 긴
 기간 동안 멈추게 된다. 그리고 이 메서드를 호출한다 해도 애플리케이션이 더
 효율적으로 되지 않을 것이다. 특히 성능 모니터링이나 벤치마킹을 하고 있을 때
 모든 규칙에 대한 예외가 있다. 마찬가지로 힙 분석을 할 때는 힙 덤프를 뜨기 전에
 강제로 풀 GC를 일으키는 편이 좋다. `jcmd <process is> GC.run`을 수행하거나
 `jconsole`을 이용해서 JVM에 접속하여 메모리 패널 내의 GC 수행 버튼을 클릭할 수
 있다. 다른 예외는 RMI로, 이는 분산 가비지 컬렉터의 일부로 매시간
 `System.gc()`를 호출한다. 그 타이밍은 두 개의 시스템 속성인
 `-Dsun.rmi.dgc.server.gcInterval=N`과 `-Dsun.rmi.dgc.client.gcInterval=N`에
 다른 값을 설정해서 변경할 수 있다. `N`에 들어갈 값은 밀리세컨드로, 자바 7(이전
 릴리즈 버전에서 바뀜)에서 디폴트 값은 3600000(1시간)이다. `System.gc()`
 메서드를 부정확하게 호출하는 업체의 코드를 쓰고 있다면 JVM 인자 내에
 `-XX:+DisableExplicitGC`를 포함해서 그로 인한 GC들을 완전히 막을 수 있다. 이
 플래그의 디폴트는 `false`다.<br>
처리율 컬렉터는 보통 동시 병렬 컬렉터보다 평균 응답 시간이 더 빠르지만, 동시
 병렬 컬렉터는 흔히 90%대나 99%대 응답 시간이 더 빠를 것이다. 처리율 컬렉터가 풀
 GC를 과도하게 수행하면 보통 병렬 컬렉터의 평균 응답 시간이 더 빠르다. 경험에
 따른 기본 법칙상 CMS는 4GB보다 적은 힙에서 G1보다 성능이 좋을 거라고 예상된다.

풀 GC 동안 시스템에서 스와핑이 일어난다면 그렇지 않을 때보다 중단은 몇 배 더
 길게 일어날 것이다. 그러므로 힙 크기 설정에서의 첫 번째 규칙은 머신 내의
 물리적인 메모리의 크기 보다 더 큰 힙을 지정하지 않는 것이며, JVM이 여러 개
 실행되고 있다면 모든 힙의 총합에 적용된다.<br>
`-XX:MewRatio=N` (디폴트 값 2, 영 제너레이션 크기 = 힙 크기 / (1 + NewRatio)),
 `-XmnN` (`-XX:NewSize=N` + `-XX:MaxNewSize=N`)<br>
JVM이 클래스들을 로드할 때 이 클래스들의 특정 메타데이터에 대해 계속 파악하고
 있어야 한다. 이 공간은 자바 7에서 permgen(또는 퍼머넌트 제너레이션)이라고
 불리고, 자바 8에서는 메타스페이스(Metaspace)라고 불린다. 퍼머넌트 제너레이션을
 단계적으로 폐지하는 데서 오는 이점 중 하나는 (퍼머넌트 제너레이션과 달리)
 메타스페이스는 필요한 만큼의 공간을 디폴트로 사용하고 있기 때문에 그 크기를
 정할 필요가 거의 없다는 점이다. 퍼머넌트 제너레이션의 크기는 `-XX:PermSize=N`와
 `-XX:MaxPermSize=N` 플래그로 명시한다. 메타스페이스는 `XX:MetaspaceSize=N`와
 `-XX:MaxMetaspaceSize=N` 플래그로 명시한다. 그 이름과 달리 퍼머넌트
 제너레이션에 저장된 데이터는 영속적이지 않다(그런 의미에서 메타스페이스는 훨씬
 더 나은 이름이다). 특히 클래스는 다른 요소처럼 GC 대상이 될 수 있다.<br>
`jmap`은 인자로 (자바 7에서) `-permstat`나 (자바 8에서) `-clstats`를 써서
 클래스로더에 대한 정보를 출력하는 데 이용할 수 있다.<br>
이 스레드의 개수는 `-XX:ParallelGCThreads=N` 플래그로 제어된다. 기본값은
 ParallelGCThreads = 8 + ((N - 8) * 5 / 8). 이 GC 동작은 애플리케이션 스레드가
 실행되는 걸 막기 때문에 JVM은 중지 시간을 최소화하기 위해 가능한 CPU 자원을
 많이 사용하고자 한다. 게다가 머신에 한 개 이상의 JVM이 실행되고 있다면 모든
 JVM에 대해 GC 스레드의 총 개수를 제한하는 편이 좋다.<br>
GC 알고리즘의 성능 목표를 고려해볼 때 JVM은 최적의 메모리 크기를 사용하기 위해
 힙과 제너레이션 크기를 자동 튜닝할 수 있다. 여러분에게 애플리케이션 힙의 GC
 인자와 크기 제약 사항을 정교하게 튜닝할 시간이 있다면 적응 크기 조정을 할 수
 없도록 설정가능하다. 글로벌 레벨에서 `-XX:-UseAdaptiveSizePolicy`
 플래그(디폴트느 `true`)를 끄면 적응 크기 조정을 할 수 없게 된다. JVM이
 애플리케이션 내의 공간 크기를 조정하는 방법을 보려면
 `-XX:+PrintAdaptiveSizePolicy` 플래그를 설정하자.

`-verbose:gc`나 `-XX:+PrintGC` 플래그를 쓰면 간단한 GC 로그를 생성한다(각
 플래그는 별칭이다). `-XX:+PrintGCDetails` 플래그는 좀 더 많은 정보를 다은
 로그를 만들 것이다. 때로 간단한 로그만으로는 GC로 인해 무슨 일이 일어나는지
 진단하기는 너무 어렵다. 자세한 로그에 `-XX:+PrintGCTimeStamps`나
 `-XX:+PrintGCDateStamps`를 포함하는 걸 권장하며 이를 통해 GC 이벤트의 시간을
 알아낼 수 있다. 이 두 인자의 차이점은 타임스탬프가 (JVM이 시작된 때를 기점으로)
 0에 상대적이라는 점이다. `-Xloggc:filename` 플래그로 위치를 변경할 수 있지만 GC
 로그는 표준 형식으로 출력된다. `PrintGCDetails`가 이미 활성화돼있지 않는 한
 `-xloggc`를 사용하면 자동으로 간단한 GC 로그를 남길 수 있다. 로그 파일
 로테이션은 `-XX:+UseGCLogFileRotation` `-XX:NumberOfGCLogFiles=N`
 `-XX:GCLogFileSize=N` 플래그로 제어된다. 이 플래그를 사용할 수 있는 경우 파일의
 디폴트 개수는 0(무제한을 의미)이며 디폴트 로그 파일 크기는 0(무제한을
 의미)이다.<br>
분석용 도구가 몇 가지 있긴 하지만 GC 로그 파일 자체를 직접 분석하고 정독할 수
 있다. 도구 중에는 GC 히스토그램(GC Histogram)이 있다.<br>
힙 실시간 모니터링에는 `jconsole`을 사용하자. 대신 `jconsole`은 에덴, 서바이버
 스페이스, 올드 제너레이션 또느 퍼머넌트 제너레이션 중 하나만 보여줄 수
 있다.<br>
`jstat`은 스크립트 형태의 도구로 선택할 수 있다. `jstat`은 각기 다른 종류의 힙
 정보를 출력하는 옵션 아홉 개를 제공한다. 유용한 옵션인 `-gcutil`은 현재 채워져
 있는 각 GC 영역의 비율뿐만 아니라 GC에서 쓰고 있는 시간을 보여준다. `jstat`은
 명령을 반복하기 위한 밀리세컨드 수 등과 같은 선택적 인자를 취해서 시간이 흐름에
 따라 애플리케이션 내의 GC가 주는 영향을 모니터링할 수 있다는 점을 기억하자.
 여러분이 GC 로그를 생성하는 걸 잊어버렸다면 앞에서 설명한 방법이 시간이 지남에
 따라 GC가 어떻게 동작하는지 볼 때 훌륭한 대안된다.

## 6. 가비지 컬렉션 알고리즘

처리율 컬렉터에 적응 크기 적용을 허용하면 중단 시간 목표를 맞추기 위해 힙(과
 제너레이션)의 크기를 조정할 것이다. 이 목표는 `-XX:MaxGCPauseMillis=N`와
 `-XX:GCTimeRatio=N` 플래그로 설정된다. `MaxGCPauseMillis` 값은 마이너와 풀 GC
 양쪽에 적용된다는 점을 명심하자. 기본적으로 이 플래그는 미설정 상태다. 처리율
 목표 = 1 - 1 / (1 + GCTimeRatio). `GCTimeRatio`의 디폴트 값은 99다. 이 값을
 방정식에 넣으면 0.99가 나오는데, 이는 애플리케이션 처리에 시간의 99%를 쓰고
 GC에 1%만 쓰는 게 목표라는 의미다. 특정 목표가 없다면 보통 시간 비율을
 19(GC에서 시간의 5% 사용)로 시작한다. `MaxGCPauseMillis` 플래그의 우선순위가
 높다. 이 플래그가 설정되면 영과 올드 제너레이션의 크기는 중단 시간 목표를 이룰
 때까지 조정된다. 일단 그런 상황이 되면 힙의 전체 크기는 시간 비율 목표를 이룰
 때까지 늘어난다. 이 두 목표를 만족하면 JVM은 힙의 크기를 줄이려고 시도하므로
 결국 이 두 목표를 이룰 가능성이 있는 크기 중에서 가장 작은 힙이 된다.<br>
CMS가 문제에 직면했다는 사실을 나타내는 메시지 세 개를 더 살펴봐야 한다. 첫
 번째는 동시 병렬 모드 실패("concurrent mode failure")다. 영 컬렉션이 발생하고
 올드 제너레이션 내에 승격되야 할 객체 전부를 유지하기 위한 공간이 충분치 않을
 때 CMS는 기본적으로 풀 GC란 걸 수행한다. 두 번째 문제는 승격된 객체를 유지할
 공간은 올드 제너레이션 내에 충분하지만 해제 공간(free space)이 단편화되면서
 승격이 실패할("promotion failed") 때 일어난다. 전체 힙이 압축됐기 때문에 CMS가
 동시 병렬 모드 실패에 처했을 때보다 훨씬 더 오래 걸렸다. 마지막으로 CMS 로그는
 일반적인 동시 병렬 GC 메시지 한 줄 없이 풀 GC에 관한 내용을 보여줄 것이다. 이건
 퍼머넌트 제너레이션이 가득 차서 수집될 필요가 있을 때 발생한다.<br>
올드 제너레이션이 특정 수준(디폴트는 70%)까지 차면 동시 병렬 주기가 시작되고
 백그라운드 CMS 스레드는 가비지를 찾으려고 올드 제너레이션을 검사하기 시작한다.
 이 시점에서 경쟁이 시작한다. CMS는 올드 제너레이션의 남은 영역(30%)이 차기
 전까지 올드 제너레이션을 검사해서 객체를 해제하길 마쳐야 한다. 동시 병렬 주기가
 경쟁에서 진다면 CMS는 동시 병렬 모드 실패를 경험하게 될 것이다. 메모리를 더
 많이 사용할 수 있다면 힙의 크기를 늘리는 편이 더 좋은 해결책이다. 그렇지 않다면
 백그라운드 스레드가 동작하는 방식을 바꾸자. `UseCMSinitiatingOccupancyOnly`
 플래그의 디폴트는 `false`고 CMS는 백그라운드 스레드를 시작할 시기를 결정하기
 위해 더 복잡한 알고리즘을 사용한다. 백그라운드 스레드를 더 빨리 시작할 필요가
 있다면 `UseCMSInitiatingOccupancyOnly` 플래그를 `true`로 설정하고
 `-XX:CMSInitiatingOccupancyFraction=N` 플래그를 설정한다. 애플리케이션이 동시
 병렬 모드 실패를 경험했고 여분의 CPU 주기를 사용할 수 있다면,
 `-XX:ConcGCThreads=N` 플래그를 설정해서 백그라운드 스레드의 개수를 늘릴 수
 있다. 기본값은 (3 + ParallelGCThreads) / 4 이다.
 `-XX:+CMSPermGenSweepingEnabled` 플래그를 사용하면 퍼머넌트 제너레이션은 올드
 제너레이션처럼 수집된다. CMS 퍼머넌트 제너레이션 컨렉션은 퍼머넌트 제너레이션의
 사용 비율이 `-XX:CMSInitiatingPermOccupancyFraction=N`(디폴트는 80%)에 명시된
 값에 이르렀을 때 일어난다. 실제로 미참조 클래스를 해제하기 위해서는
 `-XX:+CMSClassUnloadingEnabled` 플래그가 설정되어야 한다. 그렇지 않다면 CMS
 퍼머넌트 제너레이션 수거 작업으로 몇 종류의 객체는 간신히 해제되지만 클래스
 메타데이터는 해제되지 않을 것이다. 자바 8에서 CMS는 기본적으로 메타스페이스에서
 로드되지 않은 클래스를 제거한다. 어떤 이유로 이 기능을 사용하길 원치 않는다면
 `-XX:CMSClassUnloadingEnabled` 플래그를 설정하지 말자. 단일 CPU 머신만 갖고
 있는데 컬렉터의 중단 빈도가 낮아야 한다면 어떻게 해야 하는가? 또는 여러 개
 있지만 CPU들이 매우 바쁘다면 어떻게 하겠는가? 점진적인 CMS(incremental CMS,
 iCMS)는 `-XX:+CMSIncrementalMode` 플래그를 명시해서 사용할 수 있다. 백그라운드
 스레드가 애플리케이션 스레드로 대체되는 비율은
 `-XX:CMSIncrementalSafetyFactor=N`, `-XX:CMSIncrementalDutyCycleMin=N`과
 `-XX:-CMSIncrementalPacing` `CMSIncrementalDutyCycle` 플래그의 값을 변경해서
 제어한다. 자바 8에서는 점진적인 CMS가 사용되지 않는다(deprecated). 특히 CPU에
 대한 경쟁을 제한하며, 백그라운드 주기 동안 G1의 백그라운드 스레드가 정기적으로
 멈추므로, 제한된 CPU가 있는 시스템에서 G1 컬렉터를 고려하자.<br>
각 영역(디폴트로 약 2,048개가 있다)은 올드나 신규 제너레이션에 속할 수 있고
 제너레이션의 영역은 인접할 필요는 없다. 주로 가비지 영역만 치우는 이 접근법은
 가비지 우선 방식으로, G1이라는 명칭은 가비지 우선(Garbage First)에서 나왔다. 첫
 번째 단계는 초기-표시 단계("initial-mark" phase)다. 이 단계는 부분적으로 영
 컬렉션도 수행하므로 모든 애플리케이션 스레드를 멈춘다. 다음으로 G1은 루트
 영역을 살핀다("concurrent-root-region-scan-start/end"). 루트 영역을 살펴보는
 동안 (모든 애플리케이션 스레드를 멈출) 영 제너레이션이 가득 차게 되면 영
 컬렉션은 루트 조사가 끝날 때까지 대기해야 한다. 루트 영역 조사 후에 G1은 동시
 병렬 표시 단계("concurrent-mark-start/end")에 접어든다. 이건 완벽하게
 백그라운드에서 일어난다. 표시 단계의 뒤를 이어 재표시 단계("remark"ing phase)와
 정상 소거 단계(normal "cleanup" phase)가 온다. 대개 짧은 기간이긴 하지만 이
 단계는 애플리케이션 스레드를 멈춘다. 다음으로 동시에 일어나는 추가적인 소거
 단계("concurrent-cleanup-start/end")가 있다. 그리고는 적어도 가비지가 간 곳을
 찾는 데 있어서는 정규 G1 주기(normal G1 cycle)가 끝난다. 하지만 사실상 아직
 거의 해제되지 않는다. 이제 G1은 일련의 혼합("mixed") GC를 수행한다. 이들은 정규
 영 컬렉션을 수행하지만 백그라운드 조사에서 표시된 영역 몇 군데에서 수집도 하기
 때문에 혼합되었다고 일컬어진다. 표시 영역 전부가 (거의) 수집될 때까지 혼합 GC
 주기가 계속 되고, 그때 G1은 정규 영 GC 주기를 재개할 것이다. 풀 GC가 촉발되는
 주요 시기가 네 가지 있다.
* 동시 병렬 모드 실패 - G1은 표시 주기를 시작하지만 올드 제너레이션은 주기가
  완료되기 전에 꽉 찬다. 이 경우 G1은 표시 주기를
  중단한다("concurrent-mark-abort").
* 승격 실패("to-space exhausted") - G1은 표시 주기를 완료하고 올드 영역을 치우기
  위해 혼합 GC를 수행하기 시작했지만, 올드 제너레이션이 충분한 메모리를 환원하기
  전에 올드 제너레이션의 공간이 부족해진다.
* 비우기 실패("to-space overflow") - 영 컬렉션이 수행되고 있을 때 서바이버
  스페이스와 올드 제너레이션 내에 살아남은 객체를 전부 유지하기 위한 공간이
  충분하지 않다.
* 대규모 할당 실패 - 풀 GC가 명백한 이유 없이 발생했더라도 대규모 할당에 관한
  이슈일 가능성이 높다.

여기에 적용될 수 있는 튜닝의 종류는 많지만 G1의 목표 중 하나는 튜닝을 너무 많이
 할 필요가 없어야 한다는 점이다. 그 목적을 이루기 위해 주로 처리율 컬렉터를
 튜닝하는 데 사용한 플래그인 `-XX:MaxGCPauseMillis=N`을 통해서만 G1을 튜닝한다.
 G1을 사용할 때 (그리고 처리율 컬렉터와 달리) 이 플래그의 디폴트 값은 200ms이다.
 G1이 경쟁에서 이기도록 하려면 (머신상에 가용 CPU가 충분히 있다는 가정하에서)
 백그라운드 표시 스레드의 수를 늘리도록 하자. `ConcGCThreads`의 디폴트 값은
 (ParallelGCThreads + 2) / 4 다. G1이 더 빨리 수집하기 시작하면 이 문제도 해결할
 수 있다. G1 주기는 디폴트 값이 45인 `XX:InitiatingHeapOccupancyPercent=N`으로
 명시된 점유율의 비율과 힙이 같아질 때 시작된다. CMS와 달리 그 설정은 올드
 제너레이션뿐만 아니라 전체 힙의 사용률을 기반으로 한다는 점에 주목하자. G1은
 중단 시간 목표를 맞추고자 시도할 때 절대로 그 숫자를 바꾸지 않는다. 동시 병렬
 주기 후에 G1은 올드 제너레이션 내에 미리 표시된 영역이 전부 수집되기 전까지
 새로운 동시 병렬 주기를 시작할 수 없다. 따라서 G1을 표시 주기보다 일찍 시작하기
 위한 다른 방법은 혼합 GC 쥐에서 더 많은 영역을 처리하는 것이다(그래서 결국 혼합
 GC 주기는 더 적어질 것이다.) 혼합 GC가 하는 일의 양은 세 가지 요소에 의존한다.
 첫 번째는 우선 대부분 가비지가 존재한다고 발견된 영역의 수이다. 거기에
 직접적으로 영향을 줄 수 있는 방법은 없다. 35%가 가비지라면 혼합 GC 동안 해당
 영역은 컬렉션 대상으로 선언된다. (이 값은 어떤 시점에 튜닝이 가능한 매개 변수가
 될 확률이 크다. (오픈소스 코드의 실험용 빌드에서 사용 가능한) 매개 변수의
 실험적인 명칭은 `-XX:G1MixedGCLiveThresholdPercent=N`이다.) 두 번째 요소는
 `XX:G1MixedGCCountTarget=N` 플래그의 값으로 정의되며 G1이 영역들을 처리할 때 쓸
 혼합 GC 주기의 최대 개수다. 이 플래그의 디폴트 값은 8이다. 값을 줄이면 (혼합 GC
 주기 동안 중단 시간이 더 길어지는 대신) 승격 실패를 극복하는 데 도움이 될 수
 있다. 마지막으로 세 번째 요소는 GC 중단에 대한 최대 희망 길이다(예,
 `MaxGCPauseMillis`으로 명시된 값). 중단 대상에 대해 시간이 허용한다면 G1은
 표시된 올드 제너레이션 영역의 1/8(또는 미리 정해진 값)보다 더 많이 수집할
 것이다. 

서바이버 스페이스는 영 제너레이션을 위한 일부 할당을 맡고, 힙의 다른 영역과 같이
 JVM은 이들의 크기를 동적으로 설정한다. 서바이버 스페이스의 초기 크기는
 `-XX:InitialSurvivorRatio=N` 플래그로 정해지며 다음 방정식에서 사용된다.
 survivor_space_size = new_size / (survivor_ratio + 2). 초기 서바이버 비율이
 8이라면 각 서바이버 스페이스는 영 제너레이션의 10%를 차지할 것이다. JVM은
 `-XX:MinSurvivorRatio=N` 플래그에 설정된 최대 값에 맞춰서 서바이버 스페이스
 크기를 늘일 것이다. 기본적으로 이 값은 3이며 서바이버 스페이스의 최대 크기는 영
 제너레이션의 20%가 된다는 의미다. 서바이버 스페이스를 고정 크기로 유지하려면
 `SurvivorRatio`를 원하는 값으로 설정하고 (적응 크기 적용을 비활성화시키면
 올드와 신규 제너레이션에도 적용된다는 점을 기억해야 하지만)
 `UseAdaptiveSizePolicy` 플래그를 비활성화시키자. JVM은 GC 후에 서바이버
 스페이스가 얼마나 찼는지에 따라 (정의된 비율에 맞춰서) 서바이버 스페이스의
 크기를 늘리거나 줄일지 결정한다. 서바이버 스페이스의 크기가 조정되면 기본적으로
 GC에 50% 차게 된다. 이 값은 `-XX:TargetSurvivorRatio=N` 플래그로 변경할 수
 있다. 마지막으로 올드 제너레이션으로 이동되기 전에 객체가 서바이버 스페이스
 사이에서 핑퐁을 치듯이 움직이며 남아있을 GC 주기는 몇 번인가에 대한 의문이
 있다. 그 답은 재임 한계치(tenuring threshold)에 의해 결정된다. JVM은 계속해서
 최적의 재임 한계치가 뭔지 계산한다. 한계치는 `-XX:InitialTenuringThreshold=N`
 플래그로 정의된 값으로 시작한다(디폴트는 처리율과 G1 컬렉터에서는 7이고
 CMS에서는 6이다). JVM은 결국 1과 `-XX:MaxTenuringThreshold=N` 플래그로 정의된
 값 사이에서 한계치를 결정할 것이다. 처리율과 G1 컬렉터를 위한 디폴트 최대
 한계치는 15이고 CMS에서는 6이다. `-XX:+PrintTenuringDistribution` 플래그를 쓰면
 GC 로그에 재임 통계 정보가 추가되므로 도움이 될 것이다. 가장 중요하게 살펴봐야
 할 것은 마이너 GC가 일어나는 동안 객체가 에덴에서 올드 제너레이션으로 바로
 승격될 정도로 서바이버 스페이스가 매우 작은지의 여부다. 이 로그에는 결정적인
 해답은 없지만 JVM이 재임 한계치를 1로 조정했다는 사실은 어쨌든 대부분의 객체가
 올드 제너레이션으로 바로 승격되도록 재임 한계치를 최소화시키겠다고 결정한
 것으로 보인다. 영 컬렉션에서 살아남은 객체들은 항상 오랫동안 존재한다는 사실을
 안다면 `-XX:+AlwaysTenure`를 명시할 수 있으며 `MaxTenuringThreshold`를 0으로
 설정하는 것과 기본적으로 같다. 이건 아주 드문 상황으로, 항상 객체가 서바이버
 스페이스에 저장되지 않고 올드 제너레이션으로 바로 승격된다는 의미다. 반대로
 `-XX:+NeverTenure` 플래그는 초기와 최대 재임 한계치가 무한대인 것처럼 행동하고
 JVM이 한계치를 내려서 조장하지 못하게 한다. 다시 말해서 서바이버 스페이스에
 공간이 있는 한 어떤 객체도 올드 제너레이션으로 승격되지 못할 것이다.<br>
TLAB (Thread Local Allocation Buffer) 크기 설정은 모든 GC 알고리즘에서 고려해야
 하며 G1은 매우 큰 객체(다시 말하지만 이건 상대적인 용어로 2GB 힙에 있어
 512MB보다 더 큰 객체다)에 대해 추가 고려 사항이 있다. (어떤 컬렉터를 사용할 때
 다소 큰 객체를 극복하기 위한) TLAB 크기 설정은 꽤 특이한 일이지만(G1을 사용할
 때 매우 큰 객체를 극복하기 위한) G1 영역 크기 설정은 훨씬 보편적이다. 에덴 내의
 할당이 매우 빠른 이유는 각 스레드가 객체를 할당하는 전용 영역인 TLAB를 담당하기
 때문이라는 사실이 밝혀졌다. 각 스레드에 전담 할당 영역을 설정하면 스레드는
 객체를 할당할 때 동기화를 수행할 필요가 없다. TLAB가 가득 차면 일정 크기의
 객체는 더 이상 그 안에 할당될 수 없다. 이 시점에서 JVM의 선택을 제어할 매개
 변수가 있지만 TLAB의 크기가 결정적이라는 점이 핵심이다. 기본적으로 TLAB의
 크기는 애플리케이션 내의 스레드 수, 에덴의 크기, 스레드의 할당 비율, 이렇게 세
 가지 요소를 기반으로 한다. TLAB는 크게 성능을 상승시키므로 바활성화시키려고
 시도하는 건 항상 나쁜 결정이긴 하지만 `-XX:-UseTLAB`에 명시해서 비활성화시킬 수
 있다. 자바 플라이트 레코더는 다른 도구보다 TLAB 할당을 모니터링하는 데 훨씬 더
 강력하다. TLAB 밖에 할당되는 실제 객체들을 보여줄 수 있는 다른 탭들이 있다는
 사실에 주목하자. JVM의 오픈 소스 버전에서 할 수 있는 최선은 커맨드 라인에
 `-XX:+PrintTLAB` 플래그를 추가해서 TLAB 할당을 모니터링하는 것이다. 그다음 영
 컬렉션이 일어날 때마다 GC 로그에는 각 스레드의 TLAB 사용률을 설명하는 문장과
 JVM의 전체 TLAB 사용률을 설명하는 요약문의 문장이 포함된다.

```
TLAB: gc thread: 0x00007f3c10b8f800 [id: 18519] desired_size: 221KB
    slow allocs: 8  refill waste: 3536B alloc: 0.01613    11058KB
    refills: 73 waste  0.1% gc: 10368B slow: 2112B fast: 0B

TLAB totals: thrds: 66  refills: 3234 max: 105
        slow allocs: 406 max 14 waste:  1.1% gc: 7519856B
        max: 211464B slow: 120016B max: 4808B fast: 0B max: 0B
```

이 스레드의 TLAB의 크기는 221KB다. 최근의 영 컬렉션으로 힙에서 여덟 개의 객체를
 할당했는데 (`slow allocs`) 이 스레드가 할당한 총량 중 1.6%(0.01613)이며
 11,058KB에 이른다. TLAB의 0.1%는 "낭비"됐고 이건 세 가지 사항 때문에 비롯됐다.
 10,336byte는 현재 GC 주기가 시작됐을 때 TLAB에서 해제됐다. 2,112byte는 다른
 (버려진) TLAB에서 해제됐고, 0byte는 "빠른" 특별 할당자(allocator)를 통해
 할당됐다. 이 경우에 66개의 스레드는 마지막 영 컬렉션이 일어난 후 일정의 할당
 작업을 수행했다. 이 스레드들은 개별 TLAB를 3,234번 다시 채웠으며 특정 스레드는
 105번 다시 채웠다. 전부 힙으로 406번의 할당(한 스레드가 최대 14번 했다)이
 있었으며 TLAB의 1.1%가 버려진 TLAB 내의 자유 공간에서 낭비됐다.<br>
TLAB의 크기는 `-XX:TLABSize=N` 플래그(디폴트 값은 0으로 앞으로 설명했던 동적
 연산을 사용한다는 의미다)를 사용해서 명시적으로 설정할 수 있다. 이 플래그는
 TLAB의 초기 크기만 설정한다. GC가 일어날 때마다 크기가 변경되는 걸 방지하려면
 `-XX:-ResizeTLAB`를 추가하자. 이건 TLAB를 조정해서 성능을 확인하는 데 있어 가장
 쉬운(그리고 솔직히 유일하게 실제로 유용한) 옵션이다. TLAB 로그 출력물에서
 `refill waste` 값은 결정할 수 있도록 현재의 한계치를 제공한다. TLAB가 그 값보다
 더 큰 신규 객체를 수용할 수 없다면 새 객체는 힙에 할당될 것이다. 문제의 객체가
 그 값보다 적다면 TLAB는 버려질 것이다. 이 값은 동적이지만 TLAB 크기의 1% 또는
 명확하게 `-XX:TLABWasteTargetPercent=N`으로 명시된 값을 디폴트로 시작한다. 각
 할당은 힙의 밖에서 이뤄지며 그 값은 `-XX:TLABWasteIncrement=N`(디폴트는 4)의
 값만큼 증가한다. 마지막으로 TLAB 크기 조절이 유효할 때 TLAB의 최소 크기는
 `-XX:MinTLABSize=N`(디폴트는 2KB)으로 명시할 수 있다. TLAB의 최대 크기는
 1GB(객체 정렬 목적에 따라 반올림된 정수 배열로 채워질 수 있는 최대 공간)보다
 약간 작고 변경할 수 없다.<br>
객체의 크기가 에덴 내에 맞지 않다면 올드 제너레이션으로 직접 할당돼야 한다. 이
 경우에는 단명하는 거대 객체를 쓸 필요가 없도로 애플리케이션을 변경하는 것
 말고는 할 수 있는 일이 별로 없다. G1 영역 크기는 동적이지 않으며 스타트업을 할
 때 히브이 최소 크기(`Xms`의 값)를 기반으로 결정된다. (초기 크기 대비 매우
 달라진 최대 크기를 갖는 힙은 G1 영역이너무 많아질 것이다. 이 경우 G1 영역
 크기는 늘어나야 한다.) 영역 크기는 힙 크기에 따라 결정되며 항상 최소 1MB이며
 32MB 이상은 절대로 넘지 않는다. G1 영역의 크기는 `-XX:G1HeapRegionSize=N`
 플래그(디폴트는 명목상 0이며 동적인 값을 사용할 수 있다는 의미다)로 설정할 수
 있다. 여기 주어진 값은 2승 가까이 반올림된다. 거대한 객체를 연속적인 G1 영역에
 할당해야 한다. G1은 영역의 50% 이상 차지할 정도면 객체가 거대하다고 여긴다.<br>
`AggressiveHeap` 플래그는 다양한 커맨드 라인 인자를 더 쉽게 설정할 수 있게
 만들기 위한 시도로 자바의 초기 버전에 도입됐다. 이 인자들은 단일 JVM을
 실행시키며 메모리가 많이 장착된 대형 머신에 적합할 것이다. 이건 64bit JVM에만
 적용된다, 플래그가 이 버전 이후로도 유지되어 여전히 남아있긴 하지만 더 이상
 권장되지는 않는다.<br>
디폴트 크기는 머신 내의 메모리 양을 기반으로 하며 `-XX:MaxRAM=N` 플래그로 설정할
 수 있다. 하지만 JVM은 클라이언트 컴파일러를 위해 `MaxRAM`을 1GB로, 32bit 서버
 컴파일러를 위해서는 4GB로, 64bit 컴파일러를 위해서는 128GB로 제한한다. 디폴트
 최대 힙 계산신은 다음과 같다. Default Xmx = MaxRAM / MaxRAMFraction. 그러므로
 디폴트 최대 힙은 디폴트가 4인 `-XX:MaxRAMFraction=N` 플래그의 값을 조정해서도
 설정할 수 있다. 마지막으로 상황을 흥미롭게 하기 위해 `-XX:ErgoHeapSizeLimit=N`
 플래그는 JVM이 사용해야 하는 최대 디폴트 값으로 설정될 수 있다. 그 값의
 디폴트는 0(무시한다는 의미)이다. 그렇지 않으면 MaxRAM / MaxRAMFraction보다 작은
 경우 제한 값이 사용된다. 반면 매우 작은 물리 메모리가 장착된 머신에서 JVM은
 운영체제를 위해 충분한 메모리를 반드시 남겨두고자 한다. 이게 JVM이 192MB인
 메모리만 달려있는 머신에서 최대 힙을 96MB 이하로 제한하는 이유다. 이 연산은
 디폴트가 2인 `-XX:MinRAMFraction=N` 플래그의 값을 기반으로 한다. (메모리가 96
 MB * MinRAMFraction 보다 작다면, 최대 힙 크기는 메모리 / MinRAMFraction) 초기
 힙 크기 값은 다음과 같이 결정된다. Default Xms = MaxRAM / InitialRAMFraction.
 디폴트 최소 힙 크기에서 결론 지을 수 있는 바와 같이 `InitialRAMFraction`
 플래그의 디폴트 값은 64다. 그 값이 5MB보다 작으면, 엄밀히 말해 (디폴트가 4MB인)
 `XX:OldSize=N` 더하기 (디폴트가 1MB인) `-XX:NewSize=N`로 명시된 값보다 작으면
 경고가 뜬다. 이 경우에 기존과 신규 크기의 합은 초기 힙 크기로 사용된다.

## 7. 힙 메모리 베스트 프랙티스

힙 히스토그램은 전체 힙 덤프 없이 애플리케이션 내의 많은 객체를 빨리 살펴볼 수
 있는 방법이다. 힙 히스토그램은 `jcmd process_id GC.class_histogram`을 사용해서
 얻을 수 있다. 명령어로 풀 GC가 강제로 일어나도록 하지 않더라도
 `GC.class_histogram`에서의 출력물에는 살아있는 객체만 포함된다.
 `jmap -histo process_id`로 얻은 출력물은 수집 대상이 되는 객체(죽은 객체)를
 포함한다. 히스토그램을 보기에 앞서 풀 GC를 강제로 일으키려면 대신 다음 명령어를
 실행시키자: `jmap -histo:live process_id`.<br>
심도 있는 분석을 위해서는 힙 덤프가 필요하다.
 `jcmd process_id GC.heap_dump /path/to/heap_dump.hprof` 또는
 `jmap -dump:live,file=/path/to/heap_dump.hpro process_id`. `jmap`에 `live`
 옵션을 포함하면 힙을 덤프로 뜨기 전에 풀 GC를 강제로 일으킬 것이다. 이건
 `jcmd`에서는 디폴트다. 다른 (죽은) 객체를 포함시키길 원한다면 `jcmd` 커맨드
 라인의 끝에 `-all`을 명시하면 된다. 가장 흔한 도구 세 가지는 `jhat`과
 `jvisualvm` 그리고 `mat` (EclipseLink Memory Analyzer Tool)다. 커다란 힙 공간을
 보유한 객체들은 때로 힙의 지배자(dominator)라고 불린다. GC 루트로 바로 접근하는
 것이 반드시 유용한 건 아니지만 힙 분석 도구는 특정 객체(이 경우에는 객체
 세트)의 GC 루트(root)를 찾는 방법을 제공한다. 힙 덤프를 두 개 생성했다면
 `mat`로 힙 히스토그램 두 개에서 차이점을 계산할 수 있다.
 `-XX:+HeapDumpOnOutOfMemoryError`, `-XX:HeapDumpPath=<path>`,
 `-XX:+HeapDumpAfterFullGC`, `-XX:+HeapDumpBeforeFullGC` 플래그는 자동으로 힙
 덤프를 한다. 여러 개의 힙 덤프가 생성되는 경우(예, 풀 GC가 여러 번 일어나서)에
 힙 덤프 파일명에 연속적인 숫자를 붙인다.<br>
기존 클래스로더가 영역 밖으로 나가지 않으면 클래스 메타데이터는 해제될 수 없으며
 결국 퍼머넌트 제너레이션이 가득 차고 메모리 부족 에러가 일어난다. 이 경우
 퍼머넌트 제어레이션의 크기를 늘리면 도움이 되겠지만 결국 단지 에러 발생 시기를
 연기시킬 뿐이다. 클래스로더를 많이 생성하고 폐기하는 애플리케이션 코드를
 작성하고 있다면 반드시 클래스로더 자체가 정확하게 폐기되도록 주의해서
 작업하자(특히 스레드의 컨텍스트 클래스로더를 임시 클래스로더 중의 하나로
 설정하지 않도록 하자). / 이 상황을 극복하는 데 있어 가장 좋은 방법은 컬렉션에서
 더 이상 필요 없는 아이템들이 사전에 폐기되도록 애플리케이션 로직을 변경하는
 것이다. 대신 약한(weak) 참조나 소프트(soft) 참조를 사용하는 컬렉션은
 애플리케이션 내에서 이들을 전혀 참조하지 않을 때 아이템을 자동으로 폐기할 수
 있지만 이 컬렉션은 비용이 꽤 든다. / 마지막으로 JVM이 메모리 부족 에러를
 일으키는 경우는 GC를 수행하는 데 시간이 너무 오래 걸린다고 결정할 때다.
 `GC ovrhead limit exceeded` 에러는 다음 조건이 모두 충족될 때 발생한다.
1. 풀 GC에 소요된 시간이 `-XX:GCTimeLimit=N` 프래그에서 정의한 값을 초과하다.
   디폴트 값은 98(예, 시간의 98%를 GC에서 쓴다)이다.
1. 풀 GC를 한 결과 되찾은 메모리의 양이 `-XX:GCHeapFreeLimit=N` 플래그에 명시된
   값보다 작다. 이 플래그의 디폴트 값은 2로 풀 GC 동안 힙의 2%보다 적게 해제되는
   경우 조건이 충족된다는 의미다.
1. 위의 두 조건은 다섯 번의 연속적인 (값을 조정할 수 없는) 풀 GC 주기에서
   참이어야 한다.
1. `-XX:+UseGCOverheadLimit` 플래그의 값은 (디폴트로) `true`다.

참조(reference) - 32GB 이상의 힙을 가진 64bit JVM과 32bit JVM에서는 4, 큰 힙이
 있는 64bit JVM에서는 8. 일반 객체에서 헤더 필드의 크기는 (힙 크기에 관계없이)
 32bit JVM에서는 8byte며 64bit JVM에서는 16byte다. 배열에서 헤더 필드의 크기는
 32GB이하의 힙이 있는 64bit JVM이나 32bit JVM에서는 16byte고 그 외에는 24byte다.
 객체 크기는 항상 채워지기(padding) 때문에 8byte의 배수다.<br>
연관된 코드가 스레드 세이프해야 하면 느린 초기화 처리는 더 복잡해진다. 첫 번째
 단계로 단순히 전통적인 동기화를 추가하는 게 가장 쉽다. 병목은 이중 검사 방식을
 사용하면 해결된다. 인스턴스 변수는 volatile로 선언되어야 하며 인스턴스 변수를
 로컬 변수로 할당하면 성능상의 이점이 조금 생긴다.

```java
public class CHMInitialization {
    private volatile ConcurrentHashMap instanceChm;

    public void doOperation() {
        ConcurrentHashMap chm = instanceChm;
        if (chm == null) {
            synchronized(this) {
                chm = instanceChm;
                if (chm == null) {
                    chm = new ConcurrentHashMap();
                    // ... code to populate the map
                    instanceChm = chm;
                }
            }
            // ...use the chm...
        }
    }
}

public class ImmutableObject {
    WeakHeapMap<ImmutableObject, ImmutableObject> map = new WeakHeapMap();

    public ImmutableObject canonicalVersion(ImmutableObject io) {
        synchronized(map) {
            ImmutableObject canonicalVersion = map.get(io);
            if (canonicalVersion == null) {
                map.put(io, io);
                canonicalVersion = io;
            }
            return canonicalVersion;
        }
    }
}
```

자바에서는 많은 객체 타입이 불변이다. 여기에는 대응하는 기본형--`Integer`,
 `Double`, `Boolean` 등뿐만 아니라 `BigDecimal`과 같이 숫자 기반 타입인 객체들이
 포함된다. 문자열 연결을 너무 많이 하는 경우 주의할 점은 연결된 문자의 테이블이
 네이티브 메모리 내에 구성된 고정 크기 해시 테이블이라는 것이다. 자바 7u40 이전
 릴리즈에서 테이블의 디폴트 크기는 1,009버킷이다. 자바 7u40의 64but와 그 이후의
 버전에서 디폴트 크기는 60,013이다. 자바 7을 시작으로 이 테이블의 크기는
 `-XX:StringTableSize=N` 플래그를 사용해서 JVM을 시작할 때 설정할 수 있다.
 문자열 테이블이 실행되는 방법을 알려면 `-XX:+PrintStringTableStatistics`
 인자(JDK 7u6이나 이후의 버전이 필요)를 넣고 애플리케이션을 실행시키자. JVM이
 종료할 때 다음과 같은 표를 출력할 것이다. 애플리케이션이 할당한 연결 문자열의
 개수(와 이들의 총 크기)는 `jmap -heap process_id` 명령어를 사용해서 얻을 수도
 있다(이건 JDK 7u6이나 이후 버전이 필요하다). 문자열 테이블을 너무 크게 설정하는
 데 따른 불이익은 사소하다.<br>
`String.equals()` 메서드는 꽤 빠르다.

주로 객체 재사용을 하는 방법에는 객체 풀과 스레드-로컬 변수 두 가지 있다. 이
 기술 중 어느 것이든 GC의 효율성을 해치기 때문에 지금 전 세계의 GC 엔지니어는
 신음하고 있다.<br>
다음은 JDK와 자바 EE 재사용 객체가 있는 (그리고 왜 쓰는지에 대한) 예제다. 스레드
 풀, JDBC 풀, EJB 풀, 큰 배열 (자바에서는 배열이 할당될 때 배열 내의 개별적인
 엘리먼트 전부가 특정 디폴트 값(널, 0이나 `false` 중 적절한 값)으로 초기화되어야
 한다.), 네이티브 NIO 버퍼 (직접 `java.nio.Buffer`(즉 `allocateDirect()`
 메서드를 호출해서 반환되는 버퍼)를 할당하는 동작은 버퍼의 크기에 상관없이
 비용이 비싸다. 큰 버퍼 한 개를 생성해서 필요한 만큼 영역을 나눠서 관리하고
 향후의 동작에서 재사용하도록 반환하는 편이 더 낫다.), 보안 클래스
 (`MessageDigest`, `Signature`와 다른 보안 알고리즘의 인스턴스는 초기화하는
 비용이 비싸다.), 문자열 인코더와 디코더 객체, `StringBuilder` 헬퍼, 난수 생성기
 (`Random`과 특히 `SecureRandom` 클래스의 인스턴스는 시드 값을 입력해서 수열을
 생성하는 비용이 많이 든다.), DNS 룩업에서 얻은 이름, ZIP 인코더와 디코더
 (흥미로운 반전은 이들을 초기화하는 비용이 비싸지 않다는 점이다. 하지만 반드시
 이들이 사용하는 네이티브 메모리도 해제하기 위해 객체 종결(finalization) 처리를
 필요로 하므로 해제하는 데 비용이 많이 든다.)<br>
스레드-로컬 객체의 성능상의 이점은 때로 (객체 재사용으로 아끼기보다) 동기화
 비용을 아끼는 측면에서 드러나므로 동기화는 흥미로운 점을 환시기킨다. 예를 들어
 자바 7은 `ThreadLocalRandom` 클래스를 도입했다.<br>
막연한 참조를 많이 사용하는 애플리케이션을 실행시킬 때 `-XX:+PrintReferenceGC`
 플래그 추가를 고려하자. 이 플래그는 참조를 처리하는 데 시간이 얼마나 걸리는지
 볼 수 있게 해준다.<br>
소프트 참조는 기본적으로 가장 최근에 사용된(LRU) 객체 정보를 담고 있는 하나의 큰
 풀이다. 방정식은 다음 슈도 코드와 같다. long ms = SoftRefLRUPolicyMSPerMS *
 AmountOfFreeMemoryInMB; if (now - last_access_to_reference > ms) free the
 reference. 첫 번째느 `-XX:SoftRefLRUPolicyMSPerMB=N` 플래그에 의해 설정되며
 디폴트 값은 1,000이다. 두 번째 값은 (GC 주기가 완료된 후) 힙 내 자유 메모리의
 양이다. 서버 컴파일러는 힙의 최대 가용 크기를 사용하는 반면 클라이언트
 컴파일러는 현재 힙 내에서 이용 가능한 메모리에 대한 값을 기반으로 한다. 소프트
 참조는 객체의 수가 너무 크지 않을 때 잘 동작한다. 그 외에는 LRU 캐시로 구현되며
 한정된 크기를 가진 더욱 전통적인 형태의 객체 풀을 고려하도록 하자.<br>
약한 참조는 여러 개의 스레드가 문제의 참조 대상을 동시에 사용할 때 이용해야
 한다. 그렇지 않으면 약한 참조는 가비지 컬렉터에 의해 반환될 가능성이 너무 크다.
 약하게 참조만 된 객체는 GC 주기마다 반환된다. 강한 참조가 제거될 때 약한 참조는
 바로 해제된다.<br>
파이널라이저(Finalizer)는 기능적인 이유에서 나쁘며 성능에서도 나쁘다.
 파이널라이저는 사실 막연한 참조의 특별한 경우다. JVM은 `finalize()` 메서드를
 정의한 객체를 추적하기 위해 프라이빗 참조 클래스(`java.lang.ref.Finalizer`,
 `java.lang.ref.Fina` 참조)를 이용한다. 적어도 이 문제의 일부를 회피하는
 파이널라이저를 사용한다는 대안이 있으며, 특히 일반 GC 동작 동안 참조 대상이
 해제되게 할 수 있다. 이건 함축적으로 `Finalizer` 참조를 사용하는 게 아니라
 단순히 다른 종류의 무제한 참조를 사용해서 이뤄진다. 여러분은
 `jcmd process_id GC.run_finalization` 명령어를 사용하여 파이널라이저 큐가
 처리되게 할 수 있다. 애플리케이션에 대한 이슈인지 확신하고자 파이널라이저 큐를
 모니터링하려면 `jconsole`의 VM 서머리 탭(Summary tab)에서 (실시간으로 갱신되는)
 그 크기를 살펴보자. `jmap -finalizerinfo process_id` 명령어를 실행시키면
 스크립트들이 그 정보를 모을 수 있다.

```java
private static class CleanupFinalizer extends WeakReference {
    private static ReferenceQueue<CleanupFinalizer> finRefQueue;
    private static HashSet<CleanupFinalizer> pendingRefs = new HashSet<>();

    private boolean closed = false;

    public CleanupFinalizer(Object o) {
        super(o, finRefQueue);
        allocateNative();
        pendingRefs.add(this);
    }

    public void setClosed() {
        closed = true;
        doNativeCleanup();
    }

    public void cleanup() {
        if (!closed) {
            doNativeCleanup();
        }
    }

    private native void allocateNative();
    private native void doNativeCleanup();

    static {
        finRefQueue = new ReferenceQueue<>();
        Runnable r = new Runnable() {
            public void run() {
                CleanupFinalizer fr;
                while (true) {
                    try {
                        fr = (CleanupFinalizer) finRefQueue.remove();
                        fr.cleanup();
                        pendingRefs.remove(fr);
                    } catch (Exception ex) {
                        Logger.getLogger(CleanupFinalizer.class.getName()).
                               log(Level.SEVERE, null, ex);
                    }
                }
            }
        };
        Thread t = new Thread(r);
        t.setDaemon(true);
        t.start();
    }
}

public class CleanupExample {
    private CleanupFinalizer cf;
    private HashMap data = new HashMap();

    public CleanupExample() {
        cf = new CleanupFinalizer(this);
    }

    // ...methods to put things into the hashmap...

    public void close() {
        data = null;
        cf.setClosed();
    }
}
```

## 8. 네이티브 메모리 베스트 프랙티스

최근의 리눅스 커널에서 PSS는 RSS(resident set size)를 개선한 것으로, 다른
 프로그램에 의해 공유된 데이터를 제외한다. 윈도우 시스템에서 동일한 개념은
 애플리케이션의 작업 세트(working set)라고 불리며 이 정보는 작업 관리자(task
 manager)가 보고한다.<br>
다이렉트 바이트 버퍼에 할당할 수 있는 메모리의 총 크기는
 `-XX:MaxDirectMemorySize=N` 플래그를 설정해서 명시한다. 자바 7을 시작으로 이
 플래그의 디폴트 값은 0이며 이건 무제한이라는 의미다 (이전에는 64MB).<br>
자바 8부터 JVM은 `-XX:NativeMemoryTracking=off|sumamry|detail` 옵션을 사용해서
 네이티브 메모리를 할당하는 방법에 대한 가시성을 일부 제공한다. 기본적으로
 네이티브 메모리 추적기(Native Memory Tracking, NMT)는 꺼져있다. 요약이나 상세
 모드가 활성화되어 있으면 `jcmd process_id VM.native_memory summary`를 써서
 언제든지 네이티브 메모리 정보를 얻을 수 있다. JVM이 `-XX:+PrintNMTStatistics`
 인자를 넣고 시작했다면 JVM은 프로그램이 끝날 때 할당에 대한 정보를 출력할
 것이다. NMT도 시간이 지남에 따라 메모리 할당이 일어나는 방식을 추적할 수 있게
 해준다. NMT가 활성화되게 설정하여 JVM을 시작한 후
 `jcmd process_id VM.native_memory baseline` 명령어를 사용해서 메모리 사용량에
 대한 기준점을 설정할 수 있다. 이건 JVM이 현재의 메모리 할당 정보를 표시하게
 한다. 이후 이 내용과 현재의 메모리 사용량을 비교할 수 있다.
 `jcmd process_id VM.native_memory summary.diff`<br>
`echo always > /sys/kernel/mm/transparent_hugepage/enabled`. 이 명료한 거대
 페이지를 사용할 수 있으면 `-XX:+UseLargePages` 플래그를 명시하지 말자. /
 마이크로소프트 관리 센터(Microsoft Management Center, `mmc`)를 시작하자. 스냅인
 추가/제거 → 그룹 정책 개체 편집기 → 로컬 컴퓨터 정책 → 컴퓨터 구성 → Windows
 설정 → 보안 설정 → 로컬 정책 → 사용자 권한 할당 → 메모리에 페이지 잠금 →
 사용자나 그룹을 추가하고 재부팅<br>
"oop"란 일반 객체 포인터(ordinary object pointer)를 나타낸다. oop는 JVM이 객체
 참조로 사용하는 핸들이다. 35bit oop가 있다면 어떨까? 그렇지만 대신 JVM은 참조의
 마지막 3bit는 모두 0이라고 가정할 수 있다. 이로 인해 JVM은 힙 내에서 32bit만
 쓰면서 32GB의 메모리를 참조할 수 있는 포인터를 갖게 된다. 첫 번째 4GB와 32GB
 사이에 있는 힙에서는 압축 oop를 쓰자. 압축 oop는 `-XX:+UseCompressedOops`
 플래그를 써서 활성화시킨다. 자바 7과 이후 버전에서 최대 힙 크기가 32GB보다
 작으면 디폴트로 사용할 수 있다. 두번째로 31GB 힙과 압축 oop를 사용하는
 프로그램은 보통 33GB 힙을 쓰는 프로그램보다 더 빠를 것이다. 객체 참조가 평균
 힙의 20%를 사용한다고 감안해서 처음에는 적어도 38GB로 계획을 세우는 편이 좋다.

## 9. 스레딩과 동기화 성능

(자바 API에 `ThreadPoolExecutor`가 추가된 시기보다 앞서 출시됐으므로) 사실
 대부분은 자체 스레드 풀을 만들어서 쓰고 있지만, 일부 자바 EE 애플리케이션
 서버는 작업을 관리하기 위해 `ThreadPoolExecutor` 클래스의 인스턴스를 사용하기도
 한다. 대부분의 애플리케이션 서버가 최소와 유사한 용어(예, `MinThreads`)를
 사용하는 반면 `ThreadPoolExecutor`와 연관 클래스는 스레드의 최대 수를 핵심 풀
 크기(core pool size)라고 표시한다. 하지만 `ThreadPoolExecutor`가 풀의 크기를
 재조정하는 시기를 결정하는 방법고 대부분의 자바 EE 애플리케이션 서버가 풀의
 크기를 재조정하는 시기를 결정하는 방법에는 주요 차이점이 있다.<br>
스레드 풀은 대개 보류 중인 일의 양과 머신에서 사용할 수 있는 CPU의 개수에 대한
 가시성을 갖고 있지만, 이들이 실행되고 있는 전체 환경 측면에서의 가시성은 없다.
 그러므로 직접 스레드 풀을 튜닝할 때(뿐만 아니라 `ThreadPoolExecutor`의 특정
 설정)의 주요 기능으로 작업이 보류되고 있을 때 스레드를 추가하는 것은 명확하게
 잘못된 일이다. 불행하게도 이건 스레드 풀의 최대 크기를 설정하는 것이 흔히
 과학적이기보다 예술적인 일에 더 가까운 이유기도 하다.<br>
거의 모든 경우에 최대 값과 동일한 값으로 스레드의 최소 개수를 설정하자.<br>
큐 제한에 이르면 큐에 태스트를 추가하려는 시도는 실패할 것이다.
 `ThreadPoolExecutor`에는 이런 경우를 제어하는 `rejectedExecution`
 메서드(디폴트로 `RejectedExecutionException`를 던진다)가 있다.
 (`SynchronousQueue`) 태스크가 도착했을 때 최대 개수의 스레드가 이미 작업
 중이라면 태스크는 거부된다. (`LinkedBlockingQueue`) 무한 큐를 사용할 때
 거부되는 태스크는 없을 것이다. (`ArrayBlockingQueue`) 추가 스레드는 큐가 가득
 찼을 때만 시작된다.<br>
자바 7은 (재귀적인 불할 정복 알고리즘을 위한) `ForkJoinPool` 클래스라는 새로운
 스레드 풀을 도입한다. 부모 태스크는 자식 태스크가 완료될 때까지 대기해야만
 하므로 `ThreadPoolExecutor`를 사용해서 효율적으로 알고리즘을 수행하기란
 불가능하다. 반면에 `ForkJoinPool`은 스레드가 신규 태스크를 생성하고 현재의
 태스크를 대기시키도록 한다. 여기서 (`RecursiveTask<T>`의) `fork()`와 `join()`
 메서드가 핵심이다. `ForkJoinPool`의 추가 기능은 작업 훔쳐오기(work-stealing)를
 구현한 것이다. 스레드는 자체 큐에서 나온 태스크를 우선적으로 작업하지만 큐가
 비어있다면 다른 스레드의 큐에서 나온 태스크를 훔칠 것이다. `ForkJoinPool`은
 태스크가 불균형을 이룰 때 (`ThreadPoolExecutor`보다) 더 나은 성능을
 제공한다.<br>
(자바의 현재 버전은 배열 크기가 47개 미만의 요소를 가질 때 삽입 정렬을
 사용한다.)<br>
자바 8은 특정 종류의 코드를 자동으로 병렬화시키는 기능을 도입했다. 이 병렬화는
 `ForkJoinPool` 클래스의 기능에 의존한다. 공유 풀 크기는 시스템 속성
 `-Djava.util.concurrent.ForkJoinPool.common.parallelism=N`에 명시해서 설정할 수
 있다. 공유 풀이 단일 스레드만 쓰도록 설정되었다 해도 `forEach()` 메서드는
 결과를 계상하는 데 두 개의 스레드(문장을 실행하는 스레드와 스크림에서 유래된
 데이터를 처리하기 위한 공유 풀 내의 스레드)를 사용한다.

```java
public class Termometer {
    private static ThreadLocal<NumberFormat> nfLocal = new ThreadLocal<>() {
        public NumberFormat initialValue() {
            NumberFormat nf = NumberFormat.getInstance();
            nf.setMinimumIntegerDigits(2);
            return nf;
        }
    }
    public String toString() {
        NumberFormat nf = nfLocal.get();
        nf.format(...);
    }
}
```

`java.util.concurrent.atomic` 패키지 내의 클래스는 전통적인동기화 대신 CAS 기반
 기본형을 사용한다. 결과적으로 이 클래스들(예를 들어 `AtomicLong` 클래스)의
 성능은 적어도 CAS 기본형에 대한 경쟁이 너무 높아지기 전까지는 long 변수를
 증가시키기 위해 동기화된 메서드를 쓰는 것보다 더 빠른 경향이 있다. 자바 8은
 이걸 다루기 위해 원자 단위 가산기(adders)와 누산기(accumulators)와 같은 다수의
 클래스(예를 들어 `LongAdder` 클래스)를 도입한다. 이 클래스는 전통적인 원자 단위
 클래스보다 확장성이 더 높다.<br>
엄밀히 말해서 거짓 공유(False Sharing)에는 동기화된(또는 `volatile`) 변수를
 포함시킬 필요가 없다. CPU 캐시 내에 데이터 값을 쓸 때마다 동일한 데이터 범위를
 유지하는 다른 캐시는 무효화돼야 한다. (JVM이 인터슽너 변수의 배치를 재정리해서
 모든 배열은 서로 이웃하게 놓이고 모든 `long`형 변수도 서로 인접하게 된다.) 자바
 8의 신규 기능 중 하나는 특정 필드에서 캐시 경쟁을 줄일 수 있다(JEP 142)는
 점이다. 이건 JVM이 자동으로 패딩해야 하는 변수를 표시하기 위해 신규
 어노테이션(`@sun.misc.Contended`)을 쓰면 된다. 기본적으로 JVM은 JDK의 클래스
 내를 제외하고 이 어노테이션을 무시한다. 애플리케이션 코드에서 이 어노테이션을
 사용할 수 있게 하려면 디폴트가 `true`인 `-XX:-RestrictContended` 플래그를
 포함시키자. 반면에 JDK 내에서 일어나는 자동 패딩을 비활성화시키려면 디폴트가
 `true`인 `-XX:-EnabledContended` 플래그를 설정하자. 이건 `Thread`와
 `ConcurrentHashMap` 클래스의 크기 감소로 이어질 것이다.

대개 많은 애플리케이션은 실제로 32bit JVM 내에서는 128KB 스택 크기로, 64bit JVM
 내에서는 256KB 스택 크기로 실행할 수 있다. (기본 스택 크기 - 32bit 320~512KB,
 64bit 1MB)<br>
대부분의 애플리케이션을 포함하여 스레드 풀을 사용하는 애플리케이션은 편향된 락이
 유효하면 보통 성능이 나빠진다. 프로그래밍 모델에서 다른 스레드들이 동일하게
 경쟁하는 락에 접근할 가능성이 있다. 이런 종류의 애플리케이션에서는
 `-XX:-UseBiasedLocking` 옵션을 써서 편향된 락을 비활성화시키면 성능이 약간
 개선될 수도 있다.<br>
락에 접근하다 차단된 스레드를 알림을 받을 큐에 귀속시키기 전에 스핀을 돌 횟수를
 자동 조정하며 JVM은 이 두 가지 경우의 사이에서 적절하게 균형을 맞출 것이다.
 자바의 이전 버전에서는 스핀 락을 활성화하거나 비활성화하는 플래그로
 `-XX:+UseSpinning`를 지원했었다. 자바 7과 그 이상의 버전에서 이 플래그는 더
 이상 효과가 없다. 스핀 락의 사용은 비활성화시킬 수 없다. 좀 이상하지만 스피닝이
 항상 동작을 하더라도 이 플래그의 디폴트 값은 `false`로 알려져 있다.<br>
유닉스 기반 시스템에서는 스레드의 자바 레벨 우선순위에는 영향이 거의 없고 전체
 우선순위에 대한 계산은 스레드가 마지막으로 실행된 후의 기간에 따라 좌우된다.
 윈도우에서는 더 높은 자바 우선순위를 가진 스레드가 더 낮은 우선순위를 가진
 스레드보다 더 많이 실행되는 경향이 있지만 우선순위가 낮은 스레드라 해도 꽤 많은
 CPU 시간을 쓴다. 어느 경우에나 스레드의 우선순위로는 성능에 영향을 줄 수 없다.
 일부 태스크가 다른 태스크보다 더 중요하다면 애플리케이션 로직에서 이들의
 우선순위를 정하게 해야 한다. (어느 범위까지) 이걸 가능하게 하는 방법은 태스크에
 다른 스레드 풀을 할당하고 풀의 크기를 변경하는 것이다.<br>
스레드 스택을 살펴보는 데 있어 첫 번째 경고 사항은 JVM이 특정 지역
 세이프포인트(safepoints)에서 스레드의 스택 덤프만 뜰 수 있다는 점이다. 두
 번째로 스택은 한 번에 각 스레드별로 덤프를 뜨기 때문에 이들로부터 얻은 정보가
 상충될 가능성이 있다. 스레드 두 개가 동일한 스택을 보유하거나 스레드가 아무
 스레드도 잡고 있지 않은 락을 기다리며 대기하고 있는 것처럼 보일 수 있다.<br>
이 책의 [온라인 예제][parsejstack]에는 한 개 이상의 스레드 덤프에서 모든
 스레드의 상태를 요약할 수 있는 `jstack` 출력물을 위한 기본적인 파서를 갖고
 있다.

[parsejstack]: https://github.com/ScottOaks/JavaPerformanceTuning/tree/master/JStack

## 10. 자바 엔터프라이즈 에디션 성능

세션에 할당된 상태 유지 빈은 EJB 풀 내에 없다.<br>
XML과 JSON 중 어떤 표현 방식을 쓰든 전송될 때 압축하면 큰 이익을 얻을 수 있고
 크기도 비슷해진다.<br>
SAX (푸시) 파서는 StAX (풀) 파서보다 더 빠른 경향이 있다.<br>
사용할 팩토리를 정하기 위해 순서대로 `javax.xml.stream.XMLInputFactory` 옵션을
 살펴본다. `-D` → JAVA/jre/lib/jaxp.properties → META-INF/services/ → JDL 기본
 팩토리

```java
SAXParserFactory spf = SAXParserFactory.newInstance();
spf.setValidating(true);
spf.setNamespaceAware(true);
SAXParser parser = spf.newSAXParser();
parser.setProperty(JAXPConstants.JAXP_SCHEMA_LANGUAGE,
       XMLConstants.W3C_XML_SCHEMA_NS_URI);
XMLReader xr = parser.getXMLReader();
xr.setErrorHandler(new MyCustomErrorHandler());

// 스키마 객체 재사용 - 스키마 객체를 파서 팩토리와 같이 사용
SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
StreamSource ss = new StreamSource(rp.getSchemaFilename());
Schema schema = sf.newSchema(new Source[]{ss});
SAXParserFactory spf = SAXParserFactory.newInstance();
spf.setValidating(false); // setSchema()와 setValidating() 메서드는 상호 배타적
spf.setNamespaceAware(true);
spf.setSchema(schema);
parser = spf.newSAXParser();

// 스키마 객체 재활용 - Validator 클래스의 인스턴스 사용
//  파싱을 검증과 분리시켜서 두 가지 동작을 다른 시기에 수행할 수 있도록 한다.
SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
StreamSource ss = new StreamSource(rp.getSchemaFilename());
Schema schema = sf.newSchema(new Source[]{ss});
XMLInputFactory staxFactory = XMLInputFactory.newInstance();
staxFactory.setProperty(XMLInputFactory.IS_VALIDATING, Boolean.FALSE);
XMLStreamRedaer xsr = staxFactory.createXMLStreamReader(ins);
XMLStreamReader reader = new MyXMLStreamReader(xsr);
Validator validator = schema.newValidator();
validator.validate(new StAXSource(reader));
```

최적화 코드에서 배열이 역직렬화될 때 결국 JFK 참조는 개별적인 다른 객체가 됨을
 주의하자.

## 11. 데이터베이스 성능 베스트 프랙티스

JDBC 드라이버는 자바 애플리케이션(데이터베이스 클라이언트) 내 또는 데이터베이스
 서버에서 작업을 더 많이 수행하도록 만들어질 수 있다. 여기에 대한 최고의 예제는
 오라클 데이터베이스의 thin과 thick 드라이버다. thin 드라이버는 자바
 애플리케이션 내에서 꽤 적은 공간을 차지하도록 만들어졌다. 이건 데이터베이스
 서버가 처리를 더 많이 하도록 한다. thick 드라이버는 반대다.<br>
JDBC 드라이버는 네 가지 타입(1~4)이 있다. 오늘날 널리 쓰이는 드라이버 타입은
 (네이티브 코드를 사용하는) 타입 2와 (순수 자바인) 타입 4다. 타입 1 드라이버는
 ODBC와 JDBC 간의 가교 역할을 한다. 타입 1 드라이버는 보통 성능이 그다지 좋지
 않으므로 가능한 한 ODBC를 피하는 편이 좋다. 타입 4 드라이버와 같이 타입 3
 드라이버는 순수하게 자바로만 작성되나 일부 미들웨어(일반적이진 않지만 때론
 애플리케이션 서버)가 중간 번역을 제공하는 특정 구조를 위해 설계된다.<br>
원격 JDBC 호출 한 번에 전송 - `ps.addBatch()` → `ps.executeBatch()`<br>
트랜잭션 분리 모드:
* `TRANSACTION_SERIALIZABLE` - 이건 가장 비용이 비싼 트랜잭션 모드다. 직렬화된
  트랜잭션은 쿼리를 입력할 때마다 항상 동일한 데이터를 보게 될 것이다.
* `TRANSACTION_REPEATABLE_READ` - 이건 트랜잭션 동안 모든 데이터가 락으로 잠길
  필요가 있다. 하지만 다른 트랜잭션은 언제든 테이블로 새로운 데이터를 생성할 수
  있다. 이 모드는 팬텀 읽기가 일어나게 하며 WHERE절이 있는 쿼리를 재수행하면
  트랜잭션 내에서 쿼리가 두 번째 수행될 때 다른 데이터가 조회될 수 있다. (MySQL
  디폴트, 오라클 미지원)
* `TRANSACTION_READ_COMMITTED` - 이 모드는 트랜잭션이 일어나는 동안 쓰인 열
  하나만 락으로 잠근다. 이건 반복 불가능 읽기(nonrepeatable read)로 이어진다.
  트랜잭션 내의 한 지점에서 읽은 데이터는 트랜잭션 내의 다른 지점에서 읽은
  데이터와 다를 수 있다. (오라클과 DB2 디폴트)
* `TRANSACTION_READ_UNCOMMITTED` - 락으로 잠기지 않으므로 한 트랜잭션은 다른
  트랜잭션 내에 작성된(하지만 커밋되지 않은) 데이터를 읽을 수 있다. 커밋되지
  않은 읽기(dirty read)로 알려져 있다. 첫 번째 트랜잭션은 롤백되고 두 번째
  트랜잭션은 틀린 데이터에서 일어나기 때문에 여기서 문제가 일어난다. (오라클
  미지원)
* `TRANSACTION_NONE` - (커밋 불가, 읽기 전용 쿼리)

JPA에서는 엔터티별(물론 엔터티란 [적어도 일반적으로] 단순히 데이터베이스 내의
 열을 가리킨다)로 락 레벨을 명시할 수 있다. 락 레벨로 알맞게 얻는 건 매우 어려울
 수 있으므로 JDBC 구문 내에서 락을 수행하기보다 JPA를 이용하는 편이 더 쉽다.<br>
JDBC 레벨에서 기본적으로 연결의 분리 레벨을 TRANSACTION_READ_UNCOMMITTED로
 설정하고 트랜잭션이 일어나는 동안 락으로 잠글 필요가 있는 데이터만 명시적으로
 잠근다. 이를 이루기 위한 SQL문은 비표준이다. 필요한 락 레벨을 맞추는 방법을
 확인하기 위해 데이터베이스 벤더의 문서를 살펴봐야 하지만 공통 구문은 FOR
 UPDATE절을 포함한다. 이 종류의 락은 비관적인 락이라 불린다. 락 성능은 때론
 낙관적인 락을 사용해서 개선할 수 있으며 데이터에 접근할 때 경쟁하지 않는다면
 성능상의 큰 이득을 얻게 될 것이다. 데이터베이스에서 낙관적 병렬 수행은 버전
 칼럼으로 구현된다. 트랜잭션이 완료될 때가 되면 트랜잭션은 버전 칼럼을 갱신한다.
 문제의 열이 반복 읽기나 직렬화 구문을 필요로 하면 트랜잭션 동안 데이터를 읽기만
 하더라도 갱신되어야 한다. 커밋된 읽기 구문을 위해 버전 칼럼은 열 내의 다른
 데이터도 변경될 때만 갱신될 필요가 있다.<br>
이를 제어하기 위해 JDBC 드라이버가 한 번에 얼마나 많은 열을 전송해야 하는지 알
 수 있다록 `PreparedStatement` 객체의 `setFetchSize()` 메서드를 쓰자. 이 디폴트
 값은 JDBC 드라이버마다 다양하다. 예를 들어 오라클의 JDBC 드라이버에서 디폴트
 값은 10이다.

JPA는 엔터티 클래스의 바이트 코드를 변경해서 성능을 많이 개선한다. 일부 JPA
 구현체도 클래스들이 JVM으로 로드되면서 바이트 코드를 동적으로 개선하기 위한
 방법을 제공한다.<br>
간단한 네임드 쿼리를 사용하는 편이 데이터를 로드하기 더 빠를 것 같더라도
 엔티티가 `find()` 메서드에 대한 호출을 통해 L2 캐시로 로드되면 장기적으로 무슨
 일이 일어날지 고려하자.

## 12. 자바 SE API 팁

자바 7 이전의 문제는 클래스를 로드하는 메서드가 동기화됐다는 점이었다. 이런
 이유로 해서 JDK6 동작은 `-XX:+AlwaysLockClassLoader` 플래그를 이용해서 활성화할
 수 있다.

`Random`과 `ThreadLocalRandom` 클래스의 차이점은 `Random` 클래스의 주요 동작이
 동기화된다는 것이다. 이 클래스들과 `SecureRandom` 클래스 간의 차이는 사용하는
 알고리즘에 있다. 이와 같은 애플리케이션에서 성능 테스트를 수행할 때 타이밍은
 매우 변동이 심하다는 점을 인식하고 있어야 한다.

가능한 한 가장 빠른 코드를 작성하는 데 관시이 있다면 JNI를 피하는 게 좋다.<br>
흥미롭게도 반대는 사실이 아니라고 할 수 있다. C 코드에서 자바로 다시 호출하는
 경우는 큰 성능상의 불이익을 주지 않는다. JNI 코드는 관련 매개 변수가 단순한
 기본형이 아니라면 더 나쁘게 동작한다. 자바에서 C로 호출하면 C 함수로 대상
 객체를 명시적으로 넘긴다. C에서 자바를 호출할 때는 객체를 넘기지 않는다. 배열
 형태의 데이터 처리는 네이티브 코드에서 특별히 다룰 대상이다. 배열이 고정된 동안
 가비지 컬렉터는 동작할 수 없으므로 JNI 코드 내에서 가장 비용이 비싼 실수 중의
 하나는 오래 동작하는 코드 내의 문자열이나 배열을 고정하는 것이다.

성능상의 이슈는 스택 트레이스를 처리하는 데서 오므로
 `-XX:-StackTraceInThrowable` 플래그로 스택 트레이스 생성이 비활성화되도록
 설정할 수 있지만, 좋은 생각이 아니다. 코드에서 실제로 스택 트레이스를 조사하고
 거기에서 발견된 사항을 기반으로 한 예외를 해결할 방법을 결정한다. (CORBA의 참조
 구현체는 이처럼 동작한다.) 이 자체는 문제가 많지만 스택 트레이스를 비활성화시킨
 결과로 인해 이상하게 코드가 깨질 수 있다.

`AggressiveOpts` 플래그는 기본적인 자바 SE 기능 중 동작 몇 개에 영향을 준다. 이
 플래그의 목적은 시험 삼아서 최적화를 도입하는 것이다. `AggressiveOpts` 플래그
 사용에 따른 주요 영향은 기본 JDK 클래스 몇 개를 다른 구현체로 대체한다는
 것이다. 특히 `java.math` 패키지의 `BigDecimal`, `BigInteger`와
 `MutableBigDecimal` 클래스, `java.text` 패키지의 `DecimalFormat`,
 `DigitalList`(?)와 `NumberFormat` 크래스, `java.util` 패키지의 `HashMap`,
 `LinkedHashMap`와 `TreeMap` 클래스가 그 대상이다. 자바 8에서 베이스 JDK
 클래스에 통합되거나 다른 방식으로 개선되면서 이 대체 구현체는 제거됐다.
 `AggressiveOpts` 플래그를 설정하면 `AutoFill` 플래그를 사용할 수 있는데,
 JDK7에서 7u4까지 디폴트는 `false`다. 이 플래그는 컴파일러가 루프 최적화를 더
 잘할 수 있게 해준다. 마찬가지로 이 플래그는 `DoEscapeAnalysis` 플래그(JDK 7u4
 이후 버전에서 디폴트로 설정)도 활성화시킨다. `AutoBoxCacheMax` 플래그(디폴트 값
 128)는 20,000까지 설정되고 더 많은 값이 오토박싱되게 하며 (메모리를 약간 더
 사용하는 대신) 특정 애플리케이션에서 성능을 약간 높일 수 있다.
 `BiasedLockingStartupDelay`의 값은 디폴트인 2,000에서 500까지 줄일 수 있으며
 이건 편향된 락이 애플리케이션이 수행되기 시작한 후 바로 시작한다는 의미다.
 마지막으로 이 플래그는 `OptimizeStringConcat` 플래그를 사용할 수 있는데,
 `OptimizeStringConcat` 플래그를 사용할 수 있다면 JVM 저스트 인 타임 컴파일러는
 `StringBuilder` 객체 자체의 생성을 최적화할 수 있다. `OptimizeStringConcat`
 플래그는 7u4부터 디폴트가 `true`이다.

결국에 성능이 동일하므로 람다를 위한 코드를 살펴보면 익명 클래스를 생성하기 위한
 간편 문법이라는 결론을 내리는 것이 당연하다. 하지만 실제로는 그렇게 돌아가지
 않으며 람다를 위한 코드는 JDK 8 내의 특별한 헬퍼 클래스를 통해 호출되는 스태틱
 메서드를 생성한다. 익명 클래스는 실제 자바 클래스로, 개별 클래스 파일을 갖고
 있고 클래스로더에서 로드된다.

## Appendix A. 튜닝 플래그 요약

