# [DevOps와 SE를 위한 리눅스 커널 이야기: 단단한 서버 구축을 위한 12가지 키워드][homepage], 강진우 저, 인사이트 (2017)

[homepage]: http://www.insightbook.co.kr/12200

## 1장 시스템 구성 정보 확인하기

`/proc/cmdline` (`intel_idle.max_cstate=0`, `crashkernel=auto`),
 ``/boot/config-`uname -r` ``<br>
`dmidecode`, `lscpu`<br>
`smartctl -a /dev/sda -d cciss,0`<br>
`lspci`, `ethtool` `-g` (ring buffer), `-k` (성능 최적화 옵션,
 `generic-receive-offload` off), `-i` (커널 모듈 정보)

## 2장 top을 통해 살펴보는 프로세스 정보들

`top -b -n 1` (한번만 확인)<br>
`sar`의 `%commit` (할당만 해주고 실제 사용하지 않는 메모리)<br>
`vm.overcommit_memory` - 0 (page cache + swap + slab reclaimable까지), 1
 (무조건), 2 (`vm.overcommit_ratio`와 swap을 가지고 계산, `/proc/meminfo` 참고)

## 3장 Load Average와 시스템 부하

loadavg - load average figures giving the number of jobs in the run queue (state
 R) or waiting for disk I/O (state D) averaged (출처: proc manpage)<br>
`vmstat` - r: The number of processes waiting for run time, b: The number of
 processes in uninterruptible sleep<br>
[Siege][siege] 테스트 도구<br>
`/proc/sched_debug`

[siege]: https://www.joedog.org/siege-home/

## 4장 free 명령이 숨기고 있는 것들

정리하자면 Page Cache는 파일의 내용을 저장하고 있는 캐시, Buffer Cache는 파일
 시스템의 메타 데이터를 담고 있는 블록을 저장하고 있는 캐시라고 할 수 있다.
 그리고 각각이 `free`에서 표현하고 있는 cached, buffers 영역이다.<br>
`/proc/meminfo` - `SwapCached` (swap으로 빠진 메모리 영역 중 다시 메모리로
 돌아온 영역), `Active` (비교적 최근에 메모리 영역이 참조되어 swap 영역으로
 이동되지 않을 메모리 영역), `Inactive` (비교적 참조된 지 오래되어 Swap 영역으로
 이동될 수 있는 메모리 영역, 기준 `vm.min_free_kbytes` 시스템에서 유지해야 하는
 최소한의 free 메모리 양), `(anon)` (특정 파일의 내용을 저장하고 있는 Page Cache
 영역을 제외한 메모리 영역, 주로 프로세스들이 사용하는 메모리 영역을 지칭),
 `(file)` (I/O 성능 향상을 위해 사용하는 영역, buffers와 cached 영역이 속함),
 `Dirty`, `Slab` (커널이 직접 사용하는 영역, dentry cache, inode cache 등 커널이
 사용하는 메모리가 포함됨, buffers/cached 영역이 아니라 used 영역으로 계산됨),
 `SReclaimable` (Slab 영역 중 재사용될 수 있는 영역), `SUnreclaim` (Slab 영역 중
 재사용될 수 없는 영역)<br>
`slaptop -o`<br>
`lsof`로 확인하여 `/proc/sys/vm/drop_cached`로 강제 플러싱

## 5장 swap, 메모리 증설의 포인트

아주 적은 양이라도 swap 영역을 쓰기 시작했다면 반드시 살펴봐야 한다. 관리 용도의
 프로세스를 죽여서 메모리 부족 현상으로 인한 성능 저하를 해결할 수 있다. swap의
 사용 여부를 판단하는 것도 중요하지만 누가 swap을 사용하느냐도 매우 중요한 판단
 기준이 된다.<br>
`/proc/<pid>/smaps` (swpa 영역의 해당 번지 확인), `/proc/<pid>/status` (총합
 `VmSwap`), [smem][smem] 유틸리티<br>
커널은 버디 시스템을 통해서 프로세스에 메모리를 할당한다. 버디 시스템은 물리
 메모리를 연속딘 메모리 영역으로 관리한다. 이런 방식으로 메모리의 단편화도 막을
 수 있고 프로세스의 요청에 더 빠르게 응답할 수 있다. 버디 시스템의 현재 상황은
 `/proc/buddyinfo`에서 볼 수 있다. 각각의 행은 2의 배수이며 각각 연속 1개, 2개,
 4개의 영역을 의미한다.

[smem]: https://www.selenic.com/smem/

## 6장 NUMA, 메모리 관리의 새로운 세계

## 7장 TIME_WAIT 소켓이 서비스에 미치는 영향

## 8장 TCP Keepalive를 이용한 세션 유지

## 9장 TCP 재전송과 타임아웃

## 10장 dirty page가 I/O에 끼치는 영향

## 11장 I/O 작업이 지나가는 관문, I/O 스케줄러

## 12장 애플리케이션 성능 측정과 튜닝

## Appendix A 커널 디버깅을 위한 커널 컴파일

## Appendix B strace를 통한 애플리케이션 분석

## Appendix C tcpdump와 와이어샤크를 통한 TCP 패킷 분석

