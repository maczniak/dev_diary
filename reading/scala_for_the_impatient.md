# [쉽게 배워서 빨리 써먹는 스칼라 프로그래밍][homepage], 케이 호스트만 저, 서광열 역, 비제이퍼블릭 (2013)

[(원서) SCALA for the Impatient][english_edition] ([errata][errata]) by Cay S. Horstmann, Addison-Wesley (2012)

[Scala levels][scala_levels]

[homepage]: http://www.bjpublic.co.kr/skin12/product_list.php?boardT=v&goods_data=aWR4PTk2JnN0YXJ0UGFnZT03MiZsaXN0Tm89NjAmdGFibGU9cmVkX2dvb2RzJnBhZ2VfaWR4PTkmc2VhcmNoX2l0ZW09||
[english_edition]: http://horstmann.com/scala/
[errata]: http://horstmann.com/scala/bugs.html
[scala_levels]: http://www.scala-lang.org/old/node/8610

## 1. 기본기 (A1)

[Scala Installation Instructions][scala_install]<br>
implicitly converted `StringOps`, `RichInt`, `RichDouble`, `RichChar`<br>
singleton object, companion object instead of static method<br>
omitted `apply` method<br>
[Scaladoc][scaladoc]

[scala_install]: http://horstmann.com/scala/install/
[scaladoc]: http://www.scala-lang.org/api/

## 2. 제어 구조와 함수 (A1)

`val s = if (x > 0) 1 else -1`<br>
`:paste`, Ctrl+K<br>
assignment and the empty block statement, `{}`, have `()` value (`Unit` type).<br>
`1 to n`, `0 until n`<br>
`import scala.util.control.Breaks._`, `breakage { ... break; ... }`<br>
generator `for (i <- 3; j <- 1 to 3 if i != j)`<br>
for comprehension `for (i <- 1 to 10) yield i % 3`<br>
must tell parameter types / must tell a return type, too, if recursive (because Hindley-Milner algorithm cannot handle subtypes)<br>
variable length parametres `def sum(args: Int*) = { ...` (`Seq` type) / `sum(1 to 5: _*)`<br>
procedures return Unit type. `def box(s : String) { ...`<br>
`lazy val` has the cost that calls the method that check whether it has been initialized thread-safely.<br>
no checked exceptions<br>
`throw` has `Nothing` type that follows the other type in if branches.

## 3. 배열 사용하기 (A1)

yield 동일한 collection

immutable 참조 변경 가능?
Array s(0) .toBuffer <-> scala.collection.mutable.ArrayBuffer .toArray
+= 1, += (1, 2, 3, 5), ++= Array(8, 13, 21)
append amortized constant time, inefficient element removal
Array can be sorted in-place `scala.util.Sorting.quickSort(a)`. ArrayBuffer cannot be sorted in-place.
use `.mkString` instead of `.toString`. ex) `a.mkString(“ and “)`, `a.mkString(“<“, “,”, “>”)`
`Array.ofDim[Double](3, 4)`
import scala.collection.JavaConversions.bufferAsJavaList; ArrayBuffer[T] -> Java List<T>, import scala.collection.JavaConversions.asScalaBuffer; Java List<T> -> scala.collection.mutable.Buffer[T]

## 4. 맵과 튜플 (A1)

## 5. 클래스 (A1)

## 6. 오브젝트 (A1)

## 7. 패키지와 임포트 (A1)

## 8. 상속 (A1)

## 9. 파일과 정규 표현식 (A1)

## 10. 트레이트 (L1)

## 11. 연산자 (L1)

## 12. 고차함수 (L1)

## 13. 콜렉션 (A2)

## 14. 패턴 매칭과 케이스 클래스 (A2)

## 15. 어노테이션 (A2)

## 16. XML 처리 (A2)

## 17. 타입 인자 (L2)

## 18. 고급 타입 (L2)

## 19. 파싱 (A3)

## 20. 액터 (A3)

## 21. 암묵 (L3)

## 22. 제한된 컨티뉴에이션 (L3)

