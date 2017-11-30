# [프로 AngularJS][homepage], 애더 프리먼 저, 유윤선 역, 비제이퍼블릭 (2015)

[source code][source_code]

AngularJS 1.2.5, Bootstrap 3.0.3

[homepage]: http://www.bjpublic.co.kr/skin12/product_list.php?boardT=v&goods_data=aWR4PTE0NiZzdGFydFBhZ2U9MzYmbGlzdE5vPTExMyZ0YWJsZT1yZWRfZ29vZHMmcGFnZV9pZHg9OSZzZWFyY2hfaXRlbT0=||
[source_code]: http://www.apress.com/kr/book/9781430264484

## 1부. 준비하기

### 1장. 준비하기

Batarang AngularJS (Chrome extension)<br>
[Internet Explorer Compatibility][ie_compatibility]<br>
Extras 추가 모듈 다운로드<br>
[LiveReload][livereload] (상용), [deployd][deployd]

[ie_compatibility]: https://docs.angularjs.org/guide/ie
[livereload]: http://livereload.com/
[deployd]: http://deployd.com/

### 2장. 첫 번째 AngularJS 앱

AngularJS 앱은 하나 이상의 모듈로 구성된다. AngularJS를 제이쿼리 같은 다른
 기술과 함께 사용하는 경우에는 `ng-app` 어트리뷰트를 문서 내 엘리먼트에
 적용함으로써 AngularJS 앱이 차지하는 범위를 좀 더 제한할 수 있다. `ng-app`이
 디렉티브이고, `data-ng-app`과 같이 적용할 수도 있다.<br>
뷰가 항상 전체 모델에 접근할 수 있는 것은 적절하지 않으므로 컨트롤러를 사용해
 뷰가 접근할 수 있는 데이터 영역을 선택하는데, 이를 스코프라고 부른다. AngularJS
 앱에서 `$`로 시작하는 변수명은 AngularJS에서 제공하는 내장 기능을 나타낸다.
 `$`가 있으면 이는 주로 내장 서비스를 나타낸다.<br>
`ng-controller` 어트리뷰트를 사용하여 컨트롤러가 책임질 HTML 문서 영역을
 지정한다.<br>
아마 대다수 독자들은 개별 엘리먼트에 이벤트 처리 코드를 추가하지 말라고 배웠을
 텐데, 여기서 `ng-click` 디렉티브를 `button` 엘리먼트에 추가하니 이상해 보일
 수도 있을 것이다. 하지만 걱정하지 않아도 된다. AngularJS가 HTML 파일을
 컴파일하다 디렉티브를 만나면, AngularJS는 이벤트 핸들러 코드가 엘리먼트와
 분리되게끔 핸들러를 설정해준다.

```javascript
todoApp.run(function ($http) {
    $http.get("todo.json").success(function (data) {
       model.items = data;
    });
});
```

### 3장. AngularJS 이해

실제로, AngularJS에는 제이쿼리의 축소 버전인 jqLite가 들어 있다. 또, 제이쿼리를
 HTML 문서에 추가하면 AngularJS에서는 이를 자동으로 감지해 jqLite 대신
 제이쿼리를 사용한다(물론 실제로 제이쿼리를 추가할 일은 거의 없겠지만).<br>
MVC 패턴의 목적은 모델에서 로직을 제거하는 게 아니다. 오히려 모델 안에 모델
 데이터를 생성, 관리하는 데 필요한 로지만 들어가게 하는 것이다.<br>
AngularJS 애플리케이션에서 데이터가 도메인 모델만 있는 것은 아니다. 컨트롤러는
 뷰 데이터(뷰 모델 데이터 또는 뷰 모델이라고도 부름)를 생성해 뷰 정의를
 단순화한다. 뷰 데이터는 영속적이지 않으며 도메인 모델 데이터의 일부를
 합성하거나 사용자 상호작용에 반응해 생성한다.<br>
AngularJS의 가장 강력한 특징 중 하나는 특히 디렉티브 기능과 관련해 제이쿼리를
 기반으로 개발됐다는 점이다. AngularJS를 사용하기로 했다면 끝없는 문제를
 야기하는 제이쿼리 편의 기능에 의존하지 말아야 한다.

### 4장. HTML 및 부트스트랩 CSS 기초

테이블을 정의할 때 `thead` 엘리먼트를 사용하지 않은 경우 브라우저는 `table`
 엘리먼트의 직계 자손인 `tbody` 엘리먼트 및 `tr` 엘리먼트를 자동으로
 추가한다.<br>
`<meta name="viewport" content="width=device-width, initial-scale=1">`

### 5장. 자바스크립트 기초

`angular` - `isFunction()`, `isString()`, `lowercase()`, `uppercase()`,
 `isNumber()`, `extend(myExtendedObject, myData)` (`copy()`), `isObject()`,
 `forEach(myData, function (value, key) { ... })`, `equals()` (`==`과 `===`는
 객체 주소 비교), `isArray()`, `isDefined()`, `isUndefined()`, `fromJson()`,
 `toJson()`<br>
`delete`, `in`, `String()`, `Number()`, `parseInt()`, `parseFloat()`<br>
문자열 결합 연산자(`+`)는 덧셈 연산자보다 우선시된다.<br>
프로미스 - `error(callback)`, `success(callback)`, `then(success[, err])`<br>
`$http.get()` (자동 JSON 인코딩/디코딩)

### 6장. 스포츠 상점: 실전 애플리케이션

### 7장. 소프츠 상점: 내비게이션 및 결제

### 8장. 스포츠 상점: 주문 및 관리자 기능

## 2부. AngularJS 활용

### 9장. AngularJS 앱 해부

### 10장. 바인딩 및 템플릿 디렉티브 활용

### 11장. 엘리먼트 및 이벤트 디렉티브 활용

### 12장. 폼 활용

### 13장. 컨트롤러 및 스코프 활용

### 14장. 필터 활용

### 15장. 커스텀 디렉티브 구현

### 16장. 고급 디렉티브 구현

### 17장. 고급 디렉티브 기능

## 3부. AngularJS 서비스

### 18장. 모듈 및 서비스 활용

### 19장. 전역 객체, 에러, 표현식을 위한 서비스

### 20장. Ajax 서비스 및 프로미스

### 21장. REST 서비스

### 22장. 뷰를 위한 서비스

### 23장. 애니메이션 및 터치를 위한 서비스

### 24장. 프로비전 및 주입을 위한 서비스

### 25장. 단위 테스트

