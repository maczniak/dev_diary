# [리액트 교과서][homepage], [아자트 마르단][author] 저, [곽현철][translator] 역, 길벗 (2018)

[English homepage][english_homepage], [English publisher][english_publisher]<br>
[source code][source_code] ([ko][source_code_ko]), [forum][forum],
 [videos][videos]

React 15.5.4

[homepage]: http://www.gilbut.co.kr/book/bookView.aspx?bookcode=BN001998
[author]: http://azat.co/
[translator]: https://wanderer.work/
[english_homepage]: http://reactquickly.co/
[english_publisher]: https://www.manning.com/books/react-quickly
[source_code]: https://github.com/azat-co/react-quickly
[source_code_ko]: https://github.com/gilbutITbook/006961
[forum]: https://forums.manning.com/forums/react-quickly
[videos]: https://www.youtube.com/playlist?list=PLguYmmjtxbWHan1ZzI1o8Xxn0_JKXQLIr

## 1부 React 기초

### 1장 React 살펴보기

React는 상태 변경에 따라 뷰를 갱신한다. DOM 비교(diffing) 또는 상태와 뷰의
 보정(reconciliation), O(n) 알고리즘 [1][diffing_algorithm1],
 [2][diffing_algorithm2]<br>
순수한 자바스크립트로 구현한 컴포넌트 기반 아키텍처는 React 이전에 없었다.<br>
이벤트 핸들러는 브라우저의 원본 이벤트 객체 대신 이 원본 객체를 감싼 합성 이벤트
 객체(synthetic event object)를 전달받는다. React의 우수한 DOM 추상화를 증명하는
 또 다른 예는 서버 측 렌더링 기능이다.<br>
가상 DOM의 또 다른 장점 중 하나는 PhantomJS 같은 헤드리스 브라우저 없이도 단위
 테스트를 수행할 수 있다는 점이다. 페이스북이 Jasmine을 기반으로 만든 Jest를
 사용하면 명령줄에서 바로 React 컴포넌트를 테스트할 수 있다.<br>
React 컴포넌트 - [1][react_component1], [2][react_component2],
 [3][react_component3], [4][react_component4], [5][react_component5],
 [6][react_component6], [7][react_component7], [8][react_component8],
 [9][react_component9], [날짜 입력][react_datepicker],
 [폼 입력값 검증][react_forms], [자동완성][react_autocomplete]<br>
React를 UI 일부에만 적용할 수도 있다. React로 프론트엔드를 개발하기 위해 특정
 백엔드를 사용할 필요는 없다. `express-react-views` 라이브러리를 사용하면 React
 컴포넌트를 백엔드에서 렌더링할 수 있다.<br>
React 팀은 버전 0.14 이후부터는 기존의 React를 `react`와 `react-dom`이라는 두
 패키지로 분리해서 npm에 배포하기 시작했다. 예를 들어 버전 0.13에서는
 `React.render()` 메서드를 사용하여 웹 페이지의 DOM 노드에 엘리먼트를 넣었다.
 버전 0.14 이후부터는 이 부분이 `react-dom` 패키지로 이전되어
 `ReactDOM.render()` 메서드를 사용한다.
React 랜더링 대상 - [터미널][react_blessed], [벡터 그래픽][react_art],
 [`<canvas>`][react_canvas], [3D][react_three], [VR][react_360]<br>
[Add-Ons 문서][addons]<br>
저자는 템플릿에 데이터를 넣어 HTML로 만드는 과정을 설명하기 위해 hydrate라는
 단어를 사용했다. 비슷한 맥락으로 React 버전 16부터는 React 서버 측 렌더링을
 거친 HTML을 프론트엔드에서 사용할 때 ReactDom.hydrate 메서드를 사용한다.
 hydrate에 대한 좀 더 자세한 설명은 저자가 참조로 추가한
 [스택 오버플로우 답변][hydrate]을 참고하기 바란다.<br>
[RefluxJS][refluxjs], [Redux][redux], Meteor, [Flux][flux],
 [React Router][react_router], [React-Bootstrap][react_bootstrap]<br>
JSX는 자바스크립트로 변환하는 작은 언어라고 생각하면 좋다.

React 엘리먼트를 직접 `<body>`에 런더링하지 않는 이유는 무엇일까? 다른
 라이브러리나 `<body>`를 조작하는 브라우저 확장 프로그램과 충돌하는 것을
 방지하기 위해서다.<br>
React의 경고와 오류 메시지는 난독화 과정을 거친 프로덕션 빌드 react.min.js에는
 포함되어 있지 않다.<br>
`data-reactroot` 속성은 해당 태그를 ReactDOM이 렌더링했다는 것을 알려준다.<br>
[node-static][node_static], [http-server][http_server],
 [Fenix Web Server][fenix_web_server]

[diffing_algorithm1]: https://reactjs.org/docs/reconciliation.html
[diffing_algorithm2]: https://calendar.perfplanet.com/2013/diff/
[react_component1]: https://github.com/brillout/awesome-react-components
[react_component2]: https://devarchy.com/react
[react_component3]: http://react-toolbox.io/
[react_component4]: https://material-ui.com/
[react_component5]: https://github.com/OfficeDev/office-ui-fabric-react
[react_component6]: https://js.coach/
[react_component7]: https://react.rocks/
[react_component8]: http://khan.github.io/react-components/
[react_component9]: http://www.reactjsx.com/
[react_datepicker]: https://github.com/Hacker0x01/react-datepicker
[react_forms]: https://github.com/prometheusresearch/react-forms
[react_autocomplete]: https://github.com/reactjs/react-autocomplete
[react_blessed]: https://github.com/Yomguithereal/react-blessed
[react_art]: https://github.com/reactjs/react-art
[react_canvas]: https://github.com/Flipboard/react-canvas
[react_three]: https://github.com/Izzimach/react-three
[react_360]: https://facebook.github.io/react-360/
[addons]: https://reactjs.org/docs/addons.html
[hydrate]: https://stackoverflow.com/questions/6991135/what-does-it-mean-to-hydrate-an-object
[refluxjs]: https://github.com/reflux/refluxjs
[redux]: https://redux.js.org/
[flux]: https://github.com/facebook/flux
[react_router]: https://github.com/ReactTraining/react-router
[react_bootstrap]: https://react-bootstrap.github.io/
[node_static]: https://github.com/cloudhead/node-static
[http_server]: https://www.npmjs.com/package/http-server
[fenix_web_server]: http://fenixwebserver.com/

### 2장 React 첫걸음

엘리먼트는 컴포넌트의 인스턴스이며, 컴포넌트 클래스라고도 한다.<br>
[React Developer Tools][react_devtools]<br>
`createElement()`의 첫 번째 매개변수로 일반적인 HTML 태그 이름은 소문자로
 작성한다. React 컴포넌트 클래스의 이름은 대문자로 시작한다.

```javascript
class HelloWorld extends React.Component { // pre-ES6 React.createClass()
  render() { // pre-ES6 render: function()
```

[ECMAScript 6 호환성 표][es6_compat_table], [ES6 탐색하기][exploring_es6]<br>
속성(`this.props`)은 엘리먼튼 내의 변경할 수 없는 (ES5 `Object.freeze()`)
 값이라고 생각하자.<br>
React 버전 16부터는 표준이 아닌 HTML 속성도 (소문자로 변경하여) 렌더링하도록
 변경되었다.<br>
[Babel REPL][babel_repl]

[react_devtools]: https://github.com/facebook/react-devtools
[es6_compat_table]: http://kangax.github.io/compat-table/es6/
[exploring_es6]: http://exploringjs.com/es6/
[babel_repl]: http://babeljs.io/repl

### 3장 JSX

DOM 요소의 속성 값으로 불 값을 사용하는 경우, 이전 버전에서는 문자열 'true' 또는
 'false'로 나타냈으나, 버전 16부터는 무시하도록 변경되었다.<br>
`return <h1 {...this.props}>Hello</h1>`<br>
나머지 연산자 - `function (url, options, ...callbacks)`,
 `function (url, options, ...[error, success])`<br>
펼침 연산자 - 함수 호출 `arr1.push(...arr2)`, 배열 리터널
 `array2 = [...array1, x, y, z]`, new 연산자를 이용한 인스턴스 생성 시
 `var d = new Date(...dates)`, ES5에서 배열을 함수의 인자로 사용하려면 `apply()`
 함수를 사용해야 했다<br>
펼침 연산자의 문법은 나머지 연산자와 비슷하다. 그렇지만 나머지 연산자는 함수
 정의나 선언 시에 사용하고, 펼침 연산자는 함수 호출이나 리터럴에 사용한다.

```jsx
render() {
  return <div>{
    (sessionFlag) => { // 즉시실행함수(Immediately Invoked Function Expression, IIFE)
      if (sessionFlag)
        return <a href='/logout'>Logout</a>
      else
        return <a href='/login'>Login</a>
    }(this.props.user.session)
  }</div>
}
```

Babel의 예전 이름은 5to6였다. 예전에는 React 팀이 `react-tools`와 브라우저
 상에서 JSX 변환을 실행하는 JSXTransformer를 유지보수했지만, 버전 0.13부터
 Babel의 사용을 권장하고 `react-tools`와 JSXTransformer 개선 작업을 중단했다.
 브라우저 내에서 런타임 변환이 필요한 경우에는 Babel 버전 5에서 제공했던
 browser.js의 즉시 사용 가능한 배포판을 사용할 수 있다. Traceur도 Babel 대신
 사용할 수 있는 도구다. 끝으로 TypeScript도 `jsx-typescript`를 이용하면 JSX
 변환을 지원하는 것 같다.<br>
브라우저의 ES6 또는 ES.Next(가장 최신 기능을 아울러 부르는 이름) 구현이 지연되는
 문제를 해결하기 위해 Babel을 사용할 수 있다. Babel CLI를 이용한 컴파일 방법은
 Gulp, Webpack 같은 도구를 사용하여 빌드 설정을 하거나, Node.js 또는
 자바스크립트로 Babel API를 직접 이용한 스크립트를 작성하는 방법에 비해 설정해야
 할 부분이 적다.<br>
`npm i babel-cli@6.9.0 babel-preset-react@6.5.0 --save-dev` (Babel CLI를 전역에
 설치하는 것은 권장하지 않는다. 프로젝트가 다른 버전의 도구를 사용하는 경우에
 충돌이 발생할 수 있기 때문이다.)<br>
`npm run`으로 명령을 실행할 때 node_modules/.bin도 PATH 환경변수에 추가한다.
 `"build": "./node_modules/.bin/babel js/script.jsx -o js/script.js"`, `-w`
 (`--watch`), -d (`--out-dir`)<br>
React/JSX는 위험한 HTML 구문에 대해 자동으로 이스케이프를 적용한다. 소스 코드에
 특수문자를 직접 복사하거나 `\u`로 시작하는 이스케이프 시퀀스로 바꾼 후에
 넣는다.<br>
JSX의 `style` 속성은 성능을 목적으로 객체를 사용한다. JSX에서는 문자열 대신
 자바스크립트 객체를 전달하고, CSS 속성은 카멜 표기법으로 작성한다 (`font-size`
 대신 `fontSize`). 자바스크립트 객체를 변수에 저장하거나 중괄호를 이중으로
 작성하여(`{{...}}`) 인라인으로 작성할 수도 있다. 이중 중괄호를 잘 살펴보면
 바깥쪽의 중괄호는 JSX에서 사용되었고, 안쪽의 중괄호는 자바스크립트의 객체
 리터럴을 작성하기 위해 사용되었다. 자바스크립트 예약어이기 때문에 `class`와
 `for` 대신에 각각 `className`과 `htmlFor`를 사용한다.

### 4장 React 컴포넌트의 상태 객체

컴포넌트 속성(`this.props`)이나, 일반적인 변수(`inputValue`), 클래스
 속성(`this.inputValue`)으로는 처리할 수 없는데, 이것들을 현재 컴포넌트 내부에서
 변경하더라도 뷰를 자동으로 변경할 수 없기 때문이다.

```jsx
class Clock extends React.Component {
  constructor(props) {
    super(props)
    this.launchClock()
    // 클래스 속성 문법이 도입되면 생성자 밖에 선언할 수 있다.
    // createClass() 메서드에서는 초기 상태 설정에 getInitialState()를 사용한다.
    this.state = {
      currentTime: (new Data()).toLocaleString('en')
    }
  }
  launchClock() {
    setInterval(() => {
      console.log('Updating time...')
      this.setState({ // this.state = ...하면 안된다.
        // this.setState는 상태 객체 전체가 아니라 전달한 항목만 갱신한다.
        // this.replaceState()는 더 이상 작동하지 않는다.
        currentTime: (new Date()).toLocaleString('en')
      }) // 두번째 아규먼트 callback 함수를 생략했다.
    }, 1000)
  }
  render() {
    console.log('Rendering Clock...')
    return <div>{this.state.currentTime}</div>
  }
}
```

`setState()`가 `render()`를 실행시킨다는 점도 기억해야 한다. 코드가 외부
 데이터에 의존하는 매우 특이한 경우, 다시 렌더링하기 위해 `this.forceUpdate()`를
 호출할 수 있다.<br>
자동 바인딩은 화살표 함수로 생성된 함수가 현재 `this` 값을 갖게 됨을 의미하며,
 수동으로 하는 방법은 클로저에서 `bind(this)` 메서드를 사용하는 것이다. 변수에
 담아 두었다가 클로저에서 `this`를 참조하는 대신 이 값을 사용할 수도 있다.<br>
상태비저장 컴포넌트는 상태 객체가 없으며, 컴포넌트 메서드 또는 다른 React의
 라이프사이클 이벤트 또는 메서드를 갖지 않는다. 상태를 가지지 않는 컴포넌트가
 React의 가장 바람직한 사례라고 볼 수 있다. 상태가 필요하지 않다면 React
 컴포넌트를 함수로 선언할 수 있고 추천한다. 상태비저장 컴포넌트는 `propTypes`와
 `defaultProps`를 프로퍼티로 가질 수 있고, 엘리먼트 참조(`refs`)를 사용할 수
 없다.

### 5장 React 컴포넌트 라이프사이클 이벤트

이벤트 - 마운팅 (`componentWillMount()`, `componentDidMount()`), 갱신
 (`componentWillReceiveProps()`, `shouldComponentUpdate()`,
 `componentWillUpdate()`, `componentDidUpdate()`), 언마운팅
 (`componentWillUnmount()`)<br>
마운팅 이벤트는 React를 바깥 세상, 즉 다른 프레임워크, 라이브러리, 데이터 저장소
 등과 연결하는 데 사용하곤 한다. `componentDidMount()`는 브라우저에서만 한 번
 실행되고, 서버 렌더링에서는 실행되지 않는다. XHR 요청처럼 브라우저에서만
 실행해야 하는 코드를 구현할 때 편리하게 사용할 수 있다. 마운팅 과정이
 상대적으로 비용이 많이 드는 작업이기 때문에 마운팅 이벤트는 재렌더링 시에는
 호출되지 않는다.<br>
지원대상 브라우저가 promise를 이용해 XHR 요청을 보낼 수 있는 통일된 방식인
 `fetch()`를 지원하지 않는다면 폴리필을 사용하거나, 다른 HTTP 에이전트
 라이브러리인 superagent, request, axios 등을 이용하거나, jQuery의 `$.agent()`나
 `$.get()`을 사용할 수 있다.<br>
`componentWillReceiveProps(newProps)` 메서드는 컴포넌트를 최초로 렌더링할 때는
 실행되지 않는다. 이 메서드는 새로운 속성을 전달받고 다시 렌더링이 이뤄지기 전,
 새로운 속성에 따라 상태를 변경하려는 경우에 유용하다. 일반적으로
 `componentWillReceiveProps(newProps)`에서 `setState()` 메서드를 호출해도 추가로
 다시 렌더링이 발생하지는 않는다.<br>
`shouldComponentUpdate()` 이벤트는 초기 렌더링 시점이나 `forceUpdate()` 호출
 시에는 실행되지 않는다. `componentWillUpdate()`와 `componentDidUpdate()`
 메서드도 초기 렌더링 시에는 호출되지 않는다.<br>
`componentDidCatch()`는 React 버전 16부터 소개된 새로운 라이프사이클 메서드로
 오류나 예외 처리를 위해 사용할 수 있다. `componentDidCatch()`가 선언된
 컴포넌트는 하위 컴포넌트에서 발생하는 오류를 모두 처리할 수 있으므로 오류
 경계라고 부르는 것이다.

### 6장 React에서 이벤트 다루기

### 7장 React에서 폼 다루기

### 8장 확장성을 고려한 React 컴포넌트

### 9장 프로젝트: Menu 컴포넌트

### 10장 프로젝트: Tooltip 컴포넌트

### 11장 프로젝트: Timer 컴포넌트

## 2부 React 아키텍처

### 12장 Webpack 빌드 도구

### 13장 React 라우팅

### 14장 Redux를 이용한 데이터 다루기

### 15장 GraphQL을 이용한 데이터 다루기

### 16장 Jest를 이용한 React 단위 테스트

### 17장 Reactr와 Node.js를 이용한 유니버셜 자바스크립트

### 18장 프로젝트: React Router를 이용한 서점 만들기

### 19장 프로젝트: Jest를 이용한 비밀번호 검사

### 20장 프로젝트: Jest, Express, MongoDB를 이용한 자동완성 컴포넌트 구현

