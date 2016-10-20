# [한 권으로 끝내는 Node & Express][homepage], 이선 브라운 저, 한선용 역, 한빛미디어 (2015)

[원서][english_edition], [소스 코드][source_code]

[homepage]: http://www.hanbit.co.kr/store/books/look.php?p_code=B1448530214
[english_edition]: http://shop.oreilly.com/product/0636920032977.do
[source_code]: https://github.com/EthanRBrown/web-development-with-node-and-express

## 1. 익스프레스 소개

Express 4.0<br>
detect the licenses of node packages - license-sniffer, license-spelunker

## 2. 노드 시작하기

[nvm][nvm] (Node Version Manager)

[nvm]: https://github.com/creationix/nvm

## 3. 익스프레스로 시간 절약

[HTML5 Boilerplate][html5_boilerplate]<br>
`npm init` (create package.json file)<br>
`npm install --save express`<br>
redirection check by Ayima's [Redirect Path][redirect_path] Chrome extension<br>
경로 매개변수 매칭 - 대소문자 구분 없음, 맨 뒤의 슬래시 무시, 쿼리스트링 무시<br>
Node의 res.end(), res.writeHead() 대신 Express의 res.send(), res.set(), res.status(), res.type() 사용<br>
`au BufNewFile, BufRead *.handlebars (or *.hbs) set file type=html` in .vimrc

```
var handlebars = require('express-handlebars')
    .create({ defaultLayout:'main' }); // { extname: '.hbs' }
app.engine('handlebars', handlebars.engine);
app.set('view engine', 'handlebars');
```

`app.use(function(req, res, [next]){` // 404 폴백 핸들러 (미들웨어)<br>
`app.use(function(err, req, res, next){` // 500 에러 핸들러 (미들웨어)

[html5_boilerplate]: https://html5boilerplate.com/
[redirect_path]: https://chrome.google.com/webstore/detail/redirect-path/aomidfkchockcldhbkggjokdkkebmdll

## 4. 모양새 갖추기

[semver][semver], [package.json][package_json]<br>
`var fortune = require('./lib/fortune.js');` // 모듈 이름 앞에 ./를 붙이면 노드는 모듈을 node_modules 디렉터리에서 찾지 않습니다.

[semver]: https://www.npmjs.org/doc/misc/semver.html
[package_json]: https://docs.npmjs.com/files/package.json

## 5. 품질보증

### 서버 운영

[nodemon][nodemon], [grunt-nodemon][grunt_nodemon]

[nodemon]: https://npmjs.org/package/nodemon
[grunt_nodemon]: https://npmjs.org/package/grunt-nodemon

### 페이지 테스트

`npm install --save-dev mocha chai` assert나 expect 함수가 노드 프레임워크에 들어있지만 브라우저에는 없기 때문에 Chai 사용

```javascript
mocha.ui('tdd');
var assert = chai.assert;

suite('Global Tests', function(){
    test('page has a valid title', function(){
        assert(document.title && document.title.match(/\S/));
    });
});

mocha.run();
```

### 교차 페이지 테스트

가상 브라우저 - Selenium, JSPhantomJS, [Zombie.js][zombie_js]

```javascript
var Browser = require('zombie'),
    assert = require('chai').assert;

var browser;

suite('Cross-Page Tests', function(){

    setup(function(){
        browser = new Browser();
    });

    test('requesting ...', function(done){
        var referrer = 'http://...';
        browser.visit(referrer, function(){
            browser.clickLink('.requestGroupRate', function(){
                assert(browser.field('referrer').value == referrer);
                browser.assert.text('h1', 'Request Group Rate');
                done();
            });
        });
    });

});
```

`mocha -u tdd -R (report) spec qa/tests-crosspage.js 2>/dev/null`

[zombie_js]: http://zombie.js.org

### 논리 테스트

### 린트

JSLint, JSHint ([plugins for editors and IDEs][jshint_plugins]), [ESLint][eslint]

[jshint_plugins]: http://www.jshint.com/install
[eslint]: http://eslint.org

### 링크 체크

[LinkChecker][linkchecker]

[linkchecker]: http://wummel.github.io/linkchecker

### 그런트를 통한 자동화

```
npm install -g grunt-cli
npm install --save-dev grunt
npm install --save-dev grunt-cafe-mocha
npm install --save-dev grunt-contrib-jshint
npm install --save-dev grunt-exec
```

[Grunt plugins][grunt_plugins]<br>
Gruntfile.js<br>
[`grunt.option` document][about_grunt_option]

[grunt_plugins]: http://gruntjs.com/plugins
[about_grunt_option]: http://gruntjs.com/api/grunt.option

### 지속적 통합(CI)

Travis CI (`.travis.yml`), Jenkins [NodeJS Plugin][jenkins_nodejs_plugin], JetBrains TeamCity

## 6. 요청과 응답 객체

[인터넷 미디어 타입 목록][list_of_media_types]<br>
소스코드 읽기 - lib/application.js, lib/express.js, lib/request.js, lib/response.js, lib/router/router.js<br>
`res.render('about');` // use views/about.handlebars<br>
`res.render('custom-layout', { layout: 'custom' });` // use views/layouts/custom.handlebars

[list_of_media_types_list]: http://www.iana.org/assignments/media-types/media-types.xhtml

## 7. 핸들바를 사용한 템플릿

templating engine ([comparison][linkedin_picks_dustjs], [chooser][template_chooser]) - [Pug][pug] (Jade, default), [Dust][dust], [handlebars][handlebars]<br>
`{{variable}}`, `{{{verbatim}}}`, `{{! comment}}`, `{{#each foo}} {{else}} {{/each}}`, `{{#if foo}} {{else}} {{/if}}`, `{{#unless}} {{/unless}}`, within contexts `{{.}}` or `{{../foo}}`<br>
`npm install --save express-handlebars`<br>
`app.set('view cache', true);`<br>
`{{> weather}}` uses `views/partials/weather.handlebars` (or `partials.weather`).

```javascript
app.use(function(req, res, next){
    if(!res.locals.partials) res.locals.partials = {};
    res.locals.partials.weatherContext = getWeatherData();
    next();
});

var handlebars = require('express-handlebars').create({
    defaultLayout:'main',
    helpers: {
        section: function(name, options){
            if(!this._sections) this._sections = {};
            this._sections[name] = options.fn(this);
            return null;
        }
    }
});
```

```
{{#section 'head'}}
    ...
{{/section}}

{{{_sections.head}}
```

[ThemeForest][themeforest], [WrapBootstrap][wrapbootstrap]<br>

client-side handlebars

```
{{#section 'head'}}
    <script src="//cdnjs.cloudflare.com/ajax/libs/handlebars.js/1.3.0/handlebars.min.js"></script>
    <script id="nurseryRhymeTemplate" type="text/x-handlebars-template">
        Marry had a little <b>\{{animal}}</b>, its <b>\{{bodyPart}}</b>
        was <b>\{{adjective}}</b> as <b>\{{noun}}</b>.
    </script>
{{/section}}

{{#section 'jquery'}}
    <script>
        $(document}.ready(function(){
            var nurseryRhymeTemplate = Handlebars.compile(
                $('#nurseryRhymeTemplate').html());
            $nurseryRhyme.html(nurseryRhymeTemplate({ ... }));
        });
    </script>
{{/section}}
```

[lnkedin_picks_dustjs]: https://engineering.linkedin.com/frontend/client-side-templating-throwdown-mustache-handlebars-dustjs-and-more
[template_chooser]: http://garann.github.io/template-chooser/
[dust]: http://akdubya.github.io/dustjs/
[handlebars]: http://handlebarsjs.com/
[pug]: https://pugjs.org/api/getting-started.html
[themeforest]: https://themeforest.net/category/site-templates
[wrapbootstrap]: https://wrapbootstrap.com/

## 8. 폼 처리

## 9. 쿠키와 세션

## 10. 미들웨어

## 11. 이메일 보내기

## 12. 실무 관심사

## 13. 지속성

## 14. 라우팅

## 15. REST API와 JSON

## 16. 정적 콘텐츠

## 17. 익스프레스에서 MVC 구현

## 18. 보안

## 19. 타사 API와의 통합

## 20. 디버그

## 21. 사이트 오픈

## 22. 유지보수

## 23. 추가 자원

