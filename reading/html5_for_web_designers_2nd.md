# [웹디자이너를 위한 HTML5, 개정판][homepage], [제레미 키스][jeremy_keith], 레이철 앤드루 저, 김중철, 이시온 역, 웹액츄얼리코리아 (2016)

[원서][english_edition]

[homepage]: ???
[jeremy_keith]: https://adactio.com/
[english_edition]: https://abookapart.com/products/html5-for-web-designers

## 1장. 마크업 언어의 간략한 역사

## 2장. HTML5의 설계

[HTML Design Principles][html_design_principles]<br>
[htmllint][htmllint]<br>
[Google HTML/CSS Style Guide][google_html_css_style_guide]<br>
W3C [HTML draft specification][html_draft_spec] on github (old [HTML5][w3c_html5] and [HTML][w3c_html]), [WHATWG HTML live standard][whatwg_html_live_standard]<br>
[HTML5 API demos][html5_api_demos]

[html_design_principles]: https://www.w3.org/TR/html-design-principles/
[htmllint]: http://htmllint.github.io/
[google_html_css_style_guide]: https://google.github.io/styleguide/htmlcssguide.xml
[html_draft_spec]: http://w3c.github.io/html/
[w3c_html5]: https://www.w3.org/TR/html5/
[w3c_html]: https://www.w3.org/TR/html-markup/
[whatwg_html_live_standard]: https://html.spec.whatwg.org/multipage/
[html5_api_demos]: https://github.com/AurelioDeRosa/HTML5-API-demos

## 3장. 리치 미디어

[responsive images community group][responsive_images_community_group] -> [responsive issues community group][responsive_issues_community_group]<br>
[canvas sub DOM][canvas_sub_dom] for accessibility<br>
[jQuery Visualize][jquery_visualize] chart library<br>
[An Overview of SVG Sprite Creation Techniques][svg_sprite_technique]<br>
audio autobuffer [cannot be disabled][safari_audio_autobuffer] in Safari.<br>
[full-screen video background][fullscreen_video_background], ([example][video_background_example])<br>
[WebVTT][webvtt] (Web Video Text Tracks)

[responsive_images_community_group]: http://responsiveimages.org/
[responsive_issues_community_group]: http://ricg.io/
[canvas_sub_dom]: https://www.paciellogroup.com/blog/2015/02/html5-canvas-sub-dom/
[jquery_visualize]: https://www.filamentgroup.com/lab/update-to-jquery-visualize-accessible-charts-with-html5-from-designing-with.html
[svg_sprite_technique]: https://24ways.org/2014/an-overview-of-svg-sprite-creation-techniques/
[safari_audio_autobuffer]: https://bugs.webkit.org/show_bug.cgi?id=25267
[fullscreen_video_background]: https://envato.com/blog/video-background-html5-video/
[video_background_example]: http://mediaboom.com/
[webvtt]: https://developer.mozilla.org/en/docs/Web/API/Web_Video_Text_Tracks_Format

## 4장. WEB FORMS 2.0

input types - search, email, url, tel, range, number, date, datetime, datetime-local, time, month, week, color, pattern<br>
:required :valid :invalid

## 5장. HTML 의미론

[microformat][microformat] (표준 기구 없음, `class` 속성 사용), [microdata][microdata] (W3C 표준화 중단, `item*` 속성 사용, [schema.org][schema_org_with_microdata]에서 사용)<br>
[Semantics in HTML 5][html5_semantics], [Are Namespaces (and mU) Necessary?][harmful_namespace]<br>
Google [Structured Data][google_structured_data]<br>
`<mark>`, `<time>`, `<meter>`, `<progress>`<br>
Google [Web Authoring Statistics][google_web_authoring_statistics]<br>
주식 시세, 계산기, 시계, 날씨 등과 같이 단독으로 사용할 수 있는 위젯의 경우에 반드시 `article` 요소를 사용<br>
[Article or section?][article_or_section]<br>
`section`으로 나눈 콘텐츠마다 각각 독립적인 개요를 가지기 때문에 새로운 제목을 `h1`부터 작성할 수 있고 모듈화가 가능하다. `<style scoped>`는 현재 파이어폭스만 지원한다.

[microformat]: http://microformats.org/
[microdata]: https://www.w3.org/TR/microdata/
[schema_org_with_microdata]: https://schema.org/docs/gs.html
[html5_semantics]: http://alistapart.com/article/semanticsinHTML5
[harmful_namespace]: https://www.mnot.net/blog/2006/04/07/extensibility
[google_structured_data]: https://developers.google.com/search/docs/guides/intro-structured-data
[google_web_authoring_statistics]: https://developers.google.com/webmasters/state-of-the-web/
[article_or_section]: https://developers.whatwg.org/sections.html#article-or-section

## 6장. 지금 HTML5를 사용하려면

[Modernizr][modernizr] browser feature detection<br>
evergreen web browser is a web browser that automatically updates itself on startup.<br>
[Picturefill][picturefill] responsive image pollyfill<br>
[Can I use...][caniuse] and [HTML5 Accessibility][html5_accessibility]<br>
[ARIA][aria] (Accessible Rich Internet Applications) `role="alert"`<br>
[Validator.nu][validator_nu], [W3C Markup Validation Service][w3c_validator]

[modernizr]: https://modernizr.com/
[picturefill]: http://scottjehl.github.io/picturefill/
[caniuse]: http://caniuse.com/#feat=input-color
[html5_accessibility]: http://html5accessibility.com/
[aria]: https://www.w3.org/TR/wai-aria/
[validator_nu]: https://validator.nu/
[w3c_validator]: https://validator.w3.org/

## 도움이 되는 자료

[MDN HTML5][mdn_html5]<br>
[Bruce Lawson on HTML5][bruce_lawson]<br>
[HTML5 Doctor][html5doctor]<br>
[HTML 5 Demos and Examples][html5_demos]<br>
[Dive Into HTML5][dive_into_html5] ("HTML5: Up and Running", O'Reilly)<br>
[A Compendium of SVG Information][mega_list_svg_information]

[mdn_html5]: https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/HTML5
[bruce_lawson]: http://brucelawson.co.uk/category/html5/
[html5doctor]: http://html5doctor.com/
[html5_demos]: http://html5demos.com/
[dive_into_html5]: http://diveintohtml5.org/
[mega_list_svg_information]: https://css-tricks.com/mega-list-svg-information/

