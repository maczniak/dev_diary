# [파이썬 웹 프로그래밍: Django(장고)로 배우는 쉽고 빠른 웹 개발][homepage], 김석훈, 한빛미디어 (2015)

[homepage]: http://www.hanbit.co.kr/store/books/look.php?p_code=B5790464800

## 1. 웹 프로그래밍의 이해

## 2. 파이썬 웹 표준 라이브러리

## 3. Django 웹 프레임워크

```
$ django-admin.py startproject mysite
$ python manage.py startapp polls
$ python manage.py migrate # after makemigrations, (pre-1.7) syncdb
                           # $ python manage.py sqlmigrate polls 0001
$ python manage.py runserver
$ python manage.py createsuperuser
```

```
<form action="{% url 'polls:vote' question.id %}" method="post">
{% csrf_token %} (Cross Site Request Forgery)
```

`return HttpResponseRedirect(reverse('polls:results', args=(p.id,)))`

## 4. Django의 핵심 기능

### Admin 사이트 꾸미기

### 장고 파이썬 쉘로 데이터 조작하기

```
$ python manage.py shell

>>> q.choice_set.create(choice_text='Sleeping', votes=0)
```

mysite/settings.py 모듈을 임포트하려고 manage.py의 `DJANGO_SETTINGS_MODULE`을 참고한다.<br>
[django.utils.timezone][django_utils_timezone]

[django_utils_timezone]: https://docs.djangoproject.com/en/1.10/topics/i18n/timezones/

### 템플릿 시스템

`.` 탐색 순서 - 사전 -> 속성 -> 리스트<br>
정의되지 않은 변수는 settings.py의 `TEMPLATE_STRING_IF_INVALID` (기본값 '')<br>
[기본 템플릿 태그와 필터][builtin_template_tags_and_filters]<br>
forloop 변수 속성 - .counter, .counter0, .revcounter, .revcounter0, .first, .last, .parentloop<br>
`{% with total=business.employees.count %}` (or older `{% with business.employees.count as total %}`) `{% endwith }`<br>
`{# ... #}`, `{% comment "Optional note, not nested" %}` ... `{% endcomment %}`<br>
`{{ data|safe }}`, `{% autoescape off %}` ... `{% endautoescape %}`, 필터의 인자는 자동 이스케이프를 적용하지 않는다<br>
`{{ block.super }}`

[builtin_template_tags_and_filters]: https://docs.djangoproject.com/en/1.10/ref/templates/builtins/

### 폼 처리하기

## 5. 실습 예제 확장하기

## 6. 웹 서버(Apache)와 연동

## A. 장고의 데이터베이스 연동

## B. HTTP 상태 코드 전체 요약

## C. 장고의 설계 원칙

