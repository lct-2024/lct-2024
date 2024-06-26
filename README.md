# HR Zero – проект для хакатона Лидеры Цифровой Трансформации 2024

### Что успели сделать за 300 часов!

- 🎉 Бэкенд и фронт сервиса для проведения собеседований
- ✅ 3 микросервиса:
  - для авторизации пользователей по ролям
  - основной бизнес-логикой проведения собеседований
  - чаты для общения с соискателями
- 🌏 Личный кабинет соискателя
- 📕 Скрейпер для сбора вакансий и данных о кандидатах
- 🧨 AI для анализа соответствия резюме вакансиям


## Ссылки

* Сайт: https://hrzero.ru/
* Репозиторий: https://github.com/lct-2024/lct-2024

### Директории

- `/frontend/` - код фронтенда.
- `/backend/` - код бэкенда.

## Учётные записи для тестирования

**Внимание!** Для удобства проверки демо-стенда, проверка паролей была отключена и для логина можно вводить любой пароль.

- HR: hr@example.com
- Нанимающий менеджер: manager@example.com
- Кандидат 1: sasha@example.com
- Кандидат 2: mary@example.com

Ниже, в разделе `API` приведены токены авторизации для каждого из этих пользователей, позволяющие вызывать методы API в интерактивной песочнице.


## Стек технологий

* Бэкенд на Common Lisp.
* Межсервисное взаимодействие: OpenRPC
* На фронтенде: React + Redux
* База данных - PostgreSQL в облаке.
* Развёртывание всех компонент в Docker.

## Архитектура проекта

Для бэкенда мы реализовали микросервисную архитектуру из 3 сервисов:

- passport - хранит информацию о пользователях и их правах.
- chat - позволяет общаться соискателям и сотрудникам компании.
- ats - хранит все данные и логику работы с соискателями, воронки найма и тд.

Бэкенды общаются между собой и фронтендом с использованием JSON-RPC.

## API

API документировано в формате OpenRPC (аналог OpenAPI и Swagger) и может быть
просмотрено в интерфейсе https://playground.open-rpc.org/. Для этого достаточно ввести
на сайте один из следующих эндпоинтов:

- https://passport.hrzero.ru/
- https://chat.hrzero.ru/
- https://ats.hrzero.ru/

Чтобы в Playground выполнять запросы от определённого пользователя, нужно в левой нижней части экрана задать авторизационный токен
таким образом:

```
{
    "headers": {
        "Authorization": "..."
    }
}
```

Вот токены пользователей с разными ролями:

- HR (hr@example.com): `eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyLWlkIjoyNywiZmlvIjoi0JLQsNGB0LjQu9C40Lkg0J_Rg9C_0LrQuNC9Iiwicm9sZXMiOlsiaHIiXSwic2NvcGVzIjpbImF0cy5qb2IuY3JlYXRlIiwiYXRzLnByb2plY3QuY3JlYXRlIiwiYXRzLm5ld3MtcG9zdC5jcmVhdGUiLCJhdHMuam9iLmVkaXQiLCJhdHMucHJvamVjdC5lZGl0IiwiYXRzLm5ld3MtcG9zdC5lZGl0IiwiYXRzLmpvYi5kZWxldGUiLCJhdHMucHJvamVjdC5kZWxldGUiLCJhdHMubmV3cy1wb3N0LmRlbGV0ZSJdLCJpYXQiOjE3MTg1MzgyMDJ9._oKpWJ2M1n2ET_RO9fUoiA4Vpygvi12aw8nkiPUO1lA`
- Нанимающий менеджер (manager@example.com): `eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyLWlkIjoyOCwiZmlvIjoi0JjQstCw0L0g0KHQuNC00L7RgNC-0LIiLCJyb2xlcyI6WyJtYW5hZ2VyIl0sInNjb3BlcyI6WyJhdHMucHJvamVjdC5jcmVhdGUiLCJhdHMubmV3cy1wb3N0LmNyZWF0ZSJdLCJpYXQiOjE3MTg1MzgyNTJ9.W-c_q80NXYPwdqoNTiDzWoxFZErVaerFLcvD2fvjEE8`
- Кандидат 1 (sasha@example.com): `eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyLWlkIjozMSwiZmlvIjoi0JDQu9C10LrRgdCw0L3QtNGAINCS0LDRgdC40LvRjNC60L7QsiIsInJvbGVzIjpudWxsLCJzY29wZXMiOm51bGwsImlhdCI6MTcxODUzODI5Nn0.bLcvFznPM2Z-zt50NhsmVFW06ekDX47DGihAPBt7UMk`
- Кандидат 2 (mary@example.com): `eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyLWlkIjozMiwiZmlvIjoi0JzQsNGA0LjRjyDQkNCz0LDRhNC-0L3QvtCy0LAiLCJyb2xlcyI6bnVsbCwic2NvcGVzIjpudWxsLCJpYXQiOjE3MTg1MzgzMzB9.3qNCWvE_cYi3dWPeHFhhGFHahVF-gja-nU5bgrox2Go`

Замените многоточие из примера на один из токенов.

Документация по API автоматически собирается из docstring в коде и складывается в файлы
`openrpc-spec.json` внутри папки каждого из микросервисов. Затем по этим файлам
автоматически генерируется клиент для того, чтобы можно было удобно обращаться из одного
микросервиса в другой.

### Вот так выглядит архитектура сервиса


![](images/lct24-arch.jpg)


### Applicant Tracking System

Основная бизнес-логика тут. Этот микросервис хранит данные о вакансиях, кандидатах,
этапах собеседований, которые прошёл каждый из кандидатов и прочее.

Вот структура базы данных этого микросервиса (описание схемы `backend/ats/schema.sql`):

![](images/ats.png)

Так же, этот микросервис содержит дополнительные алгоритмы для парсинга резюме из файлов загруженных пользователями, и для матчинга резюме и вакансий.

А ещё, здесь же в папке `backend/ats/src/spiders/` есть код скрейперов, с помощью которых производился сбор дополнительных данных о вакансиях и кандидатах.

### Passport (Информация о пользователях, ролях и правах)

Этот микросервис хранить данные о пользователях. Email используется как идентификатор.
В базе хранятся хэши от паролей. Для аутентификации запросов между фронтендом и микросервисами мы используем JWT токены,
в которых зашифрованы основные данные о пользователе и его правах.

С каждым пользователем системы может быть ассоциирована одна или несколько ролей и каждая из ролей может быть ассоциирована с несколькими правами (scopes). Эта система позволяет гибко распределять полномочия между сотрудниками компании.

Мы используем две роли, дающие расширенные полномочия: hr и manager

Вот так выглядит структура базы этого микросервиса:

![](images/passport.png)

### Chat

Чаты для общения с кандидатами и между сотрудниками компании. Так же мы используем отдельные чаты для доставки до пользователя таких системных сообщений, как уведомления о переходе на новый этап собеседований, предложений согласовать время собеседования и тд.. В дальнейшем, для уведомлений можно будет подключать новые каналы оповещения, такие как email, telegram, Whatsup.

Структура базы чатов:

![](images/chat.png)

Технически, чаты устроены так, что доступ к ним может быть ограничен для определённой группы сотрудников компании.

## Принципы обработки вакансий

### Парсинг вакансий из резюме

Процесс обработки загруженных файлов очень простой:

- загруженный файл преобразуется из формата Word или PDF в обычный текст с помощью программы pandoc
- затем мы испольщуем специальный prompt к YandexGPT, чтобы по этому тексту сформировать JSON с полями в которые извлечены контактные данные, биография, навыки и данные об опыте и образовании соискателя.

Промпт к нейросети можно посмотреть в файле `backend/ats/src/ai/prompts/cv.txt`

### Подписка на новые вакансии

Для подписки мы сохраняем в базе фильтр, заданный пользователем: категорию, город, специальность, проект.

Далее, при появлении новой вакансии, проверяем, соответствует ли она каким-либо подпискам и оправляем соответствующим
соискателям уведомления. Сейчас этот процесс запускается по событию прямо в backend микросервиса ats, но в будущем,
если потребуется обрабатывать тысячи соискателей и сотни новых вакансий в день, стоит перевести систему на очередь
с отдельными обработчиками.

## Матрица трассировки требований

Для тестирования мы описали 10 сценариев и составили такую матрицу трассировки требований:

![](images/matrix.png)

[Ссылка на Excel документ](https://docs.google.com/spreadsheets/d/1xUVcL1cUuk8DvOQjpiERGPLdxric5y3epYXcq5PUuJM/edit?gid=1519841586#gid=1519841586) в Google Docs.


## Как собрать PDF с документацией

```
sudo apt-get install pandoc wkhtmltopdf

pandoc README.md \
       -t html \
       --pdf-engine-opt=--enable-local-file-access \
       --title 'ЛЦТ24 "Мы из Будущего"' \
       -o README.pdf
```


