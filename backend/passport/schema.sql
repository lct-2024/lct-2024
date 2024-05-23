CREATE SCHEMA IF NOT EXISTS passport;

// TODO: Для реального прода тут нужны будут какие-то индексы
//       но в рамках хакатона нет времени на то, чтобы оптимизировать
//       запросы к базе и на тестовых данных и так всё работает
//       достаточно быстро.

CREATE TABLE passport.user (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    fio TEXT COLLATE "ru_RU",
    email TEXT,
    password_hash TEXT,
    avatar_url TEXT DEFAULT 'http://www.gravatar.com/avatar/501a6ae10e3fc3956ad1052cfc6d38d9?s=200',
    admin BOOLEAN DEFAULT False,
    position text,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX passport_user_email ON passport.user (email);


---------------
-- Migrations:
