CREATE SCHEMA IF NOT EXISTS ats;

// TODO: Для реального прода тут нужны будут какие-то индексы
//       но в рамках хакатона нет времени на то, чтобы оптимизировать
//       запросы к базе и на тестовых данных и так всё работает
//       достаточно быстро.


CREATE TABLE ats.job (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.applicant (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.project (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

---------------
-- Migrations:

