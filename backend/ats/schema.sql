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


CREATE TABLE ats.city (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index city_title_idx on ats.city(title collate "ru_RU");


CREATE TABLE ats.speciality (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index speciality_title_idx on ats.speciality(title collate "ru_RU");


CREATE TABLE ats.programming_language (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index programming_language_title_idx on ats.programming_language(title collate "ru_RU");


CREATE TABLE ats.skill (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index skill_title_idx on ats.skill(title collate "ru_RU");


CREATE TABLE ats.job_programming_language (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    job_id BIGINT NOT NULL references ats.job(id) on delete cascade,
    programming_language_id BIGINT not null references ats.programming_language on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.job_skill (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    job_id BIGINT NOT NULL references ats.job(id) on delete cascade,
    skill_id BIGINT not null references ats.skill on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE ats.applicant_skill (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    applicant_id BIGINT NOT NULL references ats.applicant(id) on delete cascade,
    skill_id BIGINT NOT NULL references ats.skill(id) on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.applicant_programming_language (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    applicant_id BIGINT NOT NULL references ats.applicant on delete cascade,
    programming_language_id BIGINT NOT NULL references ats.programming_language on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.education (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    applicant_id BIGINT NOT NULL references ats.applicant on delete cascade,
    title TEXT NOT NULL,
    speciality_id BIGINT references ats.speciality on delete set null,
    type TEXT NOT NULL,
    "from" DATE NOT NULL,
    "to" DATE,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.recommendation (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    applicant_id BIGINT NOT NULL references ats.applicant on delete cascade,
    fio TEXT NOT NULL default '',
    position TEXT NOT NULL default '',
    company TEXT NOT NULL default '',
    email TEXT NOT NULL default '',
    phone TEXT NOT NULL default '',
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE ats.theme (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.project_theme (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    project_id BIGINT NOT NULL references ats.project on delete cascade,
    theme_id BIGINT NOT NULL references ats.theme on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE ats.job_applicant (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    job_id BIGINT NOT NULL references ats.job on delete cascade,
    type TEXT NOT NULL,
    applicant_id BIGINT NOT NULL references ats.job on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index uniq_job_applicant_idx on ats.job_applicant (job_id, applicant_id);

---------------
-- Migrations:


alter table ats.job add column project_id BIGINT NOT NULL references ats.project(id) on delete cascade;
alter table ats.job add column speciality_id BIGINT NOT NULL references ats.speciality(id) on delete set null;
alter table ats.job add column type_of_employment TEXT NOT NULL default 'Полная';

alter table ats.job add column experience TEXT NOT NULL default '';
alter table ats.job add column about TEXT NOT NULL default '';
alter table ats.job add column contacts JSONB NOT NULL default '[]'::jsonb;

alter table ats.applicant add column experience TEXT NOT NULL default '';
alter table ats.applicant add column about TEXT NOT NULL default '';
alter table ats.applicant add column contacts JSONB NOT NULL default '[]'::jsonb;


alter table ats.job add column category TEXT NOT NULL default 'Другое';

alter table ats.job add column city TEXT NOT NULL default 'Москва';
alter table ats.job add column active BOOLEAN NOT NULL default True;
alter table ats.job add column active_to TIMESTAMPTZ default now() + '1 month'::interval;

alter table ats.job add column salary TEXT;

alter table ats.job add column chat_id TEXT;
alter table ats.applicant add column chat_id TEXT;

alter table ats.job add column required_experience TEXT;
