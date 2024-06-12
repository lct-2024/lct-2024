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
    banned BOOLEAN Default False,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX passport_user_email ON passport.user (email);


CREATE TABLE passport.role (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);


CREATE TABLE passport.user_role (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    user_id BIGINT NOT NULL references passport.user on delete cascade,
    role_id BIGINT NOT NULL references passport.role on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index unique_user_role_idx on passport.user_role(user_id, role_id);

CREATE TABLE passport.scope (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    name TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE passport.role_scope (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    role_id BIGINT NOT NULL references passport.role on delete cascade,
    scope_id BIGINT NOT NULL references passport.scope on delete cascade,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index unique_role_scope_idx on passport.role_scope(role_id, scope_id);

CREATE OR REPLACE FUNCTION add_role_scope()
RETURNS TRIGGER AS $$
BEGIN
    INSERT INTO passport.role_scope (role_id, scope_id, created_at, updated_at)
    SELECT r.id, NEW.id, NOW(), NOW()
    FROM passport.role r
    WHERE r.name = 'admin';
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_add_role_scope
AFTER INSERT ON passport.scope
FOR EACH ROW
EXECUTE FUNCTION add_role_scope();


insert into passport.role (name) values ('admin');
insert into passport.role (name) values ('manager');
insert into passport.role (name) values ('hr');

insert into passport.scope (name) values ('passport.user.edit');
insert into passport.scope (name) values ('passport.user.ban');

insert into passport.scope (name) values ('ats.job.create');
insert into passport.scope (name) values ('ats.project.create');


INSERT INTO passport.role_scope (role_id, scope_id, created_at, updated_at)
SELECT role.id, scope.id, NOW(), NOW()
FROM passport.role, passport.scope
WHERE role.name = 'hr' and scope.name in ('ats.job.create', 'ats.project.create')
on conflict do nothing;


INSERT INTO passport.role_scope (role_id, scope_id, created_at, updated_at)
SELECT role.id, scope.id, NOW(), NOW()
FROM passport.role, passport.scope
WHERE role.name = 'manager' and scope.name in ('ats.project.create')
on conflict do nothing;


---------------
-- Migrations:

alter table passport.user add column metadata jsonb default '{}'::jsonb;
alter table passport.user drop column position;
alter table passport.user drop column admin;

-- даю себе админские права
insert into passport.user_role (user_id, role_id, created_at, updated_at)
select u.id, r.id, now(), now()
from passport.user as u
cross join passport.role as r
where u.email like '%@svetlyak.ru' and r.name = 'admin';


-- Наделяем полномочиями учётку HR
insert into passport.user_role (user_id, role_id, created_at, updated_at)
select u.id, r.id, now(), now()
from passport.user as u
cross join passport.role as r
where u.email like 'hr@example.com' and r.name = 'hr';

-- Наделяем полномочиями учётку Нанимающего Менеджера
insert into passport.user_role (user_id, role_id, created_at, updated_at)
select u.id, r.id, now(), now()
from passport.user as u
cross join passport.role as r
where u.email like 'manager@example.com' and r.name = 'manager';
