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

create unique index unique_role_name_idx on passport.role(name);


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
    description TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

create unique index unique_scope_name_idx on passport.scope(name);


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


insert into passport.role (name) values
('admin'),
('manager'),
('hr')
on conflict do nothing;

insert into passport.scope (name, description, created_at, updated_at) values
('passport.user.edit', 'Позволяет редактировать профиль любого пользователя.', now(), now()),
('passport.user.ban', 'Позволяет банить любого пользователя.', now(), now()),
('ats.job.create', 'Позволяет создавать новые вакансии.', now(), now()),
('ats.job.edit', 'Позволяет изменять вакансии.', now(), now()),
('ats.job.delete', 'Позволяет удалять вакансии.', now(), now()),
('ats.project.create', 'Позволяет создавать новые проекты.', now(), now()),
('ats.project.edit', 'Позволяет редактировать проекты.', now(), now()),
('ats.project.delete', 'Позволяет удалять проекты.', now(), now()),
('ats.news-post.create', 'Позволяет добавлять новые новости', now(), now()),
('ats.news-post.edit', 'Позволяет редактировать новости.', now(), now()),
('ats.news-post.delete', 'Позволяет удалять новости.', now(), now())
on conflict (name)
do update set description = excluded.description, updated_at=excluded.updated_at;


INSERT INTO passport.role_scope (role_id, scope_id, created_at, updated_at)
SELECT role.id, scope.id, NOW(), NOW()
FROM passport.role, passport.scope
WHERE role.name = 'hr' and scope.name in (
    'ats.job.create',
    'ats.job.edit',
    'ats.job.delete',
    'ats.project.create',
    'ats.project.edit',
    'ats.project.delete',
    'ats.news-post.create',
    'ats.news-post.edit',
    'ats.news-post.delete'
)
on conflict do nothing;


INSERT INTO passport.role_scope (role_id, scope_id, created_at, updated_at)
SELECT role.id, scope.id, NOW(), NOW()
FROM passport.role, passport.scope
WHERE role.name = 'manager' and scope.name in (
    'ats.project.create',
    'ats.news-post.create'
)
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
