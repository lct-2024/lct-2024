CREATE SCHEMA IF NOT EXISTS chat;

CREATE TABLE chat.chat (
    id UUID NOT NULL PRIMARY KEY,
    private BOOLEAN NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE TABLE chat.chat_team (
    chat_id UUID NOT NULL,
    team_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ,
    PRIMARY KEY (chat_id, team_id)
);

CREATE TABLE chat.chat_member (
    chat_id UUID NOT NULL,
    user_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ,
    PRIMARY KEY (chat_id, user_id)
);

CREATE TABLE chat.message (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    chat_id UUID NOT NULL,
    user_id BIGINT NOT NULL,
    message TEXT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
);

CREATE INDEX chat_messages_idx ON chat.message (chat_id, id);


-- для автоматической генерации id чатов:
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

ALTER TABLE chat.chat ALTER COLUMN id SET DEFAULT uuid_generate_v4();


ALTER TABLE chat.chat ADD COLUMN title TEXT;

ALTER TABLE chat.chat ADD COLUMN archived BOOLEAN NOT NULL DEFAULT False;

ALTER TABLE chat.chat ADD COLUMN content_type TEXT;

ALTER TABLE chat.chat ADD COLUMN content_id TEXT;

CREATE UNIQUE INDEX unique_content_type_and_id_idx ON chat.chat (content_type, content_id) WHERE content_type is not NULL and content_id is not NULL;
