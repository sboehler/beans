alter database dev set timezone to 'Europe/Zurich';

create table users
(
    id              bigserial primary key,
    email           varchar     not null,
    hashed_password varchar     not null,
    created_at      timestamptz not null default now()
);

create unique index on users (email);
