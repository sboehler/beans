CREATE TABLE users
(
  id              BIGSERIAL PRIMARY KEY,
  email           VARCHAR NOT NULL,
  hashed_password VARCHAR NOT NULL,
  created_at      timestamptz not null default now()
);

CREATE UNIQUE INDEX ON users (email);
