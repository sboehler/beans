CREATE TABLE users (
  id            BIGSERIAL PRIMARY KEY,
  email         VARCHAR NOT NULL,
  hashed_password VARCHAR NOT NULL
);

CREATE UNIQUE INDEX ON users (email);
