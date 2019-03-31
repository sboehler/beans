CREATE TABLE users
(
  id              BIGSERIAL PRIMARY KEY,
  email           VARCHAR NOT NULL,
  hashed_password VARCHAR NOT NULL,
  created_at      timestamp WITH TIME ZONE DEFAULT now()
);

CREATE UNIQUE INDEX ON users (email);
