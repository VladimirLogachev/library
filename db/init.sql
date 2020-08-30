CREATE TABLE IF NOT EXISTS books (
    id serial NOT NULL PRIMARY KEY,
    "title" VARCHAR NOT NULL
);

INSERT INTO
    books (title)
VALUES
    ('The Book 1'),
    ('The Book 2'),
    ('The Book 3');