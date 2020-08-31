CREATE TABLE IF NOT EXISTS authors (
    id serial NOT NULL PRIMARY KEY,
    "name" VARCHAR NOT NULL
);

CREATE TABLE IF NOT EXISTS books (
    id serial NOT NULL PRIMARY KEY,
    "title" VARCHAR NOT NULL,
    "author_id" INTEGER REFERENCES authors (id) NOT NULL
);

INSERT INTO
    authors (NAME)
VALUES
    ('Айн Рэнд'),
    ('Алекс Бэнкс, Ева Порселло'),
    ('Евгений Моргунов');

INSERT INTO
    books (title, author_id)
VALUES
    ('Ночью 16 января; Идеал; Подумай дважды', 1),
    ('Источник', 1),
    ('Атлант расправил плечи', 1),
    ('Гимн', 1),
    (
        'GraphQL: язык запросов для современных веб-приложений',
        2
    ),
    ('PostgreSQL. Основы языка SQL', 3);