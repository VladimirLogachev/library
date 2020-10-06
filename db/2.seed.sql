INSERT INTO authors (NAME)
    VALUES ('Айн Рэнд'), ('Алекс Бэнкс, Ева Порселло'), ('Евгений Моргунов');

INSERT INTO books (title, cover_image_filename, author_id)
SELECT
    'Ночью 16 января; Идеал; Подумай дважды',
    'three_plays.jpg',
    authors.id
FROM
    authors
WHERE
    authors.name = 'Айн Рэнд';

INSERT INTO books (title, cover_image_filename, author_id)
SELECT
    'Источник',
    'the_fountainhead.jpg',
    authors.id
FROM
    authors
WHERE
    authors.name = 'Айн Рэнд';

INSERT INTO books (title, cover_image_filename, author_id)
SELECT
    'Атлант расправил плечи',
    'atlas_shrugged.jpg',
    authors.id
FROM
    authors
WHERE
    authors.name = 'Айн Рэнд';

INSERT INTO books (title, cover_image_filename, author_id)
SELECT
    'Гимн',
    'anthem.jpg',
    authors.id
FROM
    authors
WHERE
    authors.name = 'Айн Рэнд';

INSERT INTO books (title, cover_image_filename, author_id)
SELECT
    'GraphQL: язык запросов для современных веб-приложений',
    'graphql.jpg',
    authors.id
FROM
    authors
WHERE
    authors.name = 'Алекс Бэнкс, Ева Порселло';

INSERT INTO books (title, cover_image_filename, author_id)
SELECT
    'PostgreSQL. Основы языка SQL',
    'postgres.jpg',
    authors.id
FROM
    authors
WHERE
    authors.name = 'Евгений Моргунов';

