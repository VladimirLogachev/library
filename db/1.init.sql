CREATE TABLE IF NOT EXISTS authors (
    id serial NOT NULL PRIMARY KEY,
    "name" VARCHAR NOT NULL
);

CREATE TABLE IF NOT EXISTS books (
    id serial NOT NULL PRIMARY KEY,
    "title" VARCHAR NOT NULL,
    "cover_image_filename" VARCHAR NOT NULL,
    "author_id" INTEGER REFERENCES authors (id) NOT NULL
);