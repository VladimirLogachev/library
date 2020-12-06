CREATE TABLE IF NOT EXISTS author (
    id serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    CONSTRAINT unique_author_name UNIQUE ("name")
);

COMMENT ON TABLE author IS 'Book authors';

COMMENT ON COLUMN author.name IS 'Author name';

CREATE TABLE IF NOT EXISTS book (
    id serial NOT NULL PRIMARY KEY,
    "title" varchar NOT NULL,
    "cover_image_source_path" varchar NOT NULL,
    "author_id" integer REFERENCES author (id) NOT NULL,
    CONSTRAINT unique_book_title_per_author UNIQUE ("title", "author_id")
);

COMMENT ON TABLE book IS 'Books';

COMMENT ON COLUMN book.title IS 'Book title';

COMMENT ON COLUMN book.author_id IS 'Book author';

COMMENT ON COLUMN book.cover_image_source_path IS 'Path to original book cover image file (relative to bucket and static directory)';

