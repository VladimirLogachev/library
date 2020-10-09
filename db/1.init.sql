CREATE TABLE IF NOT EXISTS authors (
    id serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    UNIQUE ("name")
);

COMMENT ON TABLE authors IS 'Book authors';

COMMENT ON COLUMN authors.name IS 'Author name';

CREATE TABLE IF NOT EXISTS books (
    id serial NOT NULL PRIMARY KEY,
    "title" varchar NOT NULL,
    "cover_image_source_path" varchar NOT NULL,
    "author_id" integer REFERENCES authors (id) NOT NULL,
    UNIQUE ("title", "author_id")
);

COMMENT ON TABLE books IS 'Books';

COMMENT ON COLUMN books.title IS 'Book title';

COMMENT ON COLUMN books.author_id IS 'Book author';

COMMENT ON COLUMN books.cover_image_source_path IS 'Path to original book cover image file (relative to bucket and static directory)';

