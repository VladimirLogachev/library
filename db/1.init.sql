CREATE TABLE IF NOT EXISTS author (
    "id" serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    CONSTRAINT unique_author_name UNIQUE ("name")
);
COMMENT ON TABLE author IS 'Book authors';
COMMENT ON COLUMN author.name IS 'Author name';
-- r_book_author

CREATE TABLE IF NOT EXISTS topic (
    "id" serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    CONSTRAINT unique_topic_name UNIQUE ("name")
);
COMMENT ON TABLE topic IS 'Book topics';
COMMENT ON COLUMN topic.name IS 'Topic display name';

CREATE TABLE IF NOT EXISTS rating (
    "id" serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    CONSTRAINT unique_rating_name UNIQUE ("name")
);
COMMENT ON TABLE rating IS 'Ratings';
COMMENT ON COLUMN rating.name IS 'Rating name';

CREATE TABLE IF NOT EXISTS book (
    "id" serial NOT NULL PRIMARY KEY,
    "title" varchar NOT NULL,
    "cover_image_source_path" varchar NOT NULL,
    "author_id" integer REFERENCES author (id) NOT NULL,
    CONSTRAINT unique_book_title_per_author UNIQUE ("title", "author_id")
);
COMMENT ON TABLE book IS 'Books';
COMMENT ON COLUMN book.title IS 'Book title';
COMMENT ON COLUMN book.author_id IS 'Book author';
COMMENT ON COLUMN book.cover_image_source_path IS 'Path to original book cover image file (relative to bucket and static directory)';

CREATE TABLE IF NOT EXISTS r_book_topic (
    "book_id" integer REFERENCES book (id) NOT NULL,
    "topic_id" integer REFERENCES topic (id) NOT NULL,
    CONSTRAINT topic_assigned_at_most_once UNIQUE ("book_id", "topic_id")
);
COMMENT ON TABLE r_book_topic IS 'Book ratings';

CREATE TABLE IF NOT EXISTS r_book_rating (
    "book_id" integer REFERENCES book (id) NOT NULL,
    "rating_id" integer REFERENCES rating (id) NOT NULL,
    CONSTRAINT unique_book_id UNIQUE ("book_id")
);
COMMENT ON TABLE r_book_rating IS 'Book ratings';




