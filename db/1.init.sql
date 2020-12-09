CREATE TABLE IF NOT EXISTS authors (
    "author_id" serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    CONSTRAINT unique_author_name UNIQUE ("name")
);
COMMENT ON TABLE authors IS 'Book authors';
COMMENT ON COLUMN authors.name IS 'Author name';

CREATE TABLE IF NOT EXISTS topics (
    "topic_id" serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    CONSTRAINT unique_topic_name UNIQUE ("name")
);
COMMENT ON TABLE topics IS 'Book topics';
COMMENT ON COLUMN topics.name IS 'Topic display name';

CREATE TABLE IF NOT EXISTS ratings (
    "rating_id" serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    CONSTRAINT unique_rating_name UNIQUE ("name")
);
COMMENT ON TABLE ratings IS 'Ratings';
COMMENT ON COLUMN ratings.name IS 'Rating name';

CREATE TABLE IF NOT EXISTS books (
    "book_id" serial NOT NULL PRIMARY KEY,
    "title" varchar NOT NULL,
    "cover_image_source_path" varchar NOT NULL,
    "author_id" integer REFERENCES authors (author_id) NOT NULL,
    CONSTRAINT unique_book_title_per_author UNIQUE ("title", "author_id")
);
COMMENT ON TABLE books IS 'Books';
COMMENT ON COLUMN books.title IS 'Book title';
COMMENT ON COLUMN books.author_id IS 'Book author';
COMMENT ON COLUMN books.cover_image_source_path IS 'Path to original book cover image file (relative to bucket and static directory)';

CREATE TABLE IF NOT EXISTS persons (
    "person_id" serial NOT NULL PRIMARY KEY,
    "name" varchar NOT NULL,
    "contact_info" varchar NOT NULL,
    CONSTRAINT unique_person_name UNIQUE ("name")
);
COMMENT ON TABLE persons IS 'Persons (readers)';
COMMENT ON COLUMN persons.name IS 'Person name';
COMMENT ON COLUMN persons.contact_info IS 'Any contact info.';

CREATE TABLE IF NOT EXISTS r_book_topic (
    "book_id" integer REFERENCES books (book_id) NOT NULL,
    "topic_id" integer REFERENCES topics (topic_id) NOT NULL,
    CONSTRAINT topic_assigned_at_most_once UNIQUE ("book_id", "topic_id")
);
COMMENT ON TABLE r_book_topic IS 'Book ratings';

CREATE TABLE IF NOT EXISTS r_book_rating (
    "book_id" integer REFERENCES books (book_id) NOT NULL,
    "rating_id" integer REFERENCES ratings (rating_id) NOT NULL,
    CONSTRAINT unique_book_id UNIQUE ("book_id")
);
COMMENT ON TABLE r_book_rating IS 'Book ratings';

CREATE TABLE IF NOT EXISTS books_given (
    "book_id" integer REFERENCES books (book_id) NOT NULL,
    "person_id" integer REFERENCES persons (person_id) NOT NULL,
    CONSTRAINT book_given_at_most_once UNIQUE ("book_id")
);
COMMENT ON TABLE books_given IS 'This is like a warehouse state. It is not intended to deal with dates and events, but only to tell what is available and what is not.';



