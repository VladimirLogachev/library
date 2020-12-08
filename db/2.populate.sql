INSERT INTO author (name)
VALUES
  ('Author 1'),
  ('Author 2'),
  ('Author 3'),
  ('Author 4'),
  ('Author 5'),
  ('Author 6'),
  ('Author 7'),
  ('Author 8'),
  ('Author 9'),
  ('Author 10'),
  ('Author 11'),
  ('Author 12'),
  ('Author 13');

INSERT INTO book ( title, cover_image_source_path, author_id )
VALUES
  ('Book 1', 'connecting_dots.png', 1),
  ('Book 2', 'connecting_dots.png', 2),
  ('Book 3', 'connecting_dots.png', 3),
  ('Book 4', 'connecting_dots.png', 4),
  ('Book 5', 'connecting_dots.png', 5),
  ('Book 6', 'connecting_dots.png', 6),
  ('Book 7', 'connecting_dots.png', 7),
  ('Book 8', 'connecting_dots.png', 8);

INSERT INTO topic (name)
VALUES
  ('Topic 1'),
  ('Topic 2'),
  ('Topic 3'),
  ('Topic 4'),
  ('Topic 5'),
  ('Topic 6'),
  ('Topic 7'),
  ('Topic 8'),
  ('Topic 9'),
  ('Topic 10'),
  ('Topic 11'),
  ('Topic 12'),
  ('Topic 13');

INSERT INTO rating (name)
VALUES
  ('Excellent 1'),
  ('Good 2'),
  ('Neutral 3'),
  ('Bad 4');


INSERT INTO r_book_rating ( book_id, rating_id )
VALUES
  (1, 1),
  (2, 2),
  (3, 3),
  (4, 4),
  (5, 1),
  (6, 3),
  (7, 3),
  (8, 3);


INSERT INTO r_book_topic ( book_id, topic_id )
VALUES
  (1, 1),
  (2, 2),
  (2, 3),
  (2, 4),
  (2, 5),
  (2, 6),
  (3, 3),
  (4, 4),
  (5, 1),
  (6, 3),
  (7, 3),
  (8, 3);


