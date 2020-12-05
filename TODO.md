# TODO

Each division stands for 1 day of work

---
- join sql queries
- simplify stack file structure
- return entire book and entire author from mutations
- add meaningful errors on head usage
- document design choice on making Author a leaf (and generally prevent infinite-deep selections)
---
- implement routing for every item in URL structure
---
- use elm-ui for current implementation
- add color constants
- define breakpoints for media type (just come up with something and let it be)
---
- Design: States for navigation:
  - Book not found (reset? or recommendations?)
  - topic not found
  - can't load data
- implement slugs for adding topics and authors (either automatic or manual)
---
- set up elm-live 
- implement sitemap.xml generator on backend
--- 
- Design admin panel
- Desing full-page search
---
- use Nix instead of Docker and compose overrides.
---
- Add rating and topics to inputs (however, ignore them) (topicIds are ok rating is a separate relation)
- Add all the books to DemoData (requires: category, and mutation for adding it)
- Add book catalog component
- scroll to book details on book select

- Implement topics (schema, back, front)
- Implement book rating (schema, back, )

- Add book page
- Add topics to the book page
- Add rating to the book page
- Add contact mutation + backend
- Add contact button and modal

- add all other features from design to schema, back and front, one by one
- specify placeholder color for all processable images.
  colors may be derived from picture as is, or may be from list of predefined colors,
  closest to one from picture. Should be explicitly defined for all processed images.

## Infrastructure and other

- pass graphqlUrl as a flag to the frontend
- pass graphqlUrl as a flag to the backend
- set up unit tests
- containerize all the things
- set up Circle CI
- set up deployment to DigitalOcean
- set up unit tests on CI
- add authentication tokens (probably, GitHub auth for admin room)
- research on Nix
- research on Hashicorp Vault
- `parse error: Invalid numeric literal at line 1, column 10` when db is offline. It's 500, but should not contain server-specific details.
- implement reconnect and/or connection pool to db, because id does not reconnect.

- make sure there is no need in deleting `.stack-work` after schema changes.
  Make it work so that compiler does not rely on cached output and reads the schema each time.
- enforce usage of the `Int32` on Graphql schema types. No need in Integer
- consider using bigserial. At least, try.
- return meaningful error on the backend (instead of "Something went wrong")
- use distinct types for ids, not just Integer
- Add Unit type as a custom scalar (instead of meaningless booleans in return of mutations)
- question: should "create" mutation return id of created item? why and why not?
- implement reconnect on disconnect with pgReconnect and exponential backoff
  (fallback: connect and disconnect on each and every query.)
- `Also, nullable result fields resulting from outer joins are not detected and need to be handled explicitly.` in `postgres-typed`
- try to use union types with mu-haskell
- try to use transactions, check success and failure cases
- try to inspect query set in resolvers
- use joins for author's books
- switch to bigserial in sql
- authorId: ID! - leads to Data.UUID.Types.Internal.UUID,
  leads to problem either with deriving or with TH and schema. Not sure.
- add rollbar
- add metrics (prometheus, logzio)

- try to use records instead of tuples in resolvers
- add logging (figure out how to log)
- update to mu 0.4
- experiment with toast notifications
- experiment with lazy and keyed Html (use different sorting)
