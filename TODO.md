- Add new design
   - books by author
   - better book states
   - better categories
   - rating/review
   - subscribe/ask ("I want this" button)
   - admin panel
   - pagination
   - make cover art optional (also needs design)

- add all new features from design to schema, back and front, one by one
- add all the books

- pass graphqlUrl as a flag to the webapp
- pass graphqlUrl as a flag to the API
- set up unit tests
- containerize all the things
- set up Circle CI
- set up deployment to DigitalOcean
- set up unit tests on CI
- add authentication tokens (probably, GitHub auth for admin room)

- make sure there is no need in deleting `.stack-work` after schema changes. Make it work so that compiler does not rely on cached output and reads the schema each time.
- enforce usage of the `Int32` on Graphql schema types. No need in Integer
- consider using bigserial. At least, try.
- return meaningful error on the backend (instead of "Something went wrong")
- use distinct types for ids, not just Integer
- Add Unit type as a custom scalar (instead of meaningless booleans in return of mutations)
- question: should "create" mutation return id of created item? why and why not?
