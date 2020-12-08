# Library

GraphQL API and frontend for the next implementation of my offline library, implemented with [Haskell] and [Elm], using [mu-haskell], [postgres-typed] and [elm-graphql]. _(there is also [previous implementation] in Elm without backend)_

Key feature: compile-time typecheck against PostgreSQL and GraphQL schemas and (both backend and frontend).

- [TODO]
- [GraphQL queries] (curl)
- [URL structure]
- [UI design] (Figma, work in progress)

![app-preview]

## Setting up and running

1. `docker-compose up -d` — run PostgreSQL
1. `cd backend`
   - `stack build --exec backend --file-watch` — check schemas, run API server and watch for file changes
1. `cd frontend`
   - `npm i` — install dependencies
   - `npm start` — generate query library from schema, start dev-server, open [http://localhost:8000/](http://localhost:8000/):

## Notes
```sh
# initializing db
PGPassword=postgres \
  psql -f 0.recreate.sql -U postgres \
  && psql -f 1.init.sql -U postgres -d library \
  && psql -f 2.populate.sql -U postgres -d library
```
1. `stack build --ghc-options -O2 --copy-bins && ~/.local/bin/backend` — API production build (later use in dockerfile)

## Design choices and tradeoffs

1. Database does not enforce te fact that a book has at least one author. Instead, it relies on domain validation in resolvers.
1. Mu-haskell does not join sql queries, so I decided to exclude books from Authors.
   By making this tradeoff I can manually join sql queries.
1. GraphQL currently does not support input unions. But I want admin app to create book either with new author, or with existing one.
   What could I do:
   - Accept nullable fields in inputs: `newAuthor`, `existingAuthorId`, and then choose first filled (aka workaround input unions)
   - Decide on frontend and compose tasks that create author before book, if necessary (but it is not transactional which could be very confusing in case of any failure and retry)
   - Provide different mutations for such cases, and let frontend decide. This means enough typesafety, ability to make a proper SQL transaction and little work on frontend. This is my choice. 

[todo]: TODO.md
[graphql queries]: docs/queries.md
[url structure]: docs/url-structure.md
[UI design]: https://www.figma.com/file/g61ihnvBgnQtba3vfv8Rxo/Library
[previous implementation]: https://vladimirlogachev.github.io/#/en/library
[app-preview]: docs/app-preview.png
[haskell]: https://www.haskell.org
[elm]: https://elm-lang.org
[mu-haskell]: https://github.com/higherkindness/mu-haskell
[postgres-typed]: https://github.com/dylex/postgresql-typed
[elm-graphql]: https://github.com/dillonkearns/elm-graphql
