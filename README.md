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

1. `stack build --ghc-options -O2 --copy-bins && ~/.local/bin/backend` — API production build (later use in dockerfile)

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
