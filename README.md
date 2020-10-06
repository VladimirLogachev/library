# Library

![app-preview]

GraphAPI and frontend for Vladimir Logachev's offline library.

Made with [Haskell] and [Elm], using [mu-haskell], [postgres-typed] and [elm-graphql].

Key feature: compile-time check against PostgreSQL db schema and GraphQL schema on both backend and frontend.

## How to use

1. run postgres via `docker-compose up -d`
1. `cd api`
   1. dev build: `stack build --exec api --file-watch`
   1. prod build: `stack build --ghc-options -O2 --copy-bins && ~/.local/bin/api`
1. `cd webapp`
   1. generate library for querying gql: `npm run codegen`
   1. dev build: `npm start`

### Debug:

1. run query:

```sh
# Books
curl \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{ "query": "{ books { author { name }, title, coverImageUrl } }" }' \
  http://localhost:8080 | jq

# Books by Author
curl \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{ "query": "{ authors { books { title, coverImageUrl }, name } }" }' \
  http://localhost:8080 | jq

# Create author
curl \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "query": "mutation($author: AuthorInput!) {
      createAuthor(author: $author)
    }",
    "variables": {
      "author": {
        "name": "Greg"
      }
    }
  }' \
  http://localhost:8080 | jq

```

[app-preview]: docs/app-preview.png
[haskell]: https://www.haskell.org
[elm]: https://elm-lang.org
[mu-haskell]: https://github.com/higherkindness/mu-haskell
[postgres-typed]: https://github.com/dylex/postgresql-typed
[elm-graphql]: https://github.com/dillonkearns/elm-graphql
