# Library

GraphAPI for Vladimir Logachev's offline library.

Made with [mu-haskell] and [postgres-typed].

## How to use

1. run postgres via `docker-compose up -d`
1. `cd api`
1. dev build: `stack build --exec api --file-watch`
1. prod build: `stack build --ghc-options -O2 --copy-bins && ~/.local/bin/api`
1. run query:

```sh
curl \
  -X POST \
  -H "Content-Type: application/json" \
  --data '{ "query": "{ books { author { name, books { author { name }, title } } title } }" }' \
  http://localhost:8080 | jq
```

[mu-haskell]: https://github.com/higherkindness/mu-haskell
[postgres-typed]: https://github.com/dylex/postgresql-typed
