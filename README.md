# library

1. run postgres
1. check configuration in `src/Connect.hs`
1. dev: `stack build --exec api --file-watch`
1. prod: `stack build --ghc-options -O2 --copy-bins && ~/.local/bin/api`

```sh
curl \
  -X POST \
  -H "Content-Type: application/json" \
  --data '{ "query": "{ books { author { name, books { author { name }, title } } title } }" }' \
  http://localhost:8080 | jq
```
