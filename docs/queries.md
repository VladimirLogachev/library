# GraphQL queries

## Books
```sh
curl \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "query": "{
      books {
        id
        title,
        coverImageUrl
        author {
          name
        }
      }
    }" 
  }' \
  http://localhost:8080 | jq
```

## Books by author
```sh
curl \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "query": "{ 
      authors {
        books { 
          title, 
          coverImageUrl 
        }, 
        name 
      } 
    }" 
  }' \
  http://localhost:8080 | jq
```

## Create author
```sh
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

## Create book
```sh
curl \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "query": "mutation($book: BookInput!) {
      createBook(book: $book)
    }",
    "variables": {
      "book": {
        "title": "How to become rich while doing nothing",
        "coverImageSourcePath": "da nikak",
        "authorId": 1
      }
    }
  }' \
  http://localhost:8080 | jq
```