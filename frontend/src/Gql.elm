module Gql exposing (..)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import LibraryApi.InputObject exposing (AuthorInput, BookInput)
import LibraryApi.Mutation as Mutation exposing (CreateAuthorRequiredArguments, CreateBookRequiredArguments)
import LibraryApi.Object exposing (Author, Book)
import LibraryApi.Object.Author as Author
import LibraryApi.Object.Book as Book exposing (coverImageUrl)
import LibraryApi.Query as Query
import Task exposing (Task)


graphqlUrl : String
graphqlUrl =
    "http://localhost:8080"


type alias BookData =
    { id : Int
    , title : String
    , coverImageUrl : String
    , author : AuthorData
    }


type alias AuthorData =
    { id : Int
    , name : String
    }


authorSelection : SelectionSet AuthorData Author
authorSelection =
    SelectionSet.map2 AuthorData
        Author.id
        Author.name


bookSelection : SelectionSet BookData Book
bookSelection =
    SelectionSet.map4 BookData
        Book.id
        Book.title
        Book.coverImageUrl
        (Book.author authorSelection)


allBooks : Task (Graphql.Http.Error ()) (List BookData)
allBooks =
    Query.books bookSelection
        |> Graphql.Http.queryRequest graphqlUrl
        |> Graphql.Http.toTask
        |> Task.mapError (Graphql.Http.mapError <| always ())


createAuthorTask : AuthorInput -> Task (Graphql.Http.Error Int) Int
createAuthorTask =
    CreateAuthorRequiredArguments
        >> Mutation.createAuthor
        >> Graphql.Http.mutationRequest graphqlUrl
        >> Graphql.Http.toTask


createBookTask : BookInput -> Task (Graphql.Http.Error Int) Int
createBookTask =
    CreateBookRequiredArguments
        >> Mutation.createBook
        >> Graphql.Http.mutationRequest graphqlUrl
        >> Graphql.Http.toTask
