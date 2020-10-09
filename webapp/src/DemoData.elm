module DemoData exposing (..)

import Gql exposing (BookData)
import Graphql.Http
import LibraryApi.InputObject exposing (AuthorInput, BookInput)
import Task exposing (Task)


createAuthorAndBooks : AuthorInput -> List BookInput -> Task (Graphql.Http.Error Bool) (List Bool)
createAuthorAndBooks author books =
    Gql.createAuthorTask author
        |> Task.andThen (\_ -> Task.sequence <| List.map Gql.createBookTask books)


loadDemoData : Task (Graphql.Http.Error ()) (List Gql.BookData)
loadDemoData =
    Task.sequence
        [ createAuthorAndBooks
            (AuthorInput "Айн Рэнд")
            [ BookInput "Ночью 16 января; Идеал; Подумай дважды" "three_plays_x2.jpg" 1
            , BookInput "Источник" "the_fountainhead_x2.jpg" 1
            , BookInput "Атлант расправил плечи" "atlas_shrugged_x2.jpg" 1
            , BookInput "Гимн" "anthem_x2.jpg" 1
            ]
        , createAuthorAndBooks
            (AuthorInput "Алекс Бэнкс, Ева Порселло")
            [ BookInput "GraphQL: язык запросов для современных веб-приложений" "graphql_x2.jpg" 2
            ]
        , createAuthorAndBooks
            (AuthorInput "Евгений Моргунов")
            [ BookInput "PostgreSQL. Основы языка SQL" "postgres_x2.jpg" 3
            ]
        ]
        |> Task.mapError (Graphql.Http.mapError <| always ())
        |> Task.andThen (\_ -> Task.mapError (Graphql.Http.mapError <| always ()) Gql.allBooks)
