module DemoData exposing (..)

import Gql exposing (BookData)
import Graphql.Http
import LibraryApi.InputObject exposing (AuthorInput, BookInput)
import Task exposing (Task)


createAuthorAndBooks : AuthorInput -> (Int -> List BookInput) -> Task (Graphql.Http.Error Int) (List Int)
createAuthorAndBooks author fillBooks =
    Gql.createAuthorTask author
        |> Task.andThen (\authorId -> Task.sequence <| List.map Gql.createBookTask (fillBooks authorId))


loadDemoData : Task (Graphql.Http.Error ()) (List Gql.BookData)
loadDemoData =
    Task.sequence
        [ createAuthorAndBooks
            (AuthorInput "Айн Рэнд")
            (\authorId ->
                [ BookInput "Источник" "the_fountainhead.jpg" authorId
                , BookInput "Атлант расправил плечи" "atlas_shrugged.jpg" authorId
                ]
            )
        , createAuthorAndBooks
            (AuthorInput "Алекс Бэнкс, Ева Порселло")
            (\authorId ->
                [ BookInput "GraphQL: язык запросов для современных веб-приложений" "graphql.jpg" authorId
                ]
            )
        , createAuthorAndBooks
            (AuthorInput "Евгений Моргунов")
            (\authorId ->
                [ BookInput "PostgreSQL. Основы языка SQL" "postgres.jpg" authorId
                ]
            )
        , createAuthorAndBooks
            (AuthorInput "Фредерик Лалу")
            (\authorId ->
                [ BookInput "Открывая организации будущего" "reinventing_organizations.webp" authorId
                ]
            )
        , createAuthorAndBooks
            (AuthorInput "Дэйв Логан, Джон Кинг, Хэли Фишер-Райт")
            (\authorId ->
                [ BookInput "Лидер и племя" "leader.webp" authorId
                ]
            )
        , createAuthorAndBooks
            (AuthorInput "Vitaly Bragilevsky")
            (\authorId ->
                [ BookInput "Haskell in Depth" "haskell_in_depth.webp" authorId
                ]
            )
        ]
        |> Task.mapError (Graphql.Http.mapError <| always ())
        |> Task.andThen (\_ -> Task.mapError (Graphql.Http.mapError <| always ()) Gql.allBooks)
