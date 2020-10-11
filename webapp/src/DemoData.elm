module DemoData exposing (..)

import Gql exposing (BookData)
import Graphql.Http
import LibraryApi.InputObject exposing (AuthorInput, BookInput)
import Task exposing (Task)


createAuthorAndBooks : AuthorInput -> List BookInput -> Task (Graphql.Http.Error Int) (List Int)
createAuthorAndBooks author books =
    Gql.createAuthorTask author
        |> Task.andThen (\_ -> Task.sequence <| List.map Gql.createBookTask books)


loadDemoData : Task (Graphql.Http.Error ()) (List Gql.BookData)
loadDemoData =
    Task.sequence
        [ createAuthorAndBooks
            (AuthorInput "Айн Рэнд")
            [ BookInput "Источник" "the_fountainhead_x2.jpg" 1
            , BookInput "Атлант расправил плечи" "atlas_shrugged_x2.jpg" 1
            ]
        , createAuthorAndBooks
            (AuthorInput "Алекс Бэнкс, Ева Порселло")
            [ BookInput "GraphQL: язык запросов для современных веб-приложений" "graphql_x2.jpg" 2
            ]
        , createAuthorAndBooks
            (AuthorInput "Евгений Моргунов")
            [ BookInput "PostgreSQL. Основы языка SQL" "postgres_x2.jpg" 3
            ]
        , createAuthorAndBooks
            (AuthorInput "Фредерик Лалу")
            [ BookInput "Открывая организации будущего" "reinventing_organizations_x2.webp" 4
            ]
        , createAuthorAndBooks
            (AuthorInput "Дэйв Логан, Джон Кинг, Хэли Фишер-Райт")
            [ BookInput "Лидер и племя" "leader_x2.webp" 5
            ]
        , createAuthorAndBooks
            (AuthorInput "Vitaly Bragilevsky")
            [ BookInput "Haskell in Depth" "haskell_in_depth_x2.webp" 6
            ]
        ]
        |> Task.mapError (Graphql.Http.mapError <| always ())
        |> Task.andThen (\_ -> Task.mapError (Graphql.Http.mapError <| always ()) Gql.allBooks)
