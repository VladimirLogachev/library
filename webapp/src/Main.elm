module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, h1, img, p, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import LibraryApi.InputObject exposing (AuthorInput, BookInput)
import LibraryApi.Mutation as Mutation exposing (CreateAuthorRequiredArguments, CreateBookRequiredArguments)
import LibraryApi.Object exposing (Author, Book)
import LibraryApi.Object.Author as Author
import LibraryApi.Object.Book as Book exposing (coverImageUrl)
import LibraryApi.Query as Query
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
import Url exposing (Url)


graphqlUrl : String
graphqlUrl =
    "http://localhost:8080"


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlChange = RouteChange
        , onUrlRequest = OnUrlRequest
        }


type alias Model =
    RemoteData (Graphql.Http.Error ()) (List BookData)


init : D.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( RemoteData.Loading
    , fetchAllBooksTask
        |> Task.mapError (Graphql.Http.mapError <| always ())
        |> Task.attempt (RemoteData.fromResult >> GotResponse)
    )


type alias BookData =
    { title : String
    , coverImageUrl : String
    , author : String
    }


authorSelection : SelectionSet String Author
authorSelection =
    Author.name


bookSelection : SelectionSet BookData Book
bookSelection =
    SelectionSet.map3 BookData
        Book.title
        Book.coverImageUrl
        (Book.author authorSelection)


allBooksQuery : SelectionSet (List BookData) RootQuery
allBooksQuery =
    Query.books bookSelection


fetchAllBooksTask : Task (Graphql.Http.Error (List BookData)) (List BookData)
fetchAllBooksTask =
    allBooksQuery
        |> Graphql.Http.queryRequest graphqlUrl
        |> Graphql.Http.toTask


createAuthorTask : AuthorInput -> Task (Graphql.Http.Error Bool) Bool
createAuthorTask =
    CreateAuthorRequiredArguments
        >> Mutation.createAuthor
        >> Graphql.Http.mutationRequest graphqlUrl
        >> Graphql.Http.toTask


createBookTask : BookInput -> Task (Graphql.Http.Error Bool) Bool
createBookTask =
    CreateBookRequiredArguments
        >> Mutation.createBook
        >> Graphql.Http.mutationRequest graphqlUrl
        >> Graphql.Http.toTask


{-| Helper function for development purposes
-}
createAuthorAndBooks : AuthorInput -> List BookInput -> Task (Graphql.Http.Error Bool) (List Bool)
createAuthorAndBooks author books =
    createAuthorTask author
        |> Task.andThen (\_ -> Task.sequence <| List.map createBookTask books)


{-| Helper function for development purposes
-}
applySeed : Cmd Msg
applySeed =
    Task.sequence
        [ createAuthorAndBooks
            (AuthorInput "Айн Рэнд")
            [ BookInput "Ночью 16 января; Идеал; Подумай дважды" "three_plays.jpg" 1
            , BookInput "Источник" "the_fountainhead.jpg" 1
            , BookInput "Атлант расправил плечи" "atlas_shrugged.jpg" 1
            , BookInput "Гимн" "anthem.jpg" 1
            ]
        , createAuthorAndBooks
            (AuthorInput "Алекс Бэнкс, Ева Порселло")
            [ BookInput "GraphQL: язык запросов для современных веб-приложений" "graphql.jpg" 2
            ]
        , createAuthorAndBooks
            (AuthorInput "Евгений Моргунов")
            [ BookInput "PostgreSQL. Основы языка SQL" "postgres.jpg" 3
            ]
        ]
        |> Task.mapError (Graphql.Http.mapError <| always ())
        |> Task.andThen (\_ -> Task.mapError (Graphql.Http.mapError <| always ()) fetchAllBooksTask)
        |> Task.attempt (RemoteData.fromResult >> GotResponse)


type Msg
    = GotResponse Model
    | GotSeedResponse
    | RouteChange Url
    | OnUrlRequest Browser.UrlRequest
    | SeedButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( response, Cmd.none )

        SeedButtonClicked ->
            ( model, applySeed )

        GotSeedResponse ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Show (potentially) nice error message
I don't care about possibly recovered data,
so keeping a Unit type there will fit my needs well.
-}
showError : Graphql.Http.Error () -> String
showError err =
    case err of
        Graphql.Http.HttpError Graphql.Http.NetworkError ->
            "network error"

        Graphql.Http.HttpError (Graphql.Http.BadUrl _) ->
            "BadUrl"

        Graphql.Http.HttpError Graphql.Http.Timeout ->
            "Timeout"

        Graphql.Http.HttpError (Graphql.Http.BadStatus _ _) ->
            "BadStatus"

        Graphql.Http.HttpError (Graphql.Http.BadPayload _) ->
            "BadStatus"

        Graphql.Http.GraphqlError _ graphqlErrors ->
            List.map (\e -> e.message) graphqlErrors |> String.concat


{-| Render a single book
-}
viewBook : BookData -> Html Msg
viewBook bookData =
    div
        [ style "margin-bottom" "15px"
        , style "padding" "15px"
        , style "width" "130px"
        , style "flex-grow" "0"
        , style "flex-shrink" "0"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "justify-content" "flex-end"
            , style "align-items" "flex-start"
            , style "height" "160px"
            , style "margin-bottom" "15px"
            ]
            [ img
                [ src bookData.coverImageUrl
                , style "border" "1px solid #D6DBDF"
                , style "border-radius" "4px"
                ]
                []
            ]
        , p
            [ style "color" "#2980B9"
            , style "margin-bottom" "5px"
            ]
            [ text bookData.author ]
        , p [] [ text bookData.title ]
        ]


{-| Show allBooksQuery result
-}
showResult : Model -> Html Msg
showResult res =
    case res of
        Success books ->
            div
                [ style "padding" "15px"
                , style "font-family" "sans-serif"
                ]
                [ h1
                    [ style "font-weight" "600"
                    , style "font-size" "32px"
                    , style "margin-bottom" "15px"
                    ]
                    [ text "Books" ]
                , div
                    [ style "display" "flex"
                    , style "flex-wrap" "wrap"
                    ]
                  <|
                    List.map viewBook books
                ]

        Failure err ->
            div [] [ text <| showError err ]

        Loading ->
            div [] [ text "Loading ..." ]

        NotAsked ->
            div [] []


{-| Render page and page title
-}
view : Model -> Browser.Document Msg
view model =
    let
        children =
            [ showResult model
            , button [ onClick SeedButtonClicked ] [ text "Seed" ]
            ]
    in
    { title = "Lib", body = children }
