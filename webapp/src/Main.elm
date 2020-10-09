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
import LibraryApi.InputObject exposing (AuthorInput)
import LibraryApi.Mutation as Mutation exposing (CreateAuthorRequiredArguments)
import LibraryApi.Object exposing (Author, Book)
import LibraryApi.Object.Author as Author
import LibraryApi.Object.Book as Book exposing (coverImageUrl)
import LibraryApi.Query as Query
import RemoteData exposing (RemoteData(..))
import Task
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
    RemoteData (Graphql.Http.Error (List BookData)) (List BookData)


init : D.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( RemoteData.Loading
    , fetchAllBooks
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


fetchAllBooks : Cmd Msg
fetchAllBooks =
    allBooksQuery
        |> Graphql.Http.queryRequest graphqlUrl
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


createAuthorArgs : String -> CreateAuthorRequiredArguments
createAuthorArgs =
    CreateAuthorRequiredArguments << AuthorInput


seedMutation : String -> SelectionSet Bool RootMutation
seedMutation =
    Mutation.createAuthor << createAuthorArgs


registerAuthor : String -> Platform.Task (Graphql.Http.Error Bool) Bool
registerAuthor =
    seedMutation
        >> Graphql.Http.mutationRequest graphqlUrl
        >> Graphql.Http.toTask


registerAuthorAndBooks : Cmd Msg
registerAuthorAndBooks =
    -- TODO: parametrize with name and books
    registerAuthor "Айн Рэнд"
        -- |> get id
        -- |> write books as sequence
        |> Task.andThen (\_ -> registerAuthor "Алекс Бэнкс, Ева Порселло")
        |> Task.attempt (\_ -> GotSeedResponse)


applySeed : Cmd Msg
applySeed =
    -- ceclare, map, run as a sequence
    registerAuthorAndBooks



--     Cmd.batch
--         [ registerAuthor "Айн Рэнд" [...books]
--         , registerAuthor "Алекс Бэнкс, Ева Порселло" [...books]
--         , registerAuthor "Евгений Моргунов" [...books]
--         ]


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
-}
showError : Graphql.Http.Error (List BookData) -> String
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
