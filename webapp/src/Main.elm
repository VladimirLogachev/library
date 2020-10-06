module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, div, h1, img, p, text)
import Html.Attributes exposing (src, style)
import Json.Decode as D
import LibraryApi.Object exposing (Author, Book)
import LibraryApi.Object.Author as Author
import LibraryApi.Object.Book as Book exposing (coverImageUrl)
import LibraryApi.Query as Query
import RemoteData exposing (RemoteData(..))
import Url exposing (Url)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( response, Cmd.none )

        _ ->
            ( model, Cmd.none )


type Msg
    = GotResponse Model
    | RouteChange Url
    | OnUrlRequest Browser.UrlRequest


type alias Model =
    RemoteData (Graphql.Http.Error (List BookData)) (List BookData)


type alias BookData =
    { title : String
    , coverImageUrl : String
    , author : String
    }


query : SelectionSet (List BookData) RootQuery
query =
    Query.books bookSelection


authorSelection : SelectionSet String Author
authorSelection =
    Author.name


bookSelection : SelectionSet BookData Book
bookSelection =
    SelectionSet.map3 BookData
        Book.title
        Book.coverImageUrl
        (Book.author authorSelection)


graphqlUrl : String
graphqlUrl =
    "http://localhost:8080"


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest
            graphqlUrl
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


init : D.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( RemoteData.Loading
    , makeRequest
    )


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


{-| Show query result
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
            showResult model
    in
    { title = "Lib", body = [ children ] }
