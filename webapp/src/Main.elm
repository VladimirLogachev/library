module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import DemoData
import Gql exposing (BookData)
import Graphql.Http
import Html exposing (Html, a, button, div, h1, img, p, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)
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


type alias Model =
    RemoteData (Graphql.Http.Error ()) (List BookData)


init : D.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( RemoteData.Loading
    , Gql.allBooks |> Task.attempt (RemoteData.fromResult >> GotResponse)
    )


type Msg
    = GotResponse Model
    | RouteChange Url
    | OnUrlRequest Browser.UrlRequest
    | DemoDataButtonClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( response, Cmd.none )

        DemoDataButtonClicked ->
            ( model, DemoData.loadDemoData |> Task.attempt (RemoteData.fromResult >> GotResponse) )

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
        [ style "margin-bottom" "50px"
        , style "margin-right" "50px"
        , style "width" "200px"
        , style "flex-grow" "0"
        , style "flex-shrink" "0"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "justify-content" "flex-end"
            , style "align-items" "flex-start"
            , style "height" "320px"
            , style "margin-bottom" "15px"
            ]
            [ img
                [ src bookData.coverImageUrl
                , style "border" "1px solid #D6DBDF"
                , style "border-radius" "4px"
                , style "max-width" "200px"
                ]
                []
            ]
        , p [ style "color" "#212121", style "font-size" "17px", style "margin-bottom" "14px" ]
            [ text bookData.title ]
        , p [ style "color" "#616161", style "font-size" "15px" ]
            [ text bookData.author.name ]
        ]


{-| Show allBooksQuery result
-}
showResult : Model -> Html Msg
showResult res =
    case res of
        Success books ->
            div
                [ style "padding" "15px"
                , style "font-family" "Merriweather, serif"
                ]
                [ h1
                    [ style "font-weight" "400"
                    , style "font-size" "40px"
                    , style "margin-bottom" "15px"
                    , style "margin-top" "25px"
                    , style "color" "#212121"
                    ]
                    [ text "Books" ]
                , div
                    [ style "display" "flex", style "flex-wrap" "wrap" ]
                    (List.map viewBook books)
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
            , button [ onClick DemoDataButtonClicked ] [ text "Add some books (demo data)" ]
            ]
    in
    { title = "Lib", body = children }
