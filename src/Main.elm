module Main exposing (main)

import Browser
import Html.Styled exposing (Attribute, Html, button, div, h1, h3, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (class, classList)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData, WebData)
import Task



---- TYPES ----


type alias Post =
    { id : Int
    , title : String
    , body : String
    }


type alias Model =
    { posts : WebData (List Post)
    }


type Msg
    = REQUEST_POSTS
    | POSTS_RECEIVED (WebData (List Post))



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "nav" ]
            [ div [ class "nav-wrap" ]

                [ h3 [ class "nav-title" ] [ text "Elm Assignment" ]
                , button [ onClick REQUEST_POSTS, class "btn" ]
                    [ text "Re-Load" ]
                ]
            ]
        , viewPostsOrError model
        ]



-- h3 [] [ text "Posts" ]
--         ,


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            div [ class "loader" ] [  ]

        RemoteData.Success posts ->
            viewPosts posts

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div [ class "error"]
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ div []
            (List.map viewPost posts)
        ]


viewPost : Post -> Html Msg
viewPost post =
    div [ class "item" ]
        [ div [ class "id" ]
            [ text (String.fromInt post.id) ]
        , div []
            [ h3 [ class "title" ] [ text post.title ] ]
        , div []
            [ text post.body ]
        ]



---- UPDATE ----


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" int
        |> required "title" string
        |> required "body" string


getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect =
            list postDecoder
                |> Http.expectJson (RemoteData.fromResult >> POSTS_RECEIVED)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        REQUEST_POSTS ->
            ( { model | posts = RemoteData.Loading }, getPosts )

        POSTS_RECEIVED data ->
            ( { model | posts = data }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



---- MODEL ----


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RemoteData.Loading }, getPosts )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
