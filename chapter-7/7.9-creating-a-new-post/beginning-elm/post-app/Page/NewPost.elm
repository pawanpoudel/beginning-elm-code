module Page.NewPost exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Post exposing (Post, PostId, emptyPost, newPostEncoder, postDecoder)
import Route


type alias Model =
    { navKey : Nav.Key
    , post : Post
    , createError : Maybe String
    }


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , post = emptyPost
    , createError = Nothing
    }


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Create New Post" ]
        , newPostForm
        , viewError model.createError
        ]


newPostForm : Html Msg
newPostForm =
    Html.form []
        [ div []
            [ text "Title"
            , br [] []
            , input [ type_ "text", onInput StoreTitle ] []
            ]
        , br [] []
        , div []
            [ text "Author Name"
            , br [] []
            , input [ type_ "text", onInput StoreAuthorName ] []
            ]
        , br [] []
        , div []
            [ text "Author URL"
            , br [] []
            , input [ type_ "text", onInput StoreAuthorUrl ] []
            ]
        , br [] []
        , div []
            [ button [ type_ "button", onClick CreatePost ]
                [ text "Submit" ]
            ]
        ]


type Msg
    = StoreTitle String
    | StoreAuthorName String
    | StoreAuthorUrl String
    | CreatePost
    | PostCreated (Result Http.Error Post)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreTitle title ->
            let
                oldPost =
                    model.post

                updateTitle =
                    { oldPost | title = title }
            in
            ( { model | post = updateTitle }, Cmd.none )

        StoreAuthorName name ->
            let
                oldPost =
                    model.post

                updateAuthorName =
                    { oldPost | authorName = name }
            in
            ( { model | post = updateAuthorName }, Cmd.none )

        StoreAuthorUrl url ->
            let
                oldPost =
                    model.post

                updateAuthorUrl =
                    { oldPost | authorUrl = url }
            in
            ( { model | post = updateAuthorUrl }, Cmd.none )

        CreatePost ->
            ( model, createPost model.post )

        PostCreated (Ok post) ->
            ( { model | post = post, createError = Nothing }
            , Route.pushUrl Route.Posts model.navKey
            )

        PostCreated (Err error) ->
            ( { model | createError = Just (buildErrorMessage error) }
            , Cmd.none
            )


createPost : Post -> Cmd Msg
createPost post =
    Http.post
        { url = "http://localhost:5019/posts"
        , body = Http.jsonBody (newPostEncoder post)
        , expect = Http.expectJson PostCreated postDecoder
        }


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't create a post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
