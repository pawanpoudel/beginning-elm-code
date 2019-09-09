module Page.ListPosts exposing (Model, Msg, init, update, view)

import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Post exposing (Post, PostId, postsDecoder, savePosts)
import RemoteData exposing (WebData)


type alias Model =
    { posts : WebData (List Post)
    , deleteError : Maybe String
    }


type Msg
    = FetchPosts
    | PostsReceived (WebData (List Post))
    | DeletePost PostId
    | PostDeleted (Result Http.Error String)


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchPosts )


initialModel : Model
initialModel =
    { posts = RemoteData.Loading
    , deleteError = Nothing
    }


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "http://localhost:5019/posts/"
        , expect =
            postsDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostsReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | posts = RemoteData.Loading }, fetchPosts )

        PostsReceived response ->
            let
                savePostsCmd =
                    case response of
                        RemoteData.Success actualPosts ->
                            savePosts actualPosts

                        _ ->
                            Cmd.none
            in
            ( { model | posts = response }, savePostsCmd )

        DeletePost postId ->
            ( model, deletePost postId )

        PostDeleted (Ok _) ->
            ( model, fetchPosts )

        PostDeleted (Err error) ->
            ( { model | deleteError = Just (buildErrorMessage error) }
            , Cmd.none
            )


deletePost : PostId -> Cmd Msg
deletePost postId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , body = Http.emptyBody
        , expect = Http.expectString PostDeleted
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchPosts ]
            [ text "Refresh posts" ]
        , br [] []
        , br [] []
        , a [ href "/posts/new" ]
            [ text "Create new post" ]
        , viewPosts model.posts
        , viewDeleteError model.deleteError
        ]


viewPosts : WebData (List Post) -> Html Msg
viewPosts posts =
    case posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualPosts ->
            div []
                [ h3 [] [ text "Posts" ]
                , table []
                    ([ viewTableHeader ] ++ List.map viewPost actualPosts)
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Author" ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    let
        postPath =
            "/posts/" ++ Post.idToString post.id
    in
    tr []
        [ td []
            [ text (Post.idToString post.id) ]
        , td []
            [ text post.title ]
        , td []
            [ a [ href post.authorUrl ] [ text post.authorName ] ]
        , td []
            [ a [ href postPath ] [ text "Edit" ] ]
        , td []
            [ button [ type_ "button", onClick (DeletePost post.id) ]
                [ text "Delete" ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch posts at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewDeleteError : Maybe String -> Html msg
viewDeleteError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't delete post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
