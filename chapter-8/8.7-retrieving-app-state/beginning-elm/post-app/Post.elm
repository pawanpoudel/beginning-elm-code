module Post exposing
    ( Post
    , PostId
    , emptyPost
    , idParser
    , idToString
    , newPostEncoder
    , postDecoder
    , postEncoder
    , postsDecoder
    , savePosts
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Ports
import Url.Parser exposing (Parser, custom)


type alias Post =
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }


type PostId
    = PostId Int


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


idToString : PostId -> String
idToString (PostId id) =
    String.fromInt id


idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map PostId (String.toInt postId)


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


newPostEncoder : Post -> Encode.Value
newPostEncoder post =
    Encode.object
        [ ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId id) =
    Encode.int id


emptyPost : Post
emptyPost =
    { id = emptyPostId
    , title = ""
    , authorName = ""
    , authorUrl = ""
    }


emptyPostId : PostId
emptyPostId =
    PostId -1


savePosts : List Post -> Cmd msg
savePosts posts =
    Encode.list postEncoder posts
        |> Encode.encode 0
        |> Ports.storePosts
