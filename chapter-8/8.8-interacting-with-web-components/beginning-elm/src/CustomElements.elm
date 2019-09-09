module CustomElements exposing (main)

import Asset
import Browser
import Html exposing (Attribute, Html, div, h2, strong, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (on)
import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (requiredAt)


type alias CropData =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


imageCrop : List (Attribute a) -> List (Html a) -> Html a
imageCrop =
    Html.node "image-crop"


onImageCropChange : Attribute Msg
onImageCropChange =
    cropDataDecoder
        |> Decode.map UpdateCropData
        |> on "image-crop-change"


cropDataDecoder : Decoder CropData
cropDataDecoder =
    Decode.succeed CropData
        |> requiredAt [ "detail", "x" ] int
        |> requiredAt [ "detail", "y" ] int
        |> requiredAt [ "detail", "width" ] int
        |> requiredAt [ "detail", "height" ] int


view : CropData -> Html Msg
view cropData =
    div [ class "center" ]
        [ h2 [] [ text "Image Crop" ]
        , imageCrop
            [ Asset.src Asset.waterfall
            , class "wrapper"
            , onImageCropChange
            ]
            []
        , viewCropData cropData
        ]


viewCropData : CropData -> Html Msg
viewCropData cropData =
    div []
        [ viewCropDataLabel "x" cropData.x
        , viewCropDataLabel "y" cropData.y
        , viewCropDataLabel "width" cropData.width
        , viewCropDataLabel "height" cropData.height
        ]


viewCropDataLabel : String -> Int -> Html Msg
viewCropDataLabel name value =
    div []
        [ strong [] [ text (name ++ " : ") ]
        , text (String.fromInt value)
        ]


type Msg
    = UpdateCropData CropData


update : Msg -> CropData -> ( CropData, Cmd Msg )
update msg oldCropData =
    case msg of
        UpdateCropData newCropData ->
            ( newCropData, Cmd.none )


initialModel : CropData
initialModel =
    { x = 297
    , y = 0
    , width = 906
    , height = 906
    }


init : () -> ( CropData, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


main : Program () CropData Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
