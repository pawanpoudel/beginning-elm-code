port module PortExamples exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    String


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendDataToJS ]
            [ text "Send Data to JavaScript" ]
        ]


type Msg
    = SendDataToJS


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, sendData "Hello JavaScript!" )


port sendData : String -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
