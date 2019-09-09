module Asset exposing (Image, src, waterfall)

import Html exposing (Attribute)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES


waterfall : Image
waterfall =
    image "waterfall.jpg"


image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)



-- USING IMAGES


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
