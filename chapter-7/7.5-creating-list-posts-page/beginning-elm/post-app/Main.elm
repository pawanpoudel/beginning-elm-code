module Main exposing (main)

import Browser
import Page.ListPosts as ListPosts


main : Program () ListPosts.Model ListPosts.Msg
main =
    Browser.element
        { init = ListPosts.init
        , view = ListPosts.view
        , update = ListPosts.update
        , subscriptions = \_ -> Sub.none
        }
