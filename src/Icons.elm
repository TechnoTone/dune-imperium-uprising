module Icons exposing (..)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)


letterIcon : String -> Html msg
letterIcon letter =
    div
        [ class "letter-icon" ]
        [ img [ src "letter-icon.png" ] []
        , div [ class "label" ] [ text letter ]
        ]


persuasionIcon : Int -> Html msg
persuasionIcon persuasion =
    div
        [ class "persuasion-icon" ]
        [ img [ src "persuasion-icon.png" ] []
        , div [ class "label" ] [ text (String.fromInt persuasion) ]
        ]
