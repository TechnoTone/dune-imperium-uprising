module Icons exposing (..)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)


letterIcon : String -> Html msg
letterIcon letter =
    div
        [ class "group-icon" ]
        [ img [ src "letter-icon.png" ] []
        , div [ class "label" ] [ text letter ]
        ]


persuasionIcon : Int -> Html msg
persuasionIcon persuasion =
    div
        [ class "group-icon persuasion-icon" ]
        [ img [ src "persuasion-icon.png" ] []
        , div [ class "label" ] [ text (String.fromInt persuasion) ]
        ]


agentAccessIcon : String -> Html msg
agentAccessIcon imageSrc =
    div
        [ class "group-icon agent-access-icon" ]
        [ img [ src imageSrc ] []
        ]


factionIcon : String -> Html msg
factionIcon imageSrc =
    div
        [ class "group-icon faction-icon" ]
        [ img [ src imageSrc ] []
        ]


gradeIcon : String -> Html msg
gradeIcon grade =
    div
        [ class "group-icon grade-icon" ]
        [ img [ src "grade-icon.png" ] []
        , div [ class "label" ] [ text grade ]
        ]
