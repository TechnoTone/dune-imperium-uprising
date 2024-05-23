module Icons exposing (..)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)


letterIcon : String -> List (Html msg)
letterIcon letter =
    [ div
        [ class "group-icon" ]
        [ div [ class "icon" ]
            [ img [ src "letter-icon.png" ] []
            ]
        , div [ class "label" ] [ text letter ]
        ]
    ]


persuasionIcon : Int -> List (Html msg)
persuasionIcon persuasion =
    [ div
        [ class "group-icon persuasion-icon" ]
        [ div [ class "icon" ]
            [ img [ src "persuasion-icon.png" ] []
            ]
        , div [ class "label" ] [ text (String.fromInt persuasion) ]
        ]
    ]


agentAccessIcon : String -> List (Html msg)
agentAccessIcon imageSrc =
    [ div
        [ class "group-icon agent-access-icon" ]
        [ div [ class "icon" ]
            [ img [ src imageSrc ] []
            ]
        ]
    ]


factionIcon : String -> List (Html msg)
factionIcon imageSrc =
    [ div
        [ class "group-icon faction-icon" ]
        [ div [ class "icon" ]
            [ img [ src imageSrc ] []
            ]
        ]
    ]


gradeIcon : String -> List (Html msg)
gradeIcon grade =
    [ div
        [ class "group-icon grade-icon" ]
        [ div [ class "icon" ]
            [ img [ src "grade-icon.png" ] []
            ]
        , div [ class "label" ] [ text grade ]
        ]
    ]
