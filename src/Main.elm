module Main exposing (Model, Msg(..), initModel, main, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Html exposing (Attribute, a, button, div, h1, h3, img, span, text)
import Html.Attributes exposing (class, classList, href, id, src, style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch exposing (Touch)
import Task


type alias Model =
    { screen : Screen
    , menu : MenuVisibility
    , viewportSize : ( Float, Float )
    , viewOrientation : Orientation
    , zoomPosition : ( Float, Float )
    , zoomState : ZoomState
    , touches : List Touch
    }


type Screen
    = Menu
    | Board
    | Leaders Leader
    | Cards
    | Manuals
    | CombatManual


type Leader
    = MaudDib
    | GurneyHalleck
    | FeydRauthaHarkonnen
    | LadyMargotFenring
    | LadyAmberMetulli
    | PrincessIrulan
    | LadyJessica
    | ReverendMotherJessica
    | ShaddamConringIV
    | StabanTuer


type Orientation
    = Portrait
    | Landscape


type MenuVisibility
    = Hidden
    | Visible


type ZoomState
    = ZoomedOut
    | ZoomedIn


type Msg
    = GotViewport Dom.Viewport
    | WindowResize Int Int
    | Show Screen
    | FullScreenMouseDown ( Float, Float )
    | FullScreenMouseUp ( Float, Float )
    | FullScreenMouseOver ( Float, Float )
    | Touch TouchEventType Touch.Event


type TouchEventType
    = TouchStart
    | TouchMove
    | TouchEnd


type Action
    = Left
    | Right
    | Up
    | Down
    | None


initModel : () -> ( Model, Cmd Msg )
initModel =
    always <|
        ( { screen = Menu
          , menu = Hidden
          , viewportSize = ( 0, 0 )
          , viewOrientation = Portrait
          , zoomPosition = ( 0, 0 )
          , zoomState = ZoomedOut
          , touches = []
          }
        , Task.perform GotViewport Dom.getViewport
        )



{-
   @@  @@ @@@@@@ @@@@@@ @@   @@
   @@  @@   @@   @@     @@   @@
   @@  @@   @@   @@@@   @@ @ @@
    @@@@    @@   @@     @@@@@@@
     @@   @@@@@@ @@@@@@  @@ @@
-}


nothing : Html.Html Msg
nothing =
    text ""


view : Model -> Browser.Document Msg
view model =
    { title = "Dune Imperium: Uprising"
    , body =
        [ div
            [ class "screen-container" ]
            [ homeButton model, viewScreen model ]
        ]
    }


homeButton : Model -> Html.Html Msg
homeButton model =
    if model.screen == Menu then
        nothing

    else
        div
            [ class "home-button", onClick <| Show Menu ]
            [ img [ src "home.png" ] [] ]


viewScreen : Model -> Html.Html Msg
viewScreen model =
    case model.screen of
        Menu ->
            viewMenu

        Board ->
            viewFullScreenImage "board.jpg" model

        Leaders leader ->
            viewLeaders leader

        Cards ->
            div [] [ text "Cards" ]

        Manuals ->
            viewManuals

        CombatManual ->
            viewFullScreenImage "combatreference.png" model


viewMenu : Html.Html Msg
viewMenu =
    tileList "Dune Imperium: Uprising"
        [ ( "Board", "menu-board.jpg", onClick <| Show Board )
        , ( "Leaders", "menu-leaders.png", onClick <| Show (Leaders MaudDib) )
        , ( "Cards", "menu-cards.jpg", onClick <| Show Cards )
        , ( "Manuals", "menu-manuals.png", onClick <| Show Manuals )
        ]


viewLeaders : Leader -> Html.Html Msg
viewLeaders currentLeader =
    let
        avatar : ( String, Leader ) -> Html.Html Msg
        avatar ( imageSource, leader ) =
            div
                [ class "avatar" ]
                [ a
                    [ onClick <| Show (Leaders leader) ]
                    [ img [ src imageSource ] [] ]
                ]
    in
    div [ class "leaders" ]
        [ div
            [ class "avatars" ]
            [ avatar ( "avatar-mauddib.png", MaudDib )
            , avatar ( "avatar-gurneyhalleck.png", GurneyHalleck )
            ]
        , div
            [ class "leader" ]
            [ img
                [ src
                    (case currentLeader of
                        MaudDib ->
                            "leader-mauddib.png"

                        GurneyHalleck ->
                            "leader-gurneyhalleck.png"

                        FeydRauthaHarkonnen ->
                            "leader-feydrauthaharkonnen.png"

                        LadyMargotFenring ->
                            "leader-ladymargotfenring.png"

                        LadyAmberMetulli ->
                            "leader-ladyambermetulli.png"

                        PrincessIrulan ->
                            "leader-princessirulan.png"

                        LadyJessica ->
                            "leader-ladyjessica.png"

                        ReverendMotherJessica ->
                            "leader-reverendmotherjessica.png"

                        ShaddamConringIV ->
                            "leader-shaddamconringiv.png"

                        StabanTuer ->
                            "leader-stabantuer.png"
                    )
                ]
                []
            ]
        ]


viewManuals : Html.Html Msg
viewManuals =
    tileList "Manuals"
        [ ( "Rulebook (pdf)", "manual-rulebook.png", href "rulebook.pdf" )
        , ( "Supplements (pdf)", "manual-supplements.png", href "supplements.pdf" )
        , ( "Combat", "manual-combat.png", onClick <| Show CombatManual )
        ]


viewFullScreenImage : String -> Model -> Html.Html Msg
viewFullScreenImage url { zoomState, zoomPosition, viewportSize } =
    div
        [ class "fullscreen-image-container"
        , Mouse.onDown (.clientPos >> FullScreenMouseDown)
        , Mouse.onUp (.clientPos >> FullScreenMouseUp)
        , Mouse.onMove (.clientPos >> FullScreenMouseOver)
        , Touch.onStart (Touch TouchStart)
        , Touch.onMove (Touch TouchMove)
        , Touch.onEnd (Touch TouchEnd)
        ]
        (case zoomState of
            ZoomedOut ->
                [ img [ class "fullscreen-image zoomed-out", src url ] []
                ]

            ZoomedIn ->
                [ img [ class "fullscreen-image zoomed-in", src url, zoomOffset zoomPosition viewportSize ] []
                ]
        )


zoomOffset : ( Float, Float ) -> ( Float, Float ) -> Html.Attribute msg
zoomOffset ( xZoom, yZoom ) ( xTotal, yTotal ) =
    style "transform" ("scale(2.5) translate(" ++ String.fromFloat (xTotal / 2 - xZoom) ++ "px, " ++ String.fromFloat (yTotal / 2 - yZoom) ++ "px)")


tileList : String -> List ( String, String, Attribute Msg ) -> Html.Html Msg
tileList heading tiles =
    let
        tile : ( String, String, Attribute Msg ) -> Html.Html Msg
        tile ( name, imageSource, attr ) =
            div [ class "" ]
                [ a [ attr ]
                    [ div
                        [ class "tile" ]
                        [ h3 [] [ text name ]
                        , img [ src imageSource ] []
                        ]
                    ]
                ]
    in
    div []
        [ h1 [] [ text heading ]
        , div [ class "tiles" ] (List.map tile tiles)
        ]



{-
   @@  @@ @@@@@  @@@@@   @@@@  @@@@@@ @@@@@@
   @@  @@ @@  @@ @@  @@ @@  @@   @@   @@
   @@  @@ @@@@@  @@  @@ @@@@@@   @@   @@@@
   @@  @@ @@     @@  @@ @@  @@   @@   @@
    @@@@  @@     @@@@@  @@  @@   @@   @@@@@@
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noUpdate : ( Model, Cmd msg )
        noUpdate =
            ( model, Cmd.none )

        updateModel : (Model -> Model) -> ( Model, Cmd msg )
        updateModel fn =
            ( fn model, Cmd.none )
    in
    case msg of
        GotViewport viewPort ->
            updateModel (setViewport viewPort)

        WindowResize width height ->
            updateModel (setWindowSize width height)

        Show screen ->
            updateModel (\_ -> { model | screen = screen })

        FullScreenMouseDown ( x, y ) ->
            updateModel (setZoomPosition ( x, y ) >> setZoomState ZoomedIn)

        FullScreenMouseUp ( x, y ) ->
            updateModel (setZoomPosition ( x, y ) >> setZoomState ZoomedOut)

        FullScreenMouseOver ( x, y ) ->
            updateModel (setZoomPosition ( x, y ))

        Touch eventType eventData ->
            ( handleTouch eventType eventData model, Cmd.none )


setZoomPosition : ( Float, Float ) -> Model -> Model
setZoomPosition zoomPosition model =
    { model | zoomPosition = zoomPosition }


setZoomState : ZoomState -> Model -> Model
setZoomState zoomState model =
    { model | zoomState = zoomState }


setViewport : Dom.Viewport -> Model -> Model
setViewport viewport model =
    { model | viewportSize = ( viewport.scene.width, viewport.scene.height ) } |> updateViewOrientation


setWindowSize : Int -> Int -> Model -> Model
setWindowSize width height model =
    { model | viewportSize = ( toFloat width, toFloat height ) } |> updateViewOrientation


updateViewOrientation : Model -> Model
updateViewOrientation model =
    let
        ( width, height ) =
            model.viewportSize
    in
    if width > height then
        { model | viewOrientation = Landscape }

    else
        { model | viewOrientation = Portrait }


handleTouch : TouchEventType -> Touch.Event -> Model -> Model
handleTouch eventType eventData model =
    case eventType of
        TouchStart ->
            model
                |> updateTouches (addTouches eventData.changedTouches)
                |> updateZoomStateFromTouches
                |> updateZoomPositionFromTouches

        TouchMove ->
            model
                |> updateTouches (moveTouches eventData.changedTouches)
                |> updateZoomPositionFromTouches

        TouchEnd ->
            model
                |> updateTouches (removeTouches eventData.changedTouches)
                |> updateZoomStateFromTouches
                |> updateZoomPositionFromTouches


updateZoomPositionFromTouches : Model -> Model
updateZoomPositionFromTouches model =
    let
        firstTouch =
            List.head model.touches
    in
    case firstTouch of
        Just touch ->
            { model | zoomPosition = touch.clientPos }

        Nothing ->
            model


updateZoomStateFromTouches : Model -> Model
updateZoomStateFromTouches model =
    let
        firstTouchId =
            List.head model.touches |> Maybe.map .identifier
    in
    case firstTouchId of
        Just 0 ->
            { model | zoomState = ZoomedIn }

        _ ->
            { model | zoomState = ZoomedOut }


updateTouches : (List Touch -> List Touch) -> Model -> Model
updateTouches fn model =
    { model | touches = fn model.touches }


addTouches : List Touch -> List Touch -> List Touch
addTouches newTouches existingTouches =
    List.append existingTouches newTouches


moveTouches : List Touch -> List Touch -> List Touch
moveTouches newTouches _ =
    newTouches


removeTouches : List Touch -> List Touch -> List Touch
removeTouches toRemove existingTouches =
    let
        ids =
            toRemove |> List.map .identifier
    in
    existingTouches |> List.filter (\t -> not <| List.member t.identifier ids)



{-
   @@   @@   @@@@   @@@@@@  @@  @@
   @@@ @@@  @@  @@    @@    @@@ @@
   @@ @ @@  @@@@@@    @@    @@ @@@
   @@   @@  @@  @@    @@    @@  @@
   @@   @@  @@  @@  @@@@@@  @@  @@
-}


main : Program () Model Msg
main =
    Browser.document
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.onResize WindowResize
        ]
