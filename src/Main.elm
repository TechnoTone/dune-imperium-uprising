module Main exposing (Model, Msg(..), initModel, main, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Html exposing (a, button, div, h1, h3, img, span, text)
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
    = Board
    | Leaders
    | Cards
    | Manuals


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
        ( { screen = Board
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


view : Model -> Browser.Document Msg
view model =
    { title = "Dune Imperium: Uprising"
    , body =
        [ div
            [ class "screen-container" ]
            [ viewMenuBar model, viewScreen model ]
        ]
    }


viewMenuBar : Model -> Html.Html Msg
viewMenuBar model =
    div
        [ class "menu-bar" ]
        [ a [ onClick (Show Board) ] [ text "Board" ]
        , a [ onClick (Show Leaders) ] [ text "Leaders" ]
        , a [ onClick (Show Cards) ] [ text "Cards" ]
        , a [ onClick (Show Manuals) ] [ text "Manuals" ]
        ]


viewScreen : Model -> Html.Html Msg
viewScreen model =
    case model.screen of
        Board ->
            viewFullScreenImage "/static/img/board.jpg" model

        Leaders ->
            viewLeaders

        Cards ->
            div [] [ text "Cards" ]

        Manuals ->
            viewManuals


viewLeaders : Html.Html Msg
viewLeaders =
    div [ class "leaders" ]
        [ viewLeader "Feyd-Rautha Harkonnen" "leader-feydrauthaharkonnen.png" ]


viewLeader : String -> String -> Html.Html Msg
viewLeader name imageSource =
    div []
        [ h1 [] [ text name ]
        , img [ src imageSource ] []
        ]


viewManuals : Html.Html Msg
viewManuals =
    div [ class "manuals" ]
        [ viewManual "Rulebook" "manual-rulebook.png" "rulebook.pdf"
        , viewManual "Supplements" "manual-supplements.png" "supplements.pdf"
        ]


viewManual : String -> String -> String -> Html.Html Msg
viewManual title imageSource pdfSource =
    div []
        [ h1 [] [ text title ]
        , a [ href pdfSource ] [ img [ src imageSource ] [] ]
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
