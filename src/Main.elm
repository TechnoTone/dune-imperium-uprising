module Main exposing (Model, Msg(..), initModel, main, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events as Browser
import Cards exposing (CardOptions, CardOrderBy(..))
import Html exposing (Attribute, a, div, h1, h3, img, text)
import Html.Attributes exposing (class, classList, href, src, value)
import Html.Events exposing (onClick)
import Icons exposing (gradeIcon, letterIcon, persuasionIcon)
import Task


type alias Model =
    { screen : Screen
    , viewOrientation : Orientation
    , zoomState : ZoomState
    , cardBarView : CardBarView
    , cardOptions : CardOptions
    }


type Screen
    = Board
    | Leaders
    | Cards
    | Combat
    | Manuals


type CardBarView
    = CardIcon
    | CardList


type Orientation
    = Portrait
    | Landscape


type ZoomState
    = ZoomedOut
    | ZoomedIn


type Msg
    = GotViewport Dom.Viewport
    | WindowResize Int Int
    | Show Screen
    | SetZoomState ZoomState
    | ShowCardBarIcon CardOrderBy
    | ShowCardBarList
    | CardFilterChange String


initModel : () -> ( Model, Cmd Msg )
initModel =
    always <|
        ( { screen = Board
          , viewOrientation = Portrait
          , zoomState = ZoomedOut
          , cardBarView = CardIcon
          , cardOptions = CardOptions CardOrderByAz ""
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
    let
        orientationClass : Attribute msg
        orientationClass =
            case model.viewOrientation of
                Portrait ->
                    class "portrait"

                Landscape ->
                    class "landscape"
    in
    { title = "Dune Imperium: Uprising"
    , body =
        [ div
            [ class "screen-container", orientationClass ]
            [ buttonBar model
            , viewScreen model
            ]
        ]
    }


buttonBar : Model -> Html.Html Msg
buttonBar model =
    div
        [ class "button-bar" ]
        [ simpleBarButton "menu-board.jpg" (Show Board) (model.screen == Board)
        , simpleBarButton "menu-leaders.png" (Show Leaders) (model.screen == Leaders)
        , cardsButton (model.screen == Cards) model.cardBarView model.cardOptions
        , simpleBarButton "menu-combat.png" (Show Combat) (model.screen == Combat)
        , simpleBarButton "menu-manuals.png" (Show Manuals) (model.screen == Manuals)
        ]


buttonBarButton : Bool -> List (Html.Html msg) -> Html.Html msg
buttonBarButton isActive content =
    div
        [ classList [ ( "button", True ), ( "active", isActive ) ] ]
        content


buttonBarButtonWithCustomClass : String -> Bool -> List (Html.Html msg) -> Html.Html msg
buttonBarButtonWithCustomClass customClass isActive content =
    div
        [ classList [ ( "button", True ), ( "active", isActive ), ( customClass, True ) ] ]
        content


simpleBarButton : String -> Msg -> Bool -> Html.Html Msg
simpleBarButton imageSource msg isActive =
    buttonBarButton isActive [ img [ onClick msg, class "button-image", src imageSource ] [] ]


cardsButton : Bool -> CardBarView -> CardOptions -> Html.Html Msg
cardsButton isActive cardBarView cardOptions =
    if isActive then
        buttonBarButton
            isActive
            [ img [ onClick ShowCardBarList, class "button-image", src "menu-cards.jpg" ] []
            , case cardBarView of
                CardIcon ->
                    viewCardBarIcon cardOptions.orderBy

                CardList ->
                    viewCardBarList cardOptions.orderBy
            ]

    else
        buttonBarButton isActive [ div [ onClick (Show Cards) ] [ img [ class "button-image", src "menu-cards.jpg" ] [] ] ]


viewCardBarIcon : CardOrderBy -> Html.Html Msg
viewCardBarIcon orderBy =
    div
        [ class "card-button" ]
        [ div [ class "card-button-icon", onClick ShowCardBarList ] (viewCardOrderImage orderBy) ]


viewCardBarList : CardOrderBy -> Html.Html Msg
viewCardBarList currentCardOrderBy =
    let
        item : String -> CardOrderBy -> Html.Html Msg
        item label cardOrderBy =
            div
                [ classList
                    [ ( "item", True )
                    , ( "current", cardOrderBy == currentCardOrderBy )
                    ]
                , onClick <| ShowCardBarIcon cardOrderBy
                ]
                [ div
                    [ class "card-button-icon"
                    ]
                    (viewCardOrderImage cardOrderBy)
                , div [ class "description" ] [ text label ]
                ]
    in
    div
        [ class "card-button" ]
        [ div
            [ class "card-button-icon" ]
            (viewCardOrderImage currentCardOrderBy)
        , div
            [ class "list-background"
            , onClick <| ShowCardBarIcon currentCardOrderBy
            ]
            []
        , div
            [ class "list" ]
            [ item "Name" CardOrderByAz
            , item "Persuasion Cost" CardOrderByPersuasionCost
            , item "Agent Access" CardOrderByAgentAccess
            , item "Faction Synergy" CardOrderByFactionSynergy
            , item "Grade" CardOrderByGrade
            ]
        ]


viewCardOrderImage : CardOrderBy -> List (Html.Html Msg)
viewCardOrderImage order =
    case order of
        CardOrderByAz ->
            letterIcon "A-Z"

        CardOrderByPersuasionCost ->
            persuasionIcon 9

        CardOrderByAgentAccess ->
            [ div
                [ class "group-icon agent-access-icon" ]
                [ div [] [ img [ src "access-icon.png" ] [] ] ]
            ]

        CardOrderByFactionSynergy ->
            [ div
                [ class "group-icon faction-icon" ]
                [ img [ src "faction-icon.png" ] [] ]
            ]

        CardOrderByGrade ->
            gradeIcon "A+"


viewScreen : Model -> Html.Html Msg
viewScreen model =
    div [ class "content" ]
        [ case model.screen of
            Board ->
                viewFullScreenImage "board.jpg" model

            Leaders ->
                viewLeaders

            Cards ->
                viewCards model.cardOptions

            Combat ->
                viewFullScreenImage "combatreference.png" model

            Manuals ->
                viewManuals
        ]


viewLeaders : Html.Html Msg
viewLeaders =
    [ "leader-mauddib.png"
    , "leader-gurneyhalleck.png"
    , "leader-feydrauthaharkonnen.png"
    , "leader-ladymargotfenring.png"
    , "leader-ladyambermetulli.png"
    , "leader-princessirulan.png"
    , "leader-ladyjessica.png"
    , "leader-reverendmotherjessica.png"
    , "leader-shaddamconringiv.png"
    , "leader-stabantuer.png"
    ]
        |> List.map (\imgSrc -> div [ class "leader" ] [ img [ src imgSrc ] [] ])
        |> div [ class "leaders" ]


viewFullScreenImage : String -> Model -> Html.Html Msg
viewFullScreenImage url model =
    case model.zoomState of
        ZoomedOut ->
            div
                [ class "fullscreen-image-container"
                , onClick (SetZoomState ZoomedIn)
                ]
                [ img
                    [ class "fullscreen-image zoomed-out"
                    , src url
                    ]
                    []
                ]

        ZoomedIn ->
            div
                [ class "fullscreen-image-container"
                , onClick (SetZoomState ZoomedOut)
                ]
                [ img
                    [ class "fullscreen-image zoomed-in"
                    , src url
                    ]
                    []
                ]


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


viewCards : CardOptions -> Html.Html Msg
viewCards options =
    div [ class "cards-container" ]
        [ div
            [ class "cards-body" ]
            (viewCardList options)
        ]


viewCardList : CardOptions -> List (Html.Html Msg)
viewCardList options =
    options
        |> Cards.get
        |> Cards.view


viewManuals : Html.Html Msg
viewManuals =
    tileList "Manuals"
        [ ( "Rulebook (pdf)", "manual-rulebook.png", href "rulebook.pdf" )
        , ( "Supplements (pdf)", "manual-supplements.png", href "supplements.pdf" )
        , ( "FAQ (pdf)", "manual-faq.png", href "faq.pdf" )
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
            updateModel (setOrientationFromViewPort viewPort)

        WindowResize width height ->
            updateModel (setOrientation width height)

        Show screen ->
            updateModel (\_ -> { model | screen = screen })

        SetZoomState zoomState ->
            updateModel (setZoomState zoomState)

        ShowCardBarIcon cardOrderBy ->
            updateModel
                (setCardBarView CardIcon
                    >> updateCardOptions (setCardOrderBy cardOrderBy)
                )

        ShowCardBarList ->
            updateModel (setCardBarView CardList)

        CardFilterChange value ->
            updateModel (updateCardOptions (setCardFilter value))


updateCardOptions : (CardOptions -> CardOptions) -> Model -> Model
updateCardOptions fn model =
    { model | cardOptions = fn model.cardOptions }


setCardBarView : CardBarView -> Model -> Model
setCardBarView cardBarView model =
    { model | cardBarView = cardBarView }


setCardFilter : String -> CardOptions -> CardOptions
setCardFilter value options =
    { options | filter = value }


setCardOrderBy : CardOrderBy -> CardOptions -> CardOptions
setCardOrderBy order options =
    { options | orderBy = order }


setZoomState : ZoomState -> Model -> Model
setZoomState zoomState model =
    { model | zoomState = zoomState }


setOrientation : Int -> Int -> Model -> Model
setOrientation width height model =
    if width > height then
        { model | viewOrientation = Landscape }

    else
        { model | viewOrientation = Portrait }


setOrientationFromViewPort : Dom.Viewport -> Model -> Model
setOrientationFromViewPort viewport model =
    setOrientation (round viewport.scene.width) (round viewport.scene.height) model



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
        [ Browser.onResize WindowResize ]
