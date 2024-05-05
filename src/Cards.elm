module Cards exposing (Card, CardOptions, CardOrderBy(..), CardsGroup, get, view)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, name, src)
import Icons exposing (agentAccessIcon, factionIcon, gradeIcon, letterIcon, persuasionIcon)
import List.Extra as List


type CardOrderBy
    = CardOrderByAz
    | CardOrderByPersuasionCost
    | CardOrderByFactionSynergy
    | CardOrderByAgentAccess
    | CardOrderByGrade


type alias CardOptions =
    { orderBy : CardOrderBy
    , filter : String
    }


type alias Card =
    { name : String
    , imageName : String
    , persuasionCost : Int
    , factionSynergy : List Faction
    , agentAccess : List AgentAccess
    , grade : Grade
    }


type CardsGroup
    = GroupedByAz (List ( String, List Card ))
    | GroupedByPersuasionCost (List ( Int, List Card ))
    | GroupedByFactionSynergy (List ( Faction, List Card ))
    | GroupedByAgentAccess (List ( AgentAccess, List Card ))
    | GroupedByGrade (List ( Grade, List Card ))


type Faction
    = FremenFaction
    | BeneGesseritFaction
    | SpacingGuildFaction
    | EmperorFaction


factionSynergyList : List Faction
factionSynergyList =
    [ EmperorFaction
    , SpacingGuildFaction
    , BeneGesseritFaction
    , FremenFaction
    ]


factionIconImage : Faction -> String
factionIconImage faction =
    case faction of
        EmperorFaction ->
            "faction-emperor-icon.png"

        SpacingGuildFaction ->
            "faction-spacing-guild-icon.png"

        BeneGesseritFaction ->
            "faction-bene-gesserit-icon.png"

        FremenFaction ->
            "faction-fremen-icon.png"


type AgentAccess
    = EmperorAccess
    | SpacingGuildAccess
    | BeneGesseritAccess
    | FremenAccess
    | LandsraadAccess
    | CityAccess
    | SpiceTradeAccess
    | SpyAccess


agentAccessList : List AgentAccess
agentAccessList =
    [ EmperorAccess
    , SpacingGuildAccess
    , BeneGesseritAccess
    , FremenAccess
    , LandsraadAccess
    , CityAccess
    , SpiceTradeAccess
    , SpyAccess
    ]


agentAccessImage : AgentAccess -> String
agentAccessImage agentAccess =
    case agentAccess of
        EmperorAccess ->
            "access-emperor-icon.png"

        SpacingGuildAccess ->
            "access-spacing-guild-icon.png"

        BeneGesseritAccess ->
            "access-bene-gesserit-icon.png"

        FremenAccess ->
            "access-fremen-icon.png"

        LandsraadAccess ->
            "access-landsraad-icon.png"

        CityAccess ->
            "access-city-icon.png"

        SpiceTradeAccess ->
            "access-spice-trade-icon.png"

        SpyAccess ->
            "access-spy-icon.png"


type Grade
    = GradeA
    | GradeB
    | GradeC
    | GradeS
    | GradeAm
    | GradeD
    | GradeAp
    | GradeSpp
    | GradeF
    | GradeU


gradeList : List Grade
gradeList =
    [ GradeSpp
    , GradeS
    , GradeAp
    , GradeA
    , GradeAm
    , GradeB
    , GradeC
    , GradeD
    , GradeF
    , GradeU
    ]


gradeString : Grade -> String
gradeString grade =
    case grade of
        GradeSpp ->
            "S++"

        GradeS ->
            "S"

        GradeAp ->
            "A+"

        GradeA ->
            "A"

        GradeAm ->
            "A-"

        GradeB ->
            "B"

        GradeC ->
            "C"

        GradeD ->
            "D"

        GradeF ->
            "F"

        GradeU ->
            "U"


fullCardList : List Card
fullCardList =
    [ Card "Arrakis Revolt" "card-arrakisrevolt.jpg" 6 [ FremenFaction ] [ CityAccess ] GradeA
    , Card "Bene Gessserit Operative" "card-benegesseritoperative.jpg" 3 [ BeneGesseritFaction ] [ BeneGesseritAccess ] GradeA
    , Card "Branching Path" "card-branchingpath.jpg" 3 [ BeneGesseritFaction ] [ BeneGesseritAccess, CityAccess ] GradeB
    , Card "Calculus of Power" "card-calculusofpower.jpg" 3 [ EmperorFaction ] [ CityAccess, SpyAccess ] GradeA
    , Card "Captured Mentat" "card-capturedmentat.jpg" 5 [] [ LandsraadAccess, SpiceTradeAccess ] GradeAm
    , Card "Cargo Runner " "card-cargorunner.jpg" 3 [ SpacingGuildFaction ] [ LandsraadAccess, CityAccess, SpiceTradeAccess ] GradeC
    , Card "Chani, Clever Tactician" "card-chaniclevertactician.jpg" 5 [ FremenFaction ] [ FremenAccess, LandsraadAccess, CityAccess ] GradeB
    , Card "Corrinth City" "card-corrinthcity.jpg" 6 [ EmperorFaction ] [ EmperorAccess, SpiceTradeAccess ] GradeS
    , Card "Covert Operation" "card-covertoperation.jpg" 3 [] [ SpyAccess ] GradeB
    , Card "Dangerous Rhetoric" "card-dangerousrhetoric.jpg" 3 [] [ SpiceTradeAccess, SpyAccess ] GradeA
    , Card "Delivery Agreement" "card-deliveryagreement.jpg" 5 [ SpacingGuildFaction ] [ CityAccess ] GradeC
    , Card "Desert Power" "card-desertpower.jpg" 6 [ FremenFaction ] [ LandsraadAccess ] GradeA
    , Card "Desert Survival" "card-desertsurvival.jpg" 2 [ FremenFaction ] [ LandsraadAccess ] GradeC
    , Card "Double Agent" "card-doubleagent.jpg" 3 [ EmperorFaction, SpacingGuildFaction ] [ LandsraadAccess, CityAccess, SpiceTradeAccess ] GradeC
    , Card "Ecological Testing Station" "card-ecologicaltestingstation.jpg" 3 [ FremenFaction ] [ FremenAccess, CityAccess ] GradeC
    , Card "Fedaykin Stilltent" "card-fedaykinstilltent.jpg" 2 [ FremenFaction ] [ LandsraadAccess ] GradeB
    , Card "Guild Envoy" "card-guildenvoy.jpg" 3 [ SpacingGuildFaction ] [ EmperorAccess, SpacingGuildAccess, BeneGesseritAccess, FremenAccess ] GradeA
    , Card "Guild Spy" "card-guildspy.jpg" 3 [ SpacingGuildFaction ] [ SpyAccess ] GradeS
    , Card "Hidden Missive" "card-hiddenmissive.jpg" 2 [ BeneGesseritFaction ] [ SpiceTradeAccess ] GradeD
    , Card "Imperial Spymaster" "card-imperialspymaster.jpg" 2 [ EmperorFaction ] [ EmperorAccess, SpyAccess ] GradeC
    , Card "In High Places" "card-inhighplaces.jpg" 5 [ EmperorFaction, BeneGesseritFaction ] [ EmperorAccess, BeneGesseritAccess ] GradeAp
    , Card "Interstellar Trade" "card-interstellartrade.jpg" 7 [ SpacingGuildFaction ] [ LandsraadAccess, CityAccess, SpiceTradeAccess ] GradeS
    , Card "Junction Headquarters" "card-junctionheadquarters.jpg" 6 [ SpacingGuildFaction ] [ LandsraadAccess, CityAccess, SpiceTradeAccess ] GradeC
    , Card "Leadership" "card-leadership.jpg" 5 [ FremenFaction ] [ FremenAccess, LandsraadAccess ] GradeA
    , Card "Long Live The Fighters" "card-longlivethefighters.jpg" 7 [ FremenFaction ] [ FremenAccess, CityAccess ] GradeAp
    , Card "Maker Keeper" "card-makerkeeper.jpg" 2 [ BeneGesseritFaction, FremenFaction ] [ CityAccess ] GradeB
    , Card "Maula Pistol" "card-maulapistol.jpg" 3 [ FremenFaction ] [ LandsraadAccess, CityAccess ] GradeD
    , Card "Northern Watermaster" "card-northernwatermaster.jpg" 3 [ FremenFaction ] [ CityAccess ] GradeB
    , Card "Overthrow" "card-overthrow.jpg" 8 [] [ EmperorAccess, SpacingGuildAccess, BeneGesseritAccess, FremenAccess ] GradeSpp
    , Card "Paracompass" "card-paracompass.jpg" 4 [] [ CityAccess ] GradeC
    , Card "Price Is No Object" "card-priceisnoobject.jpg" 6 [ EmperorFaction, BeneGesseritFaction ] [ EmperorAccess, BeneGesseritAccess ] GradeAp
    , Card "Priority Contracts" "card-prioritycontracts.jpg" 6 [ SpacingGuildFaction ] [ LandsraadAccess, SpiceTradeAccess ] GradeB
    , Card "Public Spectacle" "card-publicspectacle.jpg" 4 [ EmperorFaction ] [ SpyAccess ] GradeS
    , Card "Rebel Supplier" "card-rebelsupplier.jpg" 3 [ FremenFaction ] [ CityAccess ] GradeC
    , Card "Reliable Informant" "card-reliableinformant.jpg" 2 [ SpacingGuildFaction ] [ SpacingGuildAccess, SpyAccess ] GradeA
    , Card "Sardaukar Coordination" "card-sardaukarcoordination.jpg" 4 [ EmperorFaction ] [ EmperorAccess, SpiceTradeAccess ] GradeS
    , Card "Sardaukar Soldier" "card-sardaukarsoldier.jpg" 1 [ EmperorFaction ] [ CityAccess ] GradeD
    , Card "Shishakli" "card-shishakli.jpg" 4 [ FremenFaction ] [ LandsraadAccess, CityAccess ] GradeA
    , Card "Smuggler’s Haven" "card-smugglershaven.jpg" 4 [ SpacingGuildFaction ] [ SpacingGuildAccess, LandsraadAccess ] GradeS
    , Card "Smuggler’s Harvester" "card-smugglersharvester.jpg" 1 [ SpacingGuildFaction ] [ LandsraadAccess ] GradeC
    , Card "Southern Elders" "card-southernelders.jpg" 4 [ BeneGesseritFaction, FremenFaction ] [ BeneGesseritAccess, FremenAccess ] GradeA
    , Card "Spacetime Folding" "card-spacetimefolding.jpg" 1 [ SpacingGuildFaction ] [ SpacingGuildAccess ] GradeB
    , Card "Spacing Guild’s Favor" "card-spacingguildsfavor.jpg" 5 [ SpacingGuildFaction ] [ SpacingGuildAccess, LandsraadAccess ] GradeA
    , Card "Spy Network" "card-spynetwork.jpg" 2 [ EmperorFaction, SpacingGuildFaction ] [] GradeB
    , Card "Steersman" "card-steersman.jpg" 8 [ SpacingGuildFaction ] [ SpacingGuildAccess, LandsraadAccess, CityAccess, SpiceTradeAccess ] GradeS
    , Card "Stilgar, The Devoted" "card-stilgarthedevoted.jpg" 6 [ FremenFaction ] [ FremenAccess, LandsraadAccess, CityAccess ] GradeS
    , Card "Strike Fleet" "card-strikefleet.jpg" 5 [] [ SpyAccess ] GradeS
    , Card "Subversive Advisor" "card-subversiveadvisor.jpg" 5 [] [ SpyAccess ] GradeB
    , Card "The Spice Must Flow" "card-thespicemustflow.jpg" 9 [ SpacingGuildFaction ] [] GradeU
    , Card "Thumper" "card-thumper.jpg" 3 [ FremenFaction ] [ LandsraadAccess ] GradeD
    , Card "Treacherous Maneuver" "card-treacherousmaneuver.jpg" 5 [ EmperorFaction ] [ EmperorAccess, SpacingGuildAccess, BeneGesseritAccess, FremenAccess ] GradeB
    , Card "Tread in Darkness" "card-treadindarkness.jpg" 4 [ BeneGesseritFaction ] [ LandsraadAccess, CityAccess, SpiceTradeAccess ] GradeC
    , Card "Truthtrance" "card-truthtrance.jpg" 4 [ BeneGesseritFaction ] [ EmperorAccess, SpacingGuildAccess, BeneGesseritAccess, FremenAccess ] GradeB
    , Card "Undercover Asset" "card-undercoverasset.jpg" 2 [ EmperorFaction, SpacingGuildFaction ] [ LandsraadAccess, CityAccess, SpiceTradeAccess ] GradeA
    , Card "Unswerving Loyalty" "card-unswervingloyalty.jpg" 1 [ FremenFaction ] [] GradeD
    , Card "Weirding Woman" "card-weirdingwoman.jpg" 1 [ BeneGesseritFaction ] [ LandsraadAccess, CityAccess ] GradeF
    , Card "Wheels Within Wheels" "card-wheelswithinwheels.jpg" 2 [ EmperorFaction, SpacingGuildFaction ] [ SpyAccess ] GradeB
    ]


get : CardOptions -> CardsGroup
get { orderBy, filter } =
    fullCardList
        |> List.filter (cardNameContains filter)
        |> groupedBy orderBy


cardNameContains : String -> Card -> Bool
cardNameContains filter card =
    if String.trim filter == "" then
        True

    else
        String.contains (String.toLower filter) (String.toLower card.name)


groupedBy : CardOrderBy -> List Card -> CardsGroup
groupedBy orderBy cardList =
    case orderBy of
        CardOrderByAz ->
            let
                cardNameFirstLetter : Card -> String
                cardNameFirstLetter { name } =
                    String.left 1 name |> String.toUpper
            in
            GroupedByAz
                (cardList
                    |> List.map cardNameFirstLetter
                    |> List.unique
                    |> List.sort
                    |> List.map
                        (\l ->
                            ( l
                            , cardList
                                |> List.filter (cardNameFirstLetter >> (==) l)
                            )
                        )
                )

        CardOrderByPersuasionCost ->
            GroupedByPersuasionCost
                (cardList
                    |> List.map .persuasionCost
                    |> List.unique
                    |> List.sort
                    |> List.map
                        (\p ->
                            ( p
                            , cardList
                                |> List.filter (.persuasionCost >> (==) p)
                            )
                        )
                )

        CardOrderByFactionSynergy ->
            GroupedByFactionSynergy
                (factionSynergyList
                    |> List.map
                        (\f ->
                            ( f
                            , cardList
                                |> List.filter (.factionSynergy >> List.member f)
                            )
                        )
                    |> List.filter (Tuple.second >> List.isEmpty >> not)
                )

        CardOrderByAgentAccess ->
            GroupedByAgentAccess
                (agentAccessList
                    |> List.map
                        (\a ->
                            ( a
                            , cardList
                                |> List.filter (.agentAccess >> List.member a)
                            )
                        )
                    |> List.filter (Tuple.second >> List.isEmpty >> not)
                )

        CardOrderByGrade ->
            GroupedByGrade
                (gradeList
                    |> List.map
                        (\g ->
                            ( g
                            , cardList
                                |> List.filter (.grade >> (==) g)
                            )
                        )
                    |> List.filter (Tuple.second >> List.isEmpty >> not)
                )


view : CardsGroup -> List (Html msg)
view cardsGroup =
    let
        groupsFn : (a -> Html msg) -> List ( a, List Card ) -> List (Html msg)
        groupsFn iconFn groupsList =
            groupsList
                |> List.concatMap
                    (\( groupId, cards ) ->
                        [ div
                            [ class "group-header" ]
                            [ iconFn groupId ]
                        , viewCards cards
                        ]
                    )
    in
    case cardsGroup of
        GroupedByAz groups ->
            groupsFn letterIcon groups

        GroupedByPersuasionCost groups ->
            groupsFn persuasionIcon groups

        GroupedByFactionSynergy groups ->
            groups
                |> List.map (Tuple.mapFirst factionIconImage)
                |> groupsFn factionIcon

        GroupedByAgentAccess groups ->
            groups
                |> List.map (Tuple.mapFirst agentAccessImage)
                |> groupsFn agentAccessIcon

        GroupedByGrade groups ->
            groups
                |> List.map (Tuple.mapFirst gradeString)
                |> groupsFn gradeIcon


viewCards : List Card -> Html msg
viewCards cards =
    div
        [ class "group-cards" ]
        (cards
            |> List.map .imageName
            |> List.map
                (\c ->
                    img [ class "card", src c ] []
                )
        )
