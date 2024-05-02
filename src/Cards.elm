module Cards exposing (Card, CardOptions, CardOrderBy(..), getCards)


type CardOrderBy
    = CardOrderByAz
    | CardOrderByPersuasionCost
    | CardOrderByAgentAccess
    | CardOrderByFactionSynergy
    | CardOrderByTier


type alias CardOptions =
    { orderBy : CardOrderBy
    , filter : String
    }


type alias Card =
    { name : String
    , persuasionCost : Int
    , factionSynergy : List String
    , agentAccess : List String
    , tier : Int
    }


getCards : CardOptions -> List Card
getCards { filter } =
    let
        filteredCards =
            List.filter (cardNameContains filter) cards
    in
    filteredCards


cardNameContains : String -> Card -> Bool
cardNameContains filter card =
    if String.trim filter == "" then
        True

    else
        String.contains (String.toLower filter) (String.toLower card.name)


cards : List Card
cards =
    []
