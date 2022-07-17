module Competition exposing (Competition, competitonToYear, getCompetition)


type alias Competition =
    { id : String
    , competitionName : String
    }


getCompetition : String -> List Competition -> Maybe Competition
getCompetition id competitions =
    let
        filtered : List Competition
        filtered =
            List.filter (\competition -> competition.id == id) competitions
    in
    case filtered of
        [] ->
            Nothing

        x :: _ ->
            Just x


competitonToYear : Competition -> Maybe Int
competitonToYear comp =
    let
        yearAndName : List String
        yearAndName =
            String.split " - " comp.competitionName
    in
    if List.length yearAndName == 2 then
        Maybe.andThen String.toInt <| List.head yearAndName

    else
        Nothing
