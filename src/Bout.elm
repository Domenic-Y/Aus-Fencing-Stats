module Bout exposing (Bout, BoutType(..), intToBoutType, viewBouts)

import Competition exposing (Competition, getCompetition, getCompetitionName)
import Fencer exposing (Fencer, getFencerNameAtIndex)
import Html.Styled exposing (Html, div, p, text)


type alias Bout =
    { winnerID : String
    , loserID : String
    , winnerScore : Int
    , loserScore : Int
    , boutType : BoutType
    , eventID : String
    , competitionID : String
    }


type BoutType
    = Pool
    | DE


intToBoutType : number -> BoutType
intToBoutType int =
    if int == 0 then
        DE

    else
        Pool


viewBout : Bout -> List Fencer -> List Competition -> Html msg
viewBout { winnerID, loserID, winnerScore, loserScore, boutType, eventID, competitionID } fencers competitions =
    div []
        [ p [] [ text <| getFencerNameAtIndex winnerID fencers ++ " " ++ String.fromInt winnerScore ++ ":" ++ String.fromInt loserScore ++ " " ++ getFencerNameAtIndex loserID fencers ]
        , p [] [ text <| getCompetitionName <| getCompetition competitionID competitions ]
        ]


viewBouts : List Bout -> List Fencer -> List Competition -> Html msg
viewBouts bouts fencers competitions =
    div [] (List.map (\bout -> viewBout bout fencers competitions) bouts)
