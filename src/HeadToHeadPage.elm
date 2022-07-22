module HeadToHeadPage exposing (viewHeadToHeadPage)

import AFFData exposing (AFFData)
import Bout exposing (Bout, viewBouts)
import Fencer exposing (Fencer)
import Html.Styled exposing (Html, div, h1, h2,  text)
import Html.Styled.Attributes exposing (css)
import Css exposing (displayFlex, margin, px)


viewHeadToHeadPage : Maybe Fencer -> Maybe Fencer -> AFFData -> Html msg
viewHeadToHeadPage maybeFencerLeft maybeFencerRight data =
    div
        []
        [ h1 [] [ text "Head to Head" ]
        , div [css [displayFlex]] [ viewMaybeFencer maybeFencerLeft, viewMaybeFencer maybeFencerRight ]
        , case ( maybeFencerLeft, maybeFencerRight ) of
            ( Just fencerLeft, Just fencerRight ) ->
                viewBouts (getBouts fencerLeft fencerRight data.bouts) data.fencers data.competitions

            _ ->
                text ""
        ]


viewMaybeFencer : Maybe Fencer -> Html msg
viewMaybeFencer maybeFencer =
    case maybeFencer of
        Just fencer ->
            div [css [margin (px 10)]] [ h2 [] [text fencer.fencerName ]]

        Nothing ->
            div [] [ text "Please Select Fencer" ]


getBouts : Fencer -> Fencer -> List Bout -> List Bout
getBouts fencerLeft fencerRight allBouts =
    List.filter (\bout -> (bout.winnerID == fencerLeft.id && bout.loserID == fencerRight.id) || (bout.winnerID == fencerRight.id && bout.loserID == fencerLeft.id)) allBouts
