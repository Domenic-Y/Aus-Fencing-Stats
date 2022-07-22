module SingleFencerPage exposing (viewSingleFencerPage)

import AFFData exposing (AFFData)
import Bout exposing (Bout, viewBouts)
import Chart as C
import Chart.Attributes as CA
import Competition exposing (Competition, competitonToYear, getCompetition)
import Css exposing (ColorValue, alignItems, auto, backgroundColor, borderRadius, center, displayFlex, fontFamilies, hex, justifyContent, margin, marginBottom, padding, px, textAlign, vw, maxWidth, em, maxWidth)
import Dict
import Fencer exposing (Fencer, getFencerWithId)
import Html.Styled exposing (Attribute, Html, div, h1, h2, h3, styled, text)
import Html.Styled.Attributes exposing (css)
import Set
import Svg


viewSingleFencerPage : Maybe Fencer -> AFFData -> Html msg
viewSingleFencerPage maybeFencer data =
    case maybeFencer of
        Just fencer ->
            let
                boutsWon : List Bout
                boutsWon =
                    List.filter (\bout -> fencer.id == bout.winnerID) data.bouts

                boutsLost : List Bout
                boutsLost =
                    List.filter (\bout -> fencer.id == bout.loserID) data.bouts

                boutsAll : List Bout
                boutsAll =
                    boutsLost ++ boutsWon
            in
            div [ css [ maxWidth (em 60) ] ]
                [ h1 [] [ text <| Fencer.firstNameLastName <| fencer ]
                , div [] [ h3 [] [ text <| String.fromInt (List.length boutsAll) ++ " Bouts Fenced 🤺" ] ]
                , div [] [ h3 [] [ text <| String.fromInt (List.length boutsWon) ++ " Bouts Won 🏆" ] ]
                , div [] [ h3 [] [ text <| String.fromInt (List.length boutsLost) ++ " Bouts Lost 😕" ] ]
                , div [] [ h3 [] [ text <| String.fromInt (Set.size <| Set.fromList <| List.map (\bout -> ( bout.eventID, bout.competitionID )) boutsAll) ++ " Events 🚀" ] ]
                , div [] [ h3 [] [ text <| String.fromInt (Set.size <| Set.fromList <| List.map .competitionID boutsAll) ++ " Competitions 📅" ] ]
                , div [ css [ displayFlex, alignItems center, justifyContent center ] ]
                    [ div [] [ h3 [] [ text "Most Victories Over" ], div [] <| List.map (\( id, _ ) -> nameBadgeGreen <| getFencerWithId id data.fencers) <| List.take 3 <| List.sortBy (\tuple -> Tuple.second tuple) <| Dict.toList <| countFencers <| List.map (\bout -> bout.loserID) <| boutsWon ]
                    , div [] [ h3 [] [ text "Most Defeated By" ], div [] <| List.map (\( id, _ ) -> nameBadgeRed <| getFencerWithId id data.fencers) <| List.take 3 <| List.sortBy (\tuple -> Tuple.second tuple) <| Dict.toList <| countFencers <| List.map (\bout -> bout.winnerID) <| boutsLost ]
                    ]
                , div [ css [ maxWidth (em 60), Css.height (vw 10), margin auto, marginBottom (px 300) ] ] [ h2 [] [ text "Cumulative Indicator" ], h3 [] [ text "= Points Scored − Points Recieved" ], indicatorChart boutsWon boutsLost data.competitions ]
                , viewBouts (List.filter (\bout -> fencer.id == bout.winnerID || fencer.id == bout.loserID) data.bouts) data.fencers data.competitions
                ]

        Nothing ->
            div [] [ text "No Fencer Selected" ]


indicatorChart : List Bout -> List Bout -> List Competition -> Html msg
indicatorChart boutsWon boutsLost comps =
    Html.Styled.fromUnstyled <|
        C.chart
            [ CA.height 100
            , CA.width 300
            , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
            ]
            [ C.xLabels [ CA.fontSize 8 ]
            , C.xLabel [ CA.fontSize 11, CA.moveDown 10 ] [ Svg.text "Year" ]
            , C.yLabels [ CA.withGrid, CA.fontSize 8 ]
            , C.yLabel [ CA.fontSize 11, CA.moveLeft 20, CA.rotate 90, CA.alignMiddle ] [ Svg.text "Indicator" ]
            , C.series .x
                [ C.interpolated .y [] [ CA.circle, CA.size 10 ]
                ]
                (accumulatePrevious <| List.map (\( year, indicator) -> { x = toFloat year, y = toFloat indicator }) <| Dict.toList <| mergeIndicators (countIndicatorPerYear boutsWon comps) (countIndicatorPerYear boutsLost comps))
            ]


countFencers : List String -> Dict.Dict String Int
countFencers fencers =
    fencers
        |> List.foldr
            (\fencer carry ->
                Dict.update
                    fencer
                    (\existingCount ->
                        case existingCount of
                            Just count ->
                                Just (count - 1)

                            Nothing ->
                                Just 0
                    )
                    carry
            )
            Dict.empty


nameBadgeElement : ColorValue compatible -> List (Attribute msg) -> List (Html msg) -> Html msg
nameBadgeElement color =
    styled div
        [ backgroundColor color
        , maxWidth (em 15)
        , borderRadius (px 20)
        , margin (px 10)
        , fontFamilies [ "Verdana", "Arial" ]
        , padding (px 5)
        , textAlign center
        , Css.color (hex "#342B40")
        , padding (em 1)
        ]


nameBadge : ColorValue compatible -> Maybe Fencer -> Html msg
nameBadge color maybeFencer =
    case maybeFencer of
        Just fencer ->
            nameBadgeElement color [] [ text <| Fencer.firstNameLastName <| fencer]

        Nothing ->
            text "Fencer Not Found :("


nameBadgeGreen : Maybe Fencer -> Html msg
nameBadgeGreen =
    nameBadge (hex "#54B84C")


nameBadgeRed : Maybe Fencer -> Html msg
nameBadgeRed =
    nameBadge (hex "#FD7B83")


countIndicatorPerYear : List Bout -> List Competition -> Dict.Dict Int Int
countIndicatorPerYear bouts comps =
    bouts
        |> List.foldr
            (\bout carry ->
                Dict.update
                    (Maybe.withDefault 2000 <| Maybe.andThen competitonToYear <| (\id -> getCompetition id comps) <| bout.competitionID)
                    (\existingCount ->
                        case existingCount of
                            Just count ->
                                Just (count + (bout.winnerScore - bout.loserScore))

                            Nothing ->
                                Just (bout.winnerScore - bout.loserScore)
                    )
                    carry
            )
            Dict.empty


mergeIndicators : Dict.Dict Int Int -> Dict.Dict Int Int -> Dict.Dict Int Int
mergeIndicators dictA dictB =
    Dict.merge
        (\key a -> Dict.insert key a)
        (\key a b -> Dict.insert key (a - b))
        (\key b -> Dict.insert key b)
        dictA
        dictB
        Dict.empty

accumulatePrevious : List { x : Float, y : Float } -> List { x : Float, y : Float }
accumulatePrevious list =
    List.indexedMap (\i data -> { x = data.x, y = List.sum (List.take i (List.map .y list)) }) list

