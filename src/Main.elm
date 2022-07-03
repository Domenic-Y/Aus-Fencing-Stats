module Main exposing (..)

import Array
import Browser
import Chart as C
import Chart.Attributes as CA
import Css exposing (..)
import Dict
import ElmTextSearch
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (on, onBlur, onClick, onFocus, onInput, onMouseDown)
import Json.Decode as D
import List
import Set
import Time exposing (Month(..))
import Time.Extra exposing (Parts, partsToPosix)
import Svg as Svg


-- MAIN


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Loaded AFFData StatsMode SearchData
    | DecodeFailed D.Error
    | SearchIndexFailed


type StatsMode
    = SingleFencer (Maybe Fencer)
    | HeadToHead (Maybe Fencer) (Maybe Fencer)


type alias SearchData =
    { searchIndex : ElmTextSearch.Index Fencer
    , searchText : String
    , searchResult : SearchResult
    , focused : Bool
    }


type SearchResult
    = SearchNone
    | SearchSuccess (List ( String, Float ))
    | SearchFailed String


type alias AFFData =
    { bouts : List RawBout
    , fencers : List Fencer
    , events : List Event
    , competitions : List Competition
    }


type alias Data =
    { affData : AFFData
    , searchData : Maybe SearchData
    }


type BoutType
    = Pool
    | DE


type alias RawBout =
    { winnerID : String
    , loserID : String
    , winnerScore : Int
    , loserScore : Int
    , boutType : BoutType
    , eventID : String
    , competitionID : String
    }


type alias Fencer =
    { id : String
    , fencerName : String
    }


type alias Event =
    { id : String
    , eventName : String
    }


type alias Competition =
    { id : String
    , competitionName : String
    }


decodeData : D.Decoder Data
decodeData =
    D.map2 Data
        (D.map4 AFFData
            (D.field "bouts"
                (D.list
                    (D.map7 RawBout
                        (D.map String.fromInt (D.field "w" D.int))
                        (D.map String.fromInt (D.field "l" D.int))
                        (D.field "1" D.int)
                        (D.field "2" D.int)
                        (D.map intToBoutType (D.field "t" D.int))
                        (D.map String.fromInt (D.field "e" D.int))
                        (D.map String.fromInt (D.field "c" D.int))
                    )
                )
            )
            (D.field "fencers"
                (D.map keyPairToFencerList
                    (D.keyValuePairs D.int)
                )
            )
            (D.field "events"
                (D.map keyPairToEventList
                    (D.keyValuePairs D.int)
                )
            )
            (D.field "competitions"
                (D.map keyPairToCompetitionList
                    (D.keyValuePairs D.int)
                )
            )
        )
        (D.map initSearchData (D.field "searchIndex" D.value))


keyPairToFencerList : List ( a, Int ) -> List { id : String, fencerName : a }
keyPairToFencerList list =
    List.map (\( name, id ) -> { id = String.fromInt id, fencerName = name }) list


keyPairToEventList : List ( a, Int ) -> List { id : String, eventName : a }
keyPairToEventList list =
    List.map (\( name, id ) -> { id = String.fromInt id, eventName = name }) list


keyPairToCompetitionList : List ( a, Int ) -> List { id : String, competitionName : a }
keyPairToCompetitionList list =
    List.map (\( name, id ) -> { id = String.fromInt id, competitionName = name }) list


intToBoutType : number -> BoutType
intToBoutType int =
    if int == 0 then
        DE

    else
        Pool


init : String -> ( Model, Cmd Msg )
init data =
    case D.decodeString decodeData data of
        Ok result ->
            case result.searchData of
                Just searchData ->
                    ( Loaded result.affData (SingleFencer Nothing) searchData
                    , Cmd.none
                    )

                Nothing ->
                    ( SearchIndexFailed
                    , Cmd.none
                    )

        Err error ->
            ( DecodeFailed error
            , Cmd.none
            )


initSearchData : D.Value -> Maybe SearchData
initSearchData index =
    let
        maybeSearchIndex =
            ElmTextSearch.fromValue searchConfig index
    in
    case maybeSearchIndex of
        Ok result ->
            Just
                { searchIndex = result
                , searchText = ""
                , searchResult = SearchNone
                , focused = False
                }

        Err _ ->
            Nothing


searchConfig : ElmTextSearch.SimpleConfig Fencer
searchConfig =
    { ref = .id
    , fields = [ ( .fencerName, 1.0 ) ]
    , listFields = []
    }


createNewSearchIndex : ElmTextSearch.Index Fencer
createNewSearchIndex =
    ElmTextSearch.new
        { ref = .id
        , fields = [ ( .fencerName, 1.0 ) ]
        , listFields = []
        }



-- UPDATE


type Msg
    = NoOp
    | UpdateSearchText String
    | FencerSelected String
    | FocusSearchBar
    | UnFocusSearchBar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loaded data statsMode searchData ->
            case msg of
                UpdateSearchText text ->
                    case ElmTextSearch.search text searchData.searchIndex of
                        Ok ( newSearchIndex, result ) ->
                            ( Loaded data statsMode { searchIndex = newSearchIndex, searchText = text, searchResult = SearchSuccess result, focused = True }, Cmd.none )

                        Err error ->
                            ( Loaded data statsMode { searchIndex = searchData.searchIndex, searchText = text, searchResult = SearchFailed error, focused = True }, Cmd.none )

                FencerSelected id ->
                    let
                        fencer =
                            getFencerWithId id data.fencers
                    in
                    case statsMode of
                        SingleFencer _ ->
                            ( Loaded data (SingleFencer fencer) searchData, Cmd.none )

                        HeadToHead _ _ ->
                            ( model, Cmd.none )

                FocusSearchBar ->
                    ( Loaded data statsMode { searchData | focused = True }, Cmd.none )

                UnFocusSearchBar ->
                    ( Loaded data statsMode { searchData | focused = False }, Cmd.none )

                NoOp ->
                    ( model, Cmd.none )

        DecodeFailed _ ->
            ( model, Cmd.none )

        SearchIndexFailed ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        case model of
            Loaded data statsMode searchData ->
                div [ css [ displayFlex, flexDirection column, alignItems center, fontFamilies [ "Verdana", "Arial" ], textAlign center ] ]
                    [ div [ css [ Css.width (vw 33) ] ]
                        [ input [ value searchData.searchText, onInput UpdateSearchText, css [ Css.width (pct 100) ], onFocus FocusSearchBar, onBlur UnFocusSearchBar ] []
                        , case searchData.searchResult of
                            SearchSuccess result ->
                                if searchData.focused then
                                    div [ css [ position absolute, Css.width (pct 100), displayFlex, alignItems center, flexDirection column, borderLeft3 (px 2) solid (hex "#36454F"), borderRight3 (px 2) solid (hex "#36454F"), borderBottom3 (px 2) solid (hex "#36454F"), Css.width (vw 33) ] ] (List.map (\( id, score ) -> div [ onMouseDown (FencerSelected id), css [ cursor pointer, backgroundColor (hex "#E5E4E2"), paddingTop (px 4), paddingBottom (px 4), borderTop3 (px 2) solid (hex "#36454F"), borderLeft3 (px 2) solid (hex "#36454F"), borderRight3 (px 2) solid (hex "#36454F"), Css.width (pct 100) ] ] [ div [ css [ textAlign left, paddingLeft (px 4) ] ] [ p [] [ text (getFencerNameAtIndex id data.fencers) ] ] ]) <| List.take 6 result)

                                else
                                    text ""

                            SearchNone ->
                                text ""

                            SearchFailed error ->
                                text ""
                        ]
                    , case statsMode of
                        SingleFencer maybeFencer ->
                            case maybeFencer of
                                Just fencer ->
                                    let
                                        boutsWon =
                                            List.filter (\bout -> fencer.id == bout.winnerID) data.bouts

                                        boutsLost =
                                            List.filter (\bout -> fencer.id == bout.loserID) data.bouts

                                        boutsAll =
                                            boutsLost ++ boutsWon
                                    in
                                    div []
                                        [ h1 [] [ text fencer.fencerName ]
                                        , div [] [ h3 [] [ text <| String.fromInt (List.length boutsAll) ++ " Bouts Fenced 🤺" ] ]
                                        , div [] [ h3 [] [ text <| String.fromInt (List.length boutsWon) ++ " Bouts Won 🏆" ] ]
                                        , div [] [ h3 [] [ text <| String.fromInt (List.length boutsLost) ++ " Bouts Lost 😕" ] ]
                                        , div [] [ h3 [] [ text <| String.fromInt (Set.size <| Set.fromList <| List.map (\bout -> ( bout.eventID, bout.competitionID )) boutsAll) ++ " Events 🚀" ] ]
                                        , div [] [ h3 [] [ text <| String.fromInt (Set.size <| Set.fromList <| List.map .competitionID boutsAll) ++ " Competitions 📅" ] ]
                                        , div [ css [ displayFlex, alignItems center, justifyContent center ] ]
                                            [ div [] [ h3 [] [ text "Most Victories Over" ], div [] <| List.map (\( id, _ ) -> nameBadgeGreen <| getFencerWithId id data.fencers) <| List.take 3 <| List.sortBy (\tuple -> Tuple.second tuple) <| Dict.toList <| countFencers <| List.map (\bout -> bout.loserID) <| boutsWon ]
                                            , div [] [ h3 [] [ text "Most Defeated By" ], div [] <| List.map (\( id, _ ) -> nameBadgeRed <| getFencerWithId id data.fencers) <| List.take 3 <| List.sortBy (\tuple -> Tuple.second tuple) <| Dict.toList <| countFencers <| List.map (\bout -> bout.winnerID) <| boutsLost ]
                                            ]
                                        , div [ css [ Css.width (vw 30),Css.height (vw 10), margin (px 60)] ] [ indicatorChart boutsWon boutsLost data.competitions ]
                                        , div [] <| List.map (\bout -> viewBout bout data) <| List.filter (\bout -> fencer.id == bout.winnerID || fencer.id == bout.loserID) data.bouts

                                        --, div [] <| List.map (\competition -> text <| String.fromInt <| competitonToYear competition) data.competitions
                                        ]

                                Nothing ->
                                    text ""

                        HeadToHead _ _ ->
                            text ""
                    ]

            DecodeFailed error ->
                text (D.errorToString error)

            SearchIndexFailed ->
                text ""


competitonToYear : Competition -> Maybe Int
competitonToYear comp =
    let
        yearAndName =
            String.split " - " comp.competitionName
    in
    if List.length yearAndName == 2 then
        let
            maybeYearInt =
                Maybe.andThen String.toInt <| List.head yearAndName
        in
        case maybeYearInt of
            Just yearInt ->
                Just yearInt

            Nothing ->
                Nothing

    else
        Nothing


indicatorChart : List RawBout -> List RawBout -> List Competition -> Html msg
indicatorChart boutsWon boutsLost comps =
    Html.Styled.fromUnstyled <|
        C.chart
            [ CA.height 100
            , CA.width 300
            , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
            ]
            [ C.xLabels [ CA.fontSize 8]
            , C.xLabel [ CA.fontSize 11, CA.moveDown 10] [ Svg.text "Year" ]
            , C.yLabels [ CA.withGrid, CA.fontSize 8 ]
            , C.yLabel [ CA.fontSize 11, CA.moveLeft 20, CA.rotate 90, CA.alignMiddle ] [ Svg.text "Surplus Points" ]
            , C.series .x
                [ C.interpolated .y [] [ CA.circle, CA.size 10  ]
                ]
                (accumulatePrevious <| List.map2 (\( x1, pointsScored ) ( x2, pointsLost ) -> { x = toFloat x1, y = toFloat (pointsScored - pointsLost) }) (Dict.toList <| countIndicatorPerYear boutsWon comps) (Dict.toList <| countIndicatorPerYear boutsLost comps))
            ]


accumulatePrevious : List { x : Float, y : Float } -> List { x : Float, y : Float }
accumulatePrevious list =
    List.indexedMap (\i data -> { x = data.x, y = List.sum (List.take i (List.map .y list)) }) list


nameBadgeElement : ColorValue compatible -> List (Attribute msg) -> List (Html msg) -> Html msg
nameBadgeElement color =
    styled div
        [ backgroundColor color
        , Css.width (vw 10)
        , borderRadius (px 20)
        , margin (px 10)
        , fontFamilies [ "Verdana", "Arial" ]
        , padding (px 5)
        , textAlign center
        , Css.color (hex "#342B40")
        ]


nameBadge : ColorValue compatible -> Maybe Fencer -> Html Msg
nameBadge color maybeFencer =
    case maybeFencer of
        Just fencer ->
            nameBadgeElement color [] [ text fencer.fencerName ]

        Nothing ->
            text "Fencer Not Found :("


nameBadgeGreen =
    nameBadge (hex "#54B84C")


nameBadgeRed =
    nameBadge (hex "#FD7B83")


getFencerName : Maybe Fencer -> String
getFencerName maybeFencer =
    case maybeFencer of
        Just fencer ->
            fencer.fencerName

        Nothing ->
            ""


getFencerNameAtIndex : String -> List Fencer -> String
getFencerNameAtIndex id fencers =
    let
        names =
            List.filter (\fencer -> fencer.id == id) fencers
    in
    case names of
        [] ->
            ""

        x :: xs ->
            x.fencerName


getFencerWithId : String -> List Fencer -> Maybe Fencer
getFencerWithId id fencers =
    let
        filtered =
            List.filter (\fencer -> fencer.id == id) fencers
    in
    case filtered of
        [] ->
            Nothing

        x :: _ ->
            Just x


viewBout : RawBout -> AFFData -> Html Msg
viewBout { winnerID, loserID, winnerScore, loserScore, boutType, eventID, competitionID } data =
    div []
        [ p [] [ text <| getFencerNameAtIndex winnerID data.fencers ++ " " ++ String.fromInt winnerScore ++ ":" ++ String.fromInt loserScore ++ " " ++ getFencerNameAtIndex loserID data.fencers ]
        , p [] [ text <| getCompetitionName <| getCompetition competitionID data.competitions ]
        ]


getCompetitionName : Maybe Competition -> String
getCompetitionName maybeComp =
    case maybeComp of
        Just comp ->
            comp.competitionName

        Nothing ->
            "Comp not found"


getCompetition : String -> List Competition -> Maybe Competition
getCompetition id competitions =
    let
        filtered =
            List.filter (\competition -> competition.id == id) competitions
    in
    case filtered of
        [] ->
            Nothing

        x :: _ ->
            Just x


viewFencers : Fencer -> Html Msg
viewFencers { id, fencerName } =
    p [] [ text fencerName ]


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


countBoutsPerYear : List RawBout -> List Competition -> Dict.Dict Int Int
countBoutsPerYear bouts comps =
    bouts
        |> List.foldr
            (\bout carry ->
                Dict.update
                    (Maybe.withDefault 2010 <| Maybe.andThen competitonToYear <| (\id -> getCompetition id comps) <| bout.competitionID)
                    (\existingCount ->
                        case existingCount of
                            Just count ->
                                Just (count + 1)

                            Nothing ->
                                Just 0
                    )
                    carry
            )
            Dict.empty


countIndicatorPerYear : List RawBout -> List Competition -> Dict.Dict Int Int
countIndicatorPerYear bouts comps =
    bouts
        |> List.foldr
            (\bout carry ->
                Dict.update
                    (Maybe.withDefault 2010 <| Maybe.andThen competitonToYear <| (\id -> getCompetition id comps) <| bout.competitionID)
                    (\existingCount ->
                        case existingCount of
                            Just count ->
                                Just (count + (bout.winnerScore - bout.loserScore))

                            Nothing ->
                                Just 0
                    )
                    carry
            )
            Dict.empty



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
