module Main exposing (Model, Msg, main)

import AFFData exposing (AFFData)
import Browser
import Css exposing (backgroundColor, hex, alignItems, auto, center, column, displayFlex, em, flexDirection, fontFamilies, fontSize, justifyContent, margin, maxWidth, minHeight, padding, pct, spaceAround, textAlign, width, color)
import Decoders exposing (decodeData)
import Fencer exposing (Fencer, getFencerWithId)
import HeadToHeadPage exposing (viewHeadToHeadPage)
import Html
import Html.Styled exposing (button, div, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as D
import SearchBar exposing (SearchData, updateSearchText, viewSearchBar)
import SingleFencerPage exposing (viewSingleFencerPage)



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



-- UPDATE


type Msg
    = UpdateSearchText String
    | FencerSelected String
    | FocusSearchBar
    | UnFocusSearchBar
    | SwitchToSingleStats
    | SwitchToHeadToHeadStats


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loaded data statsMode searchData ->
            case msg of
                UpdateSearchText text ->
                    ( Loaded data statsMode (updateSearchText text searchData), Cmd.none )

                FencerSelected id ->
                    let
                        fencer : Maybe Fencer
                        fencer =
                            getFencerWithId id data.fencers
                    in
                    case statsMode of
                        SingleFencer _ ->
                            ( Loaded data (SingleFencer fencer) searchData, Cmd.none )

                        HeadToHead maybeFencerLeft maybeFencerRight ->
                            case ( maybeFencerLeft, maybeFencerRight ) of
                                ( Nothing, Nothing ) ->
                                    ( Loaded data (HeadToHead fencer Nothing) searchData, Cmd.none )

                                ( Nothing, Just fencerRight ) ->
                                    ( Loaded data (HeadToHead fencer (Just fencerRight)) searchData, Cmd.none )

                                ( Just fencerLeft, Nothing ) ->
                                    ( Loaded data (HeadToHead (Just fencerLeft) fencer) searchData, Cmd.none )

                                ( Just fencerLeft, Just _ ) ->
                                    ( Loaded data (HeadToHead (Just fencerLeft) fencer) searchData, Cmd.none )

                FocusSearchBar ->
                    ( Loaded data statsMode { searchData | focused = True }, Cmd.none )

                UnFocusSearchBar ->
                    ( Loaded data statsMode { searchData | focused = False }, Cmd.none )

                SwitchToHeadToHeadStats ->
                    ( Loaded data (HeadToHead Nothing Nothing) searchData, Cmd.none )

                SwitchToSingleStats ->
                    ( Loaded data (SingleFencer Nothing) searchData, Cmd.none )

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
                div
                    [ css [ displayFlex, flexDirection column, alignItems center, fontFamilies [ "Verdana", "Arial" ], textAlign center, maxWidth (em 80), margin auto, color (hex "#e8e7e3") ] ]
                    [ div [ css [ displayFlex, justifyContent center, minHeight (em 2), width (pct 100), padding (em 1) ] ] [ button [ onClick SwitchToSingleStats, css buttonStyle ] [ text "Individual 🤺" ], button [ onClick SwitchToHeadToHeadStats, css buttonStyle ] [ text "Head to Head ⚔️" ] ]
                    , div
                        [ css [ Css.maxWidth (em 40) ] ]
                        [ viewSearchBar searchData data.fencers FencerSelected UpdateSearchText FocusSearchBar UnFocusSearchBar ]
                    , case statsMode of
                        SingleFencer maybeFencer ->
                            viewSingleFencerPage maybeFencer data

                        HeadToHead maybeFencerLeft maybeFencerRight ->
                            viewHeadToHeadPage maybeFencerLeft maybeFencerRight data
                    ]

            DecodeFailed error ->
                text (D.errorToString error)

            SearchIndexFailed ->
                text ""


buttonStyle : List Css.Style
buttonStyle =
    [ margin (em 1), padding (em 1), width (pct 30), fontSize (em 1.4), displayFlex, alignItems center, justifyContent center ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
