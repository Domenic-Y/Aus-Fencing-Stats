module Main exposing (Model, Msg, main)

import AFFData exposing (AFFData)
import Browser
import Css exposing (alignItems, auto, center, column, displayFlex, flexDirection, fontFamilies, margin, minWidth, pct, px, textAlign, vw)
import Decoders exposing (decodeData)
import Fencer exposing (Fencer, getFencerWithId)
import Html
import Html.Styled exposing (div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loaded data statsMode searchData ->
            case msg of
                UpdateSearchText text ->
                    ( Loaded data statsMode (updateSearchText text searchData), Cmd.none )

                FencerSelected id ->
                    case statsMode of
                        SingleFencer _ ->
                            let
                                fencer : Maybe Fencer
                                fencer =
                                    getFencerWithId id data.fencers
                            in
                            ( Loaded data (SingleFencer fencer) searchData, Cmd.none )

                        HeadToHead _ _ ->
                            ( model, Cmd.none )

                FocusSearchBar ->
                    ( Loaded data statsMode { searchData | focused = True }, Cmd.none )

                UnFocusSearchBar ->
                    ( Loaded data statsMode { searchData | focused = False }, Cmd.none )

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
                div [ css [ displayFlex, flexDirection column, alignItems center, fontFamilies [ "Verdana", "Arial" ], textAlign center, Css.width (vw 33), minWidth (px 900), margin auto ] ]
                    [ div [ css [ Css.width (pct 100) ] ]
                        [ viewSearchBar searchData data.fencers FencerSelected UpdateSearchText FocusSearchBar UnFocusSearchBar ]
                    , case statsMode of
                        SingleFencer maybeFencer ->
                            viewSingleFencerPage maybeFencer data

                        HeadToHead _ _ ->
                            text ""
                    ]

            DecodeFailed error ->
                text (D.errorToString error)

            SearchIndexFailed ->
                text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
