module SearchBar exposing (SearchData, SearchResult(..), initSearchData, updateSearchText, viewSearchBar)

import Css exposing (absolute, alignItems, backgroundColor, borderBottom3, borderLeft3, borderRight3, borderTop3, center, column, cursor, displayFlex, flexDirection, hex, left, minWidth, paddingBottom, paddingLeft, paddingTop, pct, pointer, position, px, solid, textAlign, vw)
import ElmTextSearch
import Fencer exposing (Fencer, getFencerNameAtIndex)
import Html.Styled exposing (Html, div, input, p, text)
import Html.Styled.Attributes exposing (css, tabindex, value)
import Html.Styled.Events exposing (onBlur, onFocus, onInput, onMouseDown)
import Json.Decode as D


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


initSearchData : D.Value -> Maybe SearchData
initSearchData index =
    let
        maybeSearchIndex : Result D.Error (ElmTextSearch.Index Fencer)
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


viewSearchBar : SearchData -> List Fencer -> (String -> msg) -> (String -> msg) -> msg -> msg -> Html msg
viewSearchBar searchData fencers onMouseDownMessage updateSearchMessage focusSearch unFocusSearch =
    div []
        [ input [ value searchData.searchText, onInput updateSearchMessage, css [ Css.width (pct 100) ], onFocus focusSearch, onBlur unFocusSearch ] []
        , case searchData.searchResult of
            SearchSuccess result ->
                if searchData.focused then
                    div
                        [ css [ position absolute, Css.width (pct 100), displayFlex, alignItems center, flexDirection column, borderLeft3 (px 2) solid (hex "#36454F"), borderRight3 (px 2) solid (hex "#36454F"), borderBottom3 (px 2) solid (hex "#36454F"), Css.width (vw 33), minWidth (px 900) ] ]
                        (List.map
                            (\( id, _ ) -> viewSearchResult (onMouseDownMessage id) (getFencerNameAtIndex id fencers))
                            (List.take 6 result)
                        )

                else
                    text ""

            SearchNone ->
                text ""

            SearchFailed _ ->
                text ""
        ]


viewSearchResult : msg -> String -> Html msg
viewSearchResult clickMsg fencerName =
    div
        [ onMouseDown clickMsg, css [ cursor pointer, backgroundColor (hex "#E5E4E2"), paddingTop (px 4), paddingBottom (px 4), borderTop3 (px 2) solid (hex "#36454F"), borderLeft3 (px 2) solid (hex "#36454F"), borderRight3 (px 2) solid (hex "#36454F"), Css.width (pct 100) ] ]
        [ div [ tabindex 0, css [ textAlign left, paddingLeft (px 4) ] ] [ p [] [ text fencerName ] ] ]


updateSearchText : String -> SearchData -> SearchData
updateSearchText text searchData =
    case ElmTextSearch.search text searchData.searchIndex of
        Ok ( newSearchIndex, result ) ->
            { searchIndex = newSearchIndex, searchText = text, searchResult = SearchSuccess result, focused = True }

        Err error ->
            { searchIndex = searchData.searchIndex, searchText = text, searchResult = SearchFailed error, focused = True }
