module SearchBar exposing (SearchData, SearchResult(..), initSearchData, updateSearchText, viewSearchBar)

import Css exposing (hover, absolute, color, margin, padding, alignItems, backgroundColor, borderBottom3, borderLeft3, borderRight3, borderTop3, center, column, cursor, displayFlex, flexDirection, hex, left, maxWidth, width, paddingBottom, paddingLeft, paddingTop, pct, pointer, position, px, solid, textAlign, maxWidth, em)
import ElmTextSearch
import Fencer exposing (Fencer, getFencerNameAtIndex)
import Html.Styled exposing (Html, div, input, p, text)
import Html.Styled.Attributes exposing (css, value, placeholder)
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
    div [css [color (hex ("#17181c")), width (em 40)]]
        [ input [ value searchData.searchText, placeholder "John Smith", onInput updateSearchMessage, css [ Css.width (pct 100), padding (em 1) ], onFocus focusSearch, onBlur unFocusSearch ] []
        , case searchData.searchResult of
            SearchSuccess result ->
                if searchData.focused then
                    div
                        [ css [ position absolute, Css.width (em 40), alignItems center, flexDirection column, maxWidth (em 60)] ]
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
        [ onMouseDown clickMsg, css [ hover [backgroundColor (hex "#d9d8d7")] ,cursor pointer, backgroundColor (hex "#fafafa"), paddingTop (px 4), paddingBottom (px 4) ] ]
        [ div [ css [ textAlign left, padding (em 1) ] ] [ p [css [margin (px 0)]] [ text fencerName ] ] ]


updateSearchText : String -> SearchData -> SearchData
updateSearchText text searchData =
    case ElmTextSearch.search text searchData.searchIndex of
        Ok ( newSearchIndex, result ) ->
            { searchIndex = newSearchIndex, searchText = text, searchResult = SearchSuccess result, focused = True }

        Err error ->
            { searchIndex = searchData.searchIndex, searchText = text, searchResult = SearchFailed error, focused = True }
