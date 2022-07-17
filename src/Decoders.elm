module Decoders exposing (Data, decodeData)

import AFFData exposing (AFFData)
import Bout exposing (Bout, intToBoutType)
import Fencer exposing (Fencer)
import Json.Decode as D
import SearchBar exposing (SearchData, initSearchData)


decodeData : D.Decoder Data
decodeData =
    D.map2 Data
        (D.map4 AFFData
            decodeBouts
            decodeFencer
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
        decodeSearchData


decodeBouts : D.Decoder (List Bout)
decodeBouts =
    D.field "bouts"
        (D.list
            (D.map7 Bout
                (D.map String.fromInt (D.field "w" D.int))
                (D.map String.fromInt (D.field "l" D.int))
                (D.field "1" D.int)
                (D.field "2" D.int)
                (D.map intToBoutType (D.field "t" D.int))
                (D.map String.fromInt (D.field "e" D.int))
                (D.map String.fromInt (D.field "c" D.int))
            )
        )


decodeFencer : D.Decoder (List Fencer)
decodeFencer =
    D.field "fencers"
        (D.map keyPairToFencerList
            (D.keyValuePairs D.int)
        )


keyPairToFencerList : List ( a, Int ) -> List { id : String, fencerName : a }
keyPairToFencerList list =
    List.map (\( name, id ) -> { id = String.fromInt id, fencerName = name }) list


decodeSearchData : D.Decoder (Maybe SearchData)
decodeSearchData =
    D.map initSearchData (D.field "searchIndex" D.value)


keyPairToEventList : List ( a, Int ) -> List { id : String, eventName : a }
keyPairToEventList list =
    List.map (\( name, id ) -> { id = String.fromInt id, eventName = name }) list


keyPairToCompetitionList : List ( a, Int ) -> List { id : String, competitionName : a }
keyPairToCompetitionList list =
    List.map (\( name, id ) -> { id = String.fromInt id, competitionName = name }) list


type alias Data =
    { affData : AFFData
    , searchData : Maybe SearchData
    }
