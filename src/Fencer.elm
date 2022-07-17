module Fencer exposing (Fencer, getFencerNameAtIndex, getFencerWithId)


type alias Fencer =
    { id : String
    , fencerName : String
    }


getFencerWithId : String -> List Fencer -> Maybe Fencer
getFencerWithId id fencers =
    let
        filtered : List Fencer
        filtered =
            List.filter (\fencer -> fencer.id == id) fencers
    in
    case filtered of
        [] ->
            Nothing

        x :: _ ->
            Just x


getFencerNameAtIndex : String -> List Fencer -> String
getFencerNameAtIndex id fencers =
    let
        filteredFencer : List Fencer
        filteredFencer =
            List.filter (\fencer -> fencer.id == id) fencers
    in
    case filteredFencer of
        [] ->
            ""

        x :: _ ->
            x.fencerName
