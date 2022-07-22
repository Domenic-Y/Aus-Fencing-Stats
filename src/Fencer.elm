module Fencer exposing (Fencer, getFencerNameAtIndex, getFencerWithId, firstNameLastName)


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
            firstNameLastName x

firstNameLastName : Fencer -> String
firstNameLastName fencer = 
    case String.split ", " fencer.fencerName of
        [] ->
            ""
        x1 :: [] ->
            x1
        x1 :: x2 :: [] ->
            x2 ++ " " ++ (String.join " " (List.map capitaliseLastName (String.split " " x1)))
        x1 :: x2 :: x3 :: xs ->
            x3 ++ x2 ++ x1 ++ (String.join " " xs)

capitaliseLastName : String -> String
capitaliseLastName lastName = 
  case String.uncons (String.toLower lastName) of
    Nothing -> ""
    Just (head, tail) -> 
      String.append (String.fromChar (Char.toUpper head)) tail