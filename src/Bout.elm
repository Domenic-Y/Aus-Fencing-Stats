module Bout exposing (BoutType(..), Bout, intToBoutType)


type alias Bout =
    { winnerID : String
    , loserID : String
    , winnerScore : Int
    , loserScore : Int
    , boutType : BoutType
    , eventID : String
    , competitionID : String
    }


type BoutType
    = Pool
    | DE


intToBoutType : number -> BoutType
intToBoutType int =
    if int == 0 then
        DE

    else
        Pool
