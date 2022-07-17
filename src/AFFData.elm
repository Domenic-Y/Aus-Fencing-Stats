module AFFData exposing (AFFData)

import Bout exposing (Bout)
import Competition exposing (Competition)
import Event exposing (Event)
import Fencer exposing (Fencer)


type alias AFFData =
    { bouts : List Bout
    , fencers : List Fencer
    , events : List Event
    , competitions : List Competition
    }
