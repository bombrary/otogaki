module Data.Note exposing (Note)

import Data.Time exposing (Ticks)


type alias Note =
    { id : Int
    , pitch : Int
    , start : Ticks
    , duration : Ticks
    , velocity : Int
    }
