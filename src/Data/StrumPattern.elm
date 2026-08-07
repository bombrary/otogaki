module Data.StrumPattern exposing (Direction(..), Pattern, Strum, byName, patterns)


type Direction
    = Down
    | Up


type alias Strum =
    { step : Int
    , direction : Direction
    , velocity : Int
    }


type alias Pattern =
    { name : String
    , strums : List Strum
    }


strum : Int -> Direction -> Int -> Strum
strum step direction velocity =
    { step = step, direction = direction, velocity = velocity }


patterns : List Pattern
patterns =
    [ { name = "8ビート"
      , strums =
            [ strum 0 Down 100
            , strum 4 Up 70
            , strum 6 Down 90
            , strum 8 Down 100
            , strum 12 Up 70
            , strum 14 Down 90
            ]
      }
    , { name = "フォーク"
      , strums =
            [ strum 0 Down 100
            , strum 2 Down 80
            , strum 4 Up 70
            , strum 6 Up 70
            , strum 8 Down 90
            , strum 10 Down 80
            , strum 12 Up 70
            , strum 14 Up 70
            ]
      }
    , { name = "アルペジオ（分散）"
      , strums =
            [ strum 0 Down 90
            , strum 4 Down 80
            , strum 8 Down 85
            , strum 12 Down 80
            ]
      }
    , { name = "全音符ストローク"
      , strums = [ strum 0 Down 100 ]
      }
    ]


byName : String -> Maybe Pattern
byName name =
    patterns |> List.filter (\p -> p.name == name) |> List.head
