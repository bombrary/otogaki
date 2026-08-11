module Data.StrumPattern exposing (Direction(..), Pattern, Pick(..), Strum, byName, patterns)


type Direction
    = Down
    | Up


type Pick
    = AllStrings
    | StringIndex Int


type alias Strum =
    { step : Int
    , direction : Direction
    , velocity : Int
    , pick : Pick
    }


type alias Pattern =
    { name : String
    , strums : List Strum
    }


strum : Int -> Direction -> Int -> Strum
strum step direction velocity =
    { step = step, direction = direction, velocity = velocity, pick = AllStrings }


pickStrum : Int -> Int -> Int -> Strum
pickStrum step index velocity =
    { step = step, direction = Down, velocity = velocity, pick = StringIndex index }


patterns : List Pattern
patterns =
    [ { name = "8ビート"
      , strums =
            [ strum 0 Down 100
            , strum 4 Down 90
            , strum 6 Up 70
            , strum 10 Up 70
            , strum 12 Down 95
            , strum 14 Up 70
            ]
      }
    , { name = "16ビート"
      , strums =
            [ strum 0 Down 100
            , strum 4 Down 90
            , strum 6 Down 85
            , strum 7 Up 70
            , strum 9 Up 70
            , strum 10 Down 85
            , strum 12 Down 90
            , strum 14 Down 85
            , strum 15 Up 70
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
            List.range 0 15
                |> List.map
                    (\i ->
                        pickStrum i i
                            (if modBy 4 i == 0 then
                                90

                             else
                                80
                            )
                    )
      }
    , { name = "全音符ストローク"
      , strums = [ strum 0 Down 100 ]
      }
    ]


byName : String -> Maybe Pattern
byName name =
    patterns |> List.filter (\p -> p.name == name) |> List.head
