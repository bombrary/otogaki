module Data.DrumPattern exposing (Pattern, apply, byName, patterns)

import Data.Project exposing (Project)
import Data.Time


type alias Hit =
    { pitch : Int
    , step : Int
    , velocity : Int
    }


type alias Pattern =
    { name : String
    , hits : List Hit
    }


hitsAt : Int -> List ( Int, Int ) -> List Hit
hitsAt pitch stepVels =
    List.map (\( s, v ) -> { pitch = pitch, step = s, velocity = v }) stepVels


patterns : List Pattern
patterns =
    [ { name = "8ビート"
      , hits =
            hitsAt 36 [ ( 0, 118 ), ( 8, 108 ) ]
                ++ hitsAt 38 [ ( 4, 112 ), ( 12, 112 ) ]
                ++ hitsAt 42 [ ( 0, 104 ), ( 2, 72 ), ( 4, 92 ), ( 6, 72 ), ( 8, 96 ), ( 10, 72 ), ( 12, 92 ), ( 14, 72 ) ]
      }
    , { name = "16ビート"
      , hits =
            hitsAt 36 [ ( 0, 118 ), ( 7, 94 ), ( 10, 104 ) ]
                ++ hitsAt 38 [ ( 4, 112 ), ( 12, 110 ) ]
                ++ hitsAt 42
                    [ ( 0, 106 ), ( 1, 62 ), ( 2, 82 ), ( 3, 62 )
                    , ( 4, 100 ), ( 5, 62 ), ( 6, 82 ), ( 7, 62 )
                    , ( 8, 100 ), ( 9, 62 ), ( 10, 82 ), ( 11, 62 )
                    , ( 12, 100 ), ( 13, 62 ), ( 14, 82 ), ( 15, 62 )
                    ]
      }
    , { name = "4つ打ち"
      , hits =
            hitsAt 36 [ ( 0, 116 ), ( 4, 110 ), ( 8, 112 ), ( 12, 110 ) ]
                ++ hitsAt 38 [ ( 4, 104 ), ( 12, 104 ) ]
                ++ hitsAt 46 [ ( 2, 90 ), ( 6, 86 ), ( 10, 90 ), ( 14, 86 ) ]
      }
    , { name = "バラード"
      , hits =
            hitsAt 36 [ ( 0, 104 ), ( 10, 86 ) ]
                ++ hitsAt 38 [ ( 8, 98 ) ]
                ++ hitsAt 51 [ ( 0, 88 ), ( 4, 76 ), ( 8, 84 ), ( 12, 78 ) ]
      }
    ]


byName : String -> Maybe Pattern
byName name =
    patterns
        |> List.filter (\p -> p.name == name)
        |> List.head


apply : { trackId : Int, startTicks : Int, endTicks : Int } -> Pattern -> Project -> Project
apply cfg pattern project =
    let
        bars =
            Basics.max 1 ((cfg.endTicks - cfg.startTicks) // Data.Time.ticksPerBar)

        allHits =
            List.range 0 (bars - 1)
                |> List.concatMap
                    (\bar ->
                        List.map
                            (\hit ->
                                ( hit.pitch
                                , cfg.startTicks
                                    + bar
                                    * Data.Time.ticksPerBar
                                    + hit.step
                                    * Data.Time.ticksPerSixteenth
                                , hit.velocity
                                )
                            )
                            pattern.hits
                    )

        ( newNotesRev, finalNextId ) =
            List.foldl
                (\( pitch, start, velocity ) ( acc, nid ) ->
                    ( { id = nid
                      , pitch = pitch
                      , start = start
                      , duration = Data.Time.ticksPerSixteenth
                      , velocity = velocity
                      }
                        :: acc
                    , nid + 1
                    )
                )
                ( [], project.nextId )
                allHits

        cleaned =
            Data.Project.mapNotes cfg.trackId
                (\existing ->
                    List.filter (\n -> n.start < cfg.startTicks || n.start >= cfg.endTicks) existing
                        ++ List.reverse newNotesRev
                )
                project
    in
    { cleaned | nextId = finalNextId }
