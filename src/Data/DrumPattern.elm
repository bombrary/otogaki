module Data.DrumPattern exposing (Pattern, apply, byName, patterns)

import Data.Project exposing (Project)
import Data.Time


type alias Hit =
    { pitch : Int
    , step : Int
    }


type alias Pattern =
    { name : String
    , hits : List Hit
    }


hitsOf : Int -> List Int -> List Hit
hitsOf pitch steps =
    List.map (\s -> { pitch = pitch, step = s }) steps


patterns : List Pattern
patterns =
    [ { name = "8ビート"
      , hits = hitsOf 36 [ 0, 8 ] ++ hitsOf 38 [ 4, 12 ] ++ hitsOf 42 [ 0, 2, 4, 6, 8, 10, 12, 14 ]
      }
    , { name = "16ビート"
      , hits = hitsOf 36 [ 0, 7, 10 ] ++ hitsOf 38 [ 4, 12 ] ++ hitsOf 42 (List.range 0 15)
      }
    , { name = "4つ打ち"
      , hits = hitsOf 36 [ 0, 4, 8, 12 ] ++ hitsOf 38 [ 4, 12 ] ++ hitsOf 46 [ 2, 6, 10, 14 ]
      }
    , { name = "バラード"
      , hits = hitsOf 36 [ 0, 10 ] ++ hitsOf 38 [ 8 ] ++ hitsOf 51 [ 0, 4, 8, 12 ]
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
                                )
                            )
                            pattern.hits
                    )

        ( newNotesRev, finalNextId ) =
            List.foldl
                (\( pitch, start ) ( acc, nid ) ->
                    ( { id = nid
                      , pitch = pitch
                      , start = start
                      , duration = Data.Time.ticksPerSixteenth
                      , velocity = 100
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
