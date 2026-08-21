module Data.DrumPattern exposing (Lanes(..), Mode(..), Pattern, apply, byName, patterns)

import Data.Meter
import Data.Project exposing (Project)
import Data.Time
import Data.Timeline exposing (Timeline)
import Set exposing (Set)


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
    , { name = "シャッフル"
      , hits =
            hitsAt 36 [ ( 0, 116 ), ( 8, 104 ) ]
                ++ hitsAt 38 [ ( 4, 110 ), ( 12, 106 ) ]
                ++ hitsAt 42 [ ( 0, 100 ), ( 3, 70 ), ( 6, 90 ), ( 8, 100 ), ( 11, 70 ), ( 14, 90 ) ]
      }
    , { name = "ロック8"
      , hits =
            hitsAt 36 [ ( 0, 120 ), ( 8, 112 ) ]
                ++ hitsAt 38 [ ( 4, 114 ), ( 12, 110 ) ]
                ++ hitsAt 42 [ ( 0, 100 ), ( 2, 60 ), ( 4, 95 ), ( 6, 60 ), ( 8, 100 ), ( 10, 60 ), ( 12, 95 ), ( 14, 60 ) ]
      }
    , { name = "ファンク16"
      , hits =
            hitsAt 36 [ ( 0, 118 ), ( 3, 88 ), ( 6, 96 ), ( 10, 84 ) ]
                ++ hitsAt 38 [ ( 4, 112 ), ( 12, 108 ), ( 14, 40 ) ]
                ++ hitsAt 42 [ ( 0, 92 ), ( 2, 55 ), ( 4, 80 ), ( 6, 55 ), ( 8, 90 ), ( 10, 55 ), ( 12, 80 ), ( 14, 55 ) ]
      }
    , { name = "ボサノバ"
      , hits =
            hitsAt 36 [ ( 0, 112 ), ( 3, 82 ), ( 6, 96 ), ( 10, 86 ) ]
                ++ hitsAt 42 [ ( 0, 72 ), ( 2, 54 ), ( 4, 72 ), ( 6, 54 ), ( 8, 72 ), ( 10, 54 ), ( 12, 72 ), ( 14, 54 ) ]
      }
    , { name = "キック: 4つ打ち"
      , hits = hitsAt 36 [ ( 0, 116 ), ( 4, 100 ), ( 8, 116 ), ( 12, 100 ) ]
      }
    , { name = "ハット: 8分"
      , hits = hitsAt 42 [ ( 0, 100 ), ( 2, 70 ), ( 4, 100 ), ( 6, 70 ), ( 8, 100 ), ( 10, 70 ), ( 12, 100 ), ( 14, 70 ) ]
      }
    , { name = "スネア: 2・4"
      , hits = hitsAt 38 [ ( 4, 112 ), ( 12, 100 ) ]
      }
    , { name = "フィル: タム下り"
      , hits =
            hitsAt 48 [ ( 8, 110 ) ]
                ++ hitsAt 45 [ ( 10, 106 ) ]
                ++ hitsAt 41 [ ( 12, 102 ) ]
                ++ hitsAt 38 [ ( 14, 120 ) ]
      }
    ]


byName : String -> Maybe Pattern
byName name =
    patterns
        |> List.filter (\p -> p.name == name)
        |> List.head


{-| 適用時に既存ノートをどう扱うか。既定は `ReplaceLanes`：パターンが書き込む pitch の行だけを
範囲内で消してから書くので、同じ適用を何度押しても結果が変わらない（冪等）。
-}
type Mode
    = ReplaceLanes
    | Merge
    | Replace


{-| パターンのどの行（pitch）を書き込むか。
-}
type Lanes
    = AllLanes
    | OnlyLanes (Set Int)


inLanes : Lanes -> Int -> Bool
inLanes lanes pitch =
    case lanes of
        AllLanes ->
            True

        OnlyLanes pitches ->
            Set.member pitch pitches


{-| パターンを敷く1周分（開始 tick とその長さ）。長さはその位置の拍子（`Data.Timeline.meterAt`）から求めるので、
3/4 や 6/8 のセクションでは 4/4 より短い周になる。最後の周は `endTicks` で切り詰められる。
-}
type alias Cycle =
    { start : Int, length : Int }


cycles : Timeline -> Int -> Int -> List Cycle
cycles timeline startTicks endTicks =
    cyclesHelp timeline startTicks endTicks 0 []


cyclesHelp : Timeline -> Int -> Int -> Int -> List Cycle -> List Cycle
cyclesHelp timeline startTicks endTicks guard acc =
    if startTicks >= endTicks || guard >= 4096 then
        List.reverse acc

    else
        let
            barLength =
                Data.Meter.ticksPerBar (Data.Timeline.meterAt startTicks timeline)

            cycleLength =
                Basics.min barLength (endTicks - startTicks)
        in
        if barLength <= 0 then
            List.reverse acc

        else
            cyclesHelp timeline (startTicks + barLength) endTicks (guard + 1) ({ start = startTicks, length = cycleLength } :: acc)


apply :
    { trackId : Int
    , startTicks : Int
    , endTicks : Int
    , mode : Mode
    , lanes : Lanes
    }
    -> Pattern
    -> Project
    -> Project
apply cfg pattern project =
    let
        activeHits =
            List.filter (\h -> inLanes cfg.lanes h.pitch) pattern.hits

        touchedPitches =
            activeHits |> List.map .pitch |> Set.fromList

        placedHits =
            cycles (Data.Project.timeline project) cfg.startTicks cfg.endTicks
                |> List.concatMap
                    (\cycle ->
                        activeHits
                            |> List.filterMap
                                (\h ->
                                    let
                                        tick =
                                            cycle.start + h.step * Data.Time.ticksPerSixteenth
                                    in
                                    -- サイクルの長さ（その拍子の小節長、末尾は endTicks で切り詰められている）を超える
                                    -- ステップは落とす。例：3/4（12ステップ）なら step 12〜15 はここで消える。
                                    if tick < cycle.start + cycle.length then
                                        Just { pitch = h.pitch, start = tick, velocity = h.velocity }

                                    else
                                        Nothing
                                )
                    )

        existing =
            Data.Project.notesOf cfg.trackId project

        inRange n =
            n.start >= cfg.startTicks && n.start < cfg.endTicks

        kept =
            case cfg.mode of
                Replace ->
                    List.filter (\n -> not (inRange n)) existing

                ReplaceLanes ->
                    List.filter (\n -> not (inRange n && Set.member n.pitch touchedPitches)) existing

                Merge ->
                    existing

        occupied =
            kept |> List.map (\n -> ( n.pitch, n.start )) |> Set.fromList

        fresh =
            placedHits |> List.filter (\p -> not (Set.member ( p.pitch, p.start ) occupied))

        ( newNotesRev, finalNextId ) =
            List.foldl
                (\p ( acc, nid ) ->
                    ( { id = nid
                      , pitch = p.pitch
                      , start = p.start
                      , duration = Data.Time.ticksPerSixteenth
                      , velocity = p.velocity
                      }
                        :: acc
                    , nid + 1
                    )
                )
                ( [], project.nextId )
                fresh
    in
    if List.isEmpty fresh && List.length kept == List.length existing then
        -- 何も変わらないなら project をそのまま返す（undo スタックに空エントリを積まない）。
        project

    else
        { project | nextId = finalNextId }
            |> Data.Project.mapNotes cfg.trackId (\_ -> kept ++ List.reverse newNotesRev)
