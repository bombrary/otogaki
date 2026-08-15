module ProjectJsonTest exposing (suite)

import Codec.ProjectJson as ProjectJson
import Data.Project
import Data.Track
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Codec.ProjectJson"
        [ test "demo を encode → decode すると元の Project と一致する" <|
            \_ ->
                Data.Project.demo
                    |> ProjectJson.encode
                    |> Decode.decodeValue ProjectJson.decoder
                    |> Expect.equal (Ok Data.Project.demo)
        , test "v1.0 形式（bpm が整数、memo/key/meter が欠損）も既定値で読める" <|
            \_ ->
                let
                    legacyJson =
                        Encode.object
                            [ ( "version", Encode.int 1 )
                            , ( "name", Encode.string "旧曲" )
                            , ( "bpm", Encode.int 120 )
                            , ( "tracks", Encode.list identity [] )
                            , ( "sections"
                              , Encode.list identity
                                    [ Encode.object
                                        [ ( "id", Encode.int 1 )
                                        , ( "name", Encode.string "Aメロ" )
                                        , ( "lengthBars", Encode.int 4 )
                                        , ( "memo", Encode.string "" )
                                        ]
                                    ]
                              )
                            , ( "nextId", Encode.int 1 )
                            ]
                in
                case Decode.decodeValue ProjectJson.decoder legacyJson of
                    Ok project ->
                        Expect.all
                            [ \p -> Expect.within (Expect.Absolute 0.0001) 120.0 p.bpm
                            , \p -> Expect.equal "" p.memo
                            , \p -> Expect.equal 0 (List.head p.sections |> Maybe.map (.key >> .tonic) |> Maybe.withDefault -1)
                            , \p -> Expect.equal 4 (List.head p.sections |> Maybe.map (.meter >> .numerator) |> Maybe.withDefault -1)
                            ]
                            project

                    Err err ->
                        Expect.fail ("デコード失敗: " ++ Decode.errorToString err)
        , test "bpm が小数の JSON も正しく読める" <|
            \_ ->
                let
                    project =
                        Data.Project.demo

                    withFloatBpm =
                        { project | bpm = 96.5 }
                in
                withFloatBpm
                    |> ProjectJson.encode
                    |> Decode.decodeValue ProjectJson.decoder
                    |> Result.map .bpm
                    |> Expect.equal (Ok 96.5)
        , test "参考オーディオの durationMs は encode → decode で保持される" <|
            \_ ->
                let
                    project =
                        Data.Project.demo

                    ra =
                        project.referenceAudio

                    withDuration =
                        { project | referenceAudio = { ra | durationMs = Just 222000 } }
                in
                withDuration
                    |> ProjectJson.encode
                    |> Decode.decodeValue ProjectJson.decoder
                    |> Result.map (.referenceAudio >> .durationMs)
                    |> Expect.equal (Ok (Just 222000))
        , test "referenceAudio.durationMs が欠けた旧データも Nothing として読める" <|
            \_ ->
                let
                    legacyJson =
                        Encode.object
                            [ ( "version", Encode.int 1 )
                            , ( "name", Encode.string "旧曲" )
                            , ( "bpm", Encode.int 120 )
                            , ( "tracks", Encode.list identity [] )
                            , ( "sections", Encode.list identity [] )
                            , ( "nextId", Encode.int 1 )
                            , ( "referenceAudio"
                              , Encode.object
                                    [ ( "fileName", Encode.null )
                                    , ( "offsetMs", Encode.int 0 )
                                    , ( "volume", Encode.int 80 )
                                    , ( "muted", Encode.bool False )
                                    ]
                              )
                            ]
                in
                legacyJson
                    |> Decode.decodeValue ProjectJson.decoder
                    |> Result.map (.referenceAudio >> .durationMs)
                    |> Expect.equal (Ok Nothing)
        , test "全楽器が instrumentToString / instrumentFromString で往復できる" <|
            \_ ->
                Data.Track.allInstruments
                    |> List.map (Data.Track.instrumentToString >> Data.Track.instrumentFromString)
                    |> Expect.equal (List.map Just Data.Track.allInstruments)
        , test "encodeWith は selectedTrackId フィールドを含む" <|
            \_ ->
                ProjectJson.encodeWith { selectedTrackId = 5 } Data.Project.demo
                    |> Decode.decodeValue (Decode.field "selectedTrackId" Decode.int)
                    |> Expect.equal (Ok 5)
        , test "selectedTrackIdDecoder は encodeWith の結果を Just で読める" <|
            \_ ->
                ProjectJson.encodeWith { selectedTrackId = 42 } Data.Project.demo
                    |> Decode.decodeValue ProjectJson.selectedTrackIdDecoder
                    |> Expect.equal (Ok (Just 42))
        , test "selectedTrackIdDecoder は（旧形式の）encode の結果では Nothing" <|
            \_ ->
                ProjectJson.encode Data.Project.demo
                    |> Decode.decodeValue ProjectJson.selectedTrackIdDecoder
                    |> Expect.equal (Ok Nothing)
        , test "encodeWith で保存しても project 本体は decoder で元の Project と一致する（余剰フィールドは無視）" <|
            \_ ->
                ProjectJson.encodeWith { selectedTrackId = 5 } Data.Project.demo
                    |> Decode.decodeValue ProjectJson.decoder
                    |> Expect.equal (Ok Data.Project.demo)
        , test "chordTrack.rhythm は encode → decode で保持される" <|
            \_ ->
                let
                    project =
                        Data.Project.demo

                    ct =
                        project.chordTrack

                    withRhythm =
                        { project | chordTrack = { ct | rhythm = Just "8ビート" } }
                in
                withRhythm
                    |> ProjectJson.encode
                    |> Decode.decodeValue ProjectJson.decoder
                    |> Result.map (.chordTrack >> .rhythm)
                    |> Expect.equal (Ok (Just "8ビート"))
        , test "chordTrack.rhythm が欠落した旧データも Nothing として読める（後方互換）" <|
            \_ ->
                let
                    legacyJson =
                        Encode.object
                            [ ( "version", Encode.int 1 )
                            , ( "name", Encode.string "旧曲" )
                            , ( "bpm", Encode.int 120 )
                            , ( "tracks", Encode.list identity [] )
                            , ( "chordTrack"
                              , Encode.object
                                    [ ( "text", Encode.string "C | G" )
                                    , ( "instrument", Encode.string "Piano" )
                                    , ( "muted", Encode.bool False )
                                    , ( "volume", Encode.int 100 )
                                    ]
                              )
                            , ( "sections", Encode.list identity [] )
                            , ( "nextId", Encode.int 1 )
                            ]
                in
                legacyJson
                    |> Decode.decodeValue ProjectJson.decoder
                    |> Result.map (.chordTrack >> .rhythm)
                    |> Expect.equal (Ok Nothing)
        ]
