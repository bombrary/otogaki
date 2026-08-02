module MidiEncodeTest exposing (suite)

import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Data.Project
import Expect
import Midi.Encode
import Midi.VarLen as VarLen
import Test exposing (Test, describe, test)


toByteList : Bytes.Bytes -> List Int
toByteList bytes =
    let
        width =
            Bytes.width bytes

        decoder =
            Decode.loop ( width, [] )
                (\( n, acc ) ->
                    if n <= 0 then
                        Decode.succeed (Decode.Done (List.reverse acc))

                    else
                        Decode.unsignedInt8
                            |> Decode.map (\b -> Decode.Loop ( n - 1, b :: acc ))
                )
    in
    Decode.decode decoder bytes
        |> Maybe.withDefault []


varLen : Int -> List Int
varLen value =
    toByteList (Encode.encode (VarLen.encode value))


suite : Test
suite =
    describe "MIDIエンコード"
        [ test "VarLen 0" (\_ -> Expect.equal [ 0x00 ] (varLen 0))
        , test "VarLen 127" (\_ -> Expect.equal [ 0x7F ] (varLen 127))
        , test "VarLen 128" (\_ -> Expect.equal [ 0x81, 0x00 ] (varLen 128))
        , test "VarLen 200" (\_ -> Expect.equal [ 0x81, 0x48 ] (varLen 200))
        , test "VarLen 480" (\_ -> Expect.equal [ 0x83, 0x60 ] (varLen 480))
        , test "VarLen 0x0FFFFFFF" (\_ -> Expect.equal [ 0xFF, 0xFF, 0xFF, 0x7F ] (varLen 0x0FFFFFFF))
        , test "SMFヘッダ" <|
            \_ ->
                let
                    bytes =
                        toByteList (Midi.Encode.fromProject Data.Project.demo)
                in
                Expect.equal
                    [ 0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x01 ]
                    (List.take 10 bytes)
        , test "PPQ=480" <|
            \_ ->
                let
                    bytes =
                        toByteList (Midi.Encode.fromProject Data.Project.demo)
                in
                Expect.equal [ 0x01, 0xE0 ] (bytes |> List.drop 12 |> List.take 2)
        ]
