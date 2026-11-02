module PitchNameTest exposing (suite)

import Data.PitchName as PitchName exposing (OctaveConvention(..))
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.PitchName"
        [ describe "label"
            ([ ( 60, "C4", "C3" )
             , ( 61, "C#4", "C#3" )
             , ( 48, "C3", "C2" )
             , ( 59, "B3", "B2" )
             , ( 23, "B0", "B-1" )
             , ( 0, "C-1", "C-2" )
             ]
                |> List.map
                    (\( pitch, c4, c3 ) ->
                        test ("pitch " ++ String.fromInt pitch) <|
                            \_ ->
                                Expect.equal ( c4, c3 )
                                    ( PitchName.label C4Is60 pitch, PitchName.label C3Is60 pitch )
                    )
            )
        , test "toString / fromString の往復" <|
            \_ ->
                Expect.equal [ C4Is60, C3Is60 ]
                    (List.map (\c -> PitchName.fromString (PitchName.toString c)) [ C4Is60, C3Is60 ])
        , test "未知の文字列は既定" <|
            \_ ->
                Expect.equal PitchName.default (PitchName.fromString "xyz")
        ]
