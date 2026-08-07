module Data.Key exposing
    ( Key
    , Mode(..)
    , default
    , degreeLabel
    , fromString
    , isFlatSpelled
    , modeFromString
    , modeToString
    , scalePitchClasses
    , toString
    , tonicName
    , tonicOptions
    , transpose
    )

import Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..))
import Set exposing (Set)


type Mode
    = Major
    | Minor


{-| 調。tonic は 0-11 のピッチクラス（C=0）。
-}
type alias Key =
    { tonic : Int
    , mode : Mode
    }


default : Key
default =
    { tonic = 0, mode = Major }


transpose : Int -> Key -> Key
transpose semitones key =
    { key | tonic = modBy 12 (key.tonic + semitones) }



-- スケール


scaleIntervals : Mode -> List Int
scaleIntervals mode =
    case mode of
        Major ->
            [ 0, 2, 4, 5, 7, 9, 11 ]

        Minor ->
            -- 自然的短音階
            [ 0, 2, 3, 5, 7, 8, 10 ]


{-| この調のスケール音をピッチクラスの集合で返す。ピアノロールのガイド表示に使う。
-}
scalePitchClasses : Key -> Set Int
scalePitchClasses key =
    scaleIntervals key.mode
        |> List.map (\i -> modBy 12 (key.tonic + i))
        |> Set.fromList



-- 度数


{-| コードの生トークンがフラット綴りか。`Bb` は True、`C7b9` は False（調号は2文字目だけ見る）。
-}
isFlatSpelled : String -> Bool
isFlatSpelled token =
    String.slice 1 2 token == "b"


type Casing
    = Upper
    | Lower


{-| ローマ数字本体。大文字/小文字はコードの quality 側で決めるので、ここは常に大文字。
度数を厳密な異名同音で出すには Chord に綴り情報が要るので、ドラフト用途として
キーからの距離で決め打ちし、曖昧なものだけ生トークンの綴りで分ける。
-}
romanFor : Mode -> Bool -> Int -> String
romanFor mode flat interval =
    case mode of
        Major ->
            case interval of
                0 ->
                    "I"

                1 ->
                    "♭II"

                2 ->
                    "II"

                3 ->
                    "♭III"

                4 ->
                    "III"

                5 ->
                    "IV"

                6 ->
                    if flat then
                        "♭V"

                    else
                        "♯IV"

                7 ->
                    "V"

                8 ->
                    "♭VI"

                9 ->
                    "VI"

                10 ->
                    "♭VII"

                _ ->
                    "VII"

        Minor ->
            case interval of
                0 ->
                    "I"

                1 ->
                    "♭II"

                2 ->
                    "II"

                3 ->
                    "III"

                4 ->
                    if flat then
                        "♭IV"

                    else
                        "♯III"

                5 ->
                    "IV"

                6 ->
                    if flat then
                        "♭V"

                    else
                        "♯IV"

                7 ->
                    "V"

                8 ->
                    "VI"

                9 ->
                    "♯VI"

                10 ->
                    "VII"

                _ ->
                    "♯VII"


qualityShape : Quality -> { casing : Casing, mark : String, suffix : String }
qualityShape quality =
    case quality of
        Maj ->
            { casing = Upper, mark = "", suffix = "" }

        Min ->
            { casing = Lower, mark = "", suffix = "" }

        Dom7 ->
            { casing = Upper, mark = "", suffix = "7" }

        Maj7 ->
            { casing = Upper, mark = "", suffix = "M7" }

        Min7 ->
            { casing = Lower, mark = "", suffix = "7" }

        MinMaj7 ->
            { casing = Lower, mark = "", suffix = "M7" }

        Dim ->
            { casing = Lower, mark = "°", suffix = "" }

        Dim7 ->
            { casing = Lower, mark = "°", suffix = "7" }

        HalfDim7 ->
            { casing = Lower, mark = "ø", suffix = "7" }

        Aug ->
            { casing = Upper, mark = "+", suffix = "" }

        Sus2 ->
            { casing = Upper, mark = "", suffix = "sus2" }

        Sus4 ->
            { casing = Upper, mark = "", suffix = "sus4" }

        Dom7Sus4 ->
            { casing = Upper, mark = "", suffix = "7sus4" }

        Sixth ->
            { casing = Upper, mark = "", suffix = "6" }

        MinSixth ->
            { casing = Lower, mark = "", suffix = "6" }

        Power ->
            { casing = Upper, mark = "", suffix = "5" }


extensionLabel : Extension -> String
extensionLabel ext =
    case ext of
        Add9 ->
            "add9"

        Nine ->
            "(9)"

        FlatNine ->
            "(♭9)"

        SharpNine ->
            "(♯9)"

        Eleven ->
            "(11)"

        SharpEleven ->
            "(♯11)"

        Thirteen ->
            "(13)"

        FlatThirteen ->
            "(♭13)"


alterationLabel : Alteration -> String
alterationLabel alt =
    case alt of
        Flat5 ->
            "(♭5)"

        Sharp5 ->
            "(♯5)"


{-| キーに対するコードの度数表記。`IV`、`V7`、`vi`、`♭VII`、`iiø7` など。
-}
degreeLabel : Key -> { spelledFlat : Bool } -> Chord -> String
degreeLabel key opts chord =
    let
        shape =
            qualityShape chord.quality

        roman =
            romanFor key.mode opts.spelledFlat (modBy 12 (chord.root - key.tonic))

        cased =
            case shape.casing of
                Upper ->
                    roman

                Lower ->
                    String.toLower roman

        extras =
            String.concat (List.map extensionLabel chord.extensions)
                ++ String.concat (List.map alterationLabel chord.alterations)

        slash =
            case chord.bass of
                Just bass ->
                    "/" ++ romanFor key.mode opts.spelledFlat (modBy 12 (bass - key.tonic))

                Nothing ->
                    ""
    in
    cased ++ shape.mark ++ shape.suffix ++ extras ++ slash



-- 表記


tonicNames : List String
tonicNames =
    [ "C", "C#", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B" ]


tonicName : Int -> String
tonicName pitchClass =
    List.drop (modBy 12 pitchClass) tonicNames
        |> List.head
        |> Maybe.withDefault "C"


tonicOptions : List ( Int, String )
tonicOptions =
    List.indexedMap Tuple.pair tonicNames


toString : Key -> String
toString key =
    tonicName key.tonic
        ++ (case key.mode of
                Major ->
                    ""

                Minor ->
                    "m"
           )


modeToString : Mode -> String
modeToString mode =
    case mode of
        Major ->
            "major"

        Minor ->
            "minor"


modeFromString : String -> Maybe Mode
modeFromString raw =
    case raw of
        "major" ->
            Just Major

        "minor" ->
            Just Minor

        _ ->
            Nothing


{-| シャープ表記とフラット表記のどちらでも受ける。
-}
tonicFromName : String -> Maybe Int
tonicFromName raw =
    case raw of
        "C" ->
            Just 0

        "B#" ->
            Just 0

        "C#" ->
            Just 1

        "Db" ->
            Just 1

        "D" ->
            Just 2

        "D#" ->
            Just 3

        "Eb" ->
            Just 3

        "E" ->
            Just 4

        "Fb" ->
            Just 4

        "F" ->
            Just 5

        "E#" ->
            Just 5

        "F#" ->
            Just 6

        "Gb" ->
            Just 6

        "G" ->
            Just 7

        "G#" ->
            Just 8

        "Ab" ->
            Just 8

        "A" ->
            Just 9

        "A#" ->
            Just 10

        "Bb" ->
            Just 10

        "B" ->
            Just 11

        "Cb" ->
            Just 11

        _ ->
            Nothing


fromString : String -> Maybe Key
fromString raw =
    let
        s =
            String.trim raw

        ( body, mode ) =
            if String.endsWith "m" s && String.length s > 1 then
                ( String.dropRight 1 s, Minor )

            else
                ( s, Major )
    in
    tonicFromName body |> Maybe.map (\tonic -> { tonic = tonic, mode = mode })
