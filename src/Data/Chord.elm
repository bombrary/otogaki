module Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..), toPitches)


type alias Chord =
    { root : Int
    , quality : Quality
    , extensions : List Extension
    , alterations : List Alteration
    , bass : Maybe Int
    }


type Quality
    = Maj
    | Min
    | Dom7
    | Maj7
    | Min7
    | MinMaj7
    | Dim
    | Dim7
    | HalfDim7
    | Aug
    | Sus2
    | Sus4
    | Sixth
    | MinSixth
    | Power


type Extension
    = Add9
    | Nine
    | FlatNine
    | SharpNine
    | Eleven
    | SharpEleven
    | Thirteen
    | FlatThirteen


type Alteration
    = Flat5
    | Sharp5


qualityIntervals : Quality -> List Int
qualityIntervals quality =
    case quality of
        Maj ->
            [ 0, 4, 7 ]

        Min ->
            [ 0, 3, 7 ]

        Dom7 ->
            [ 0, 4, 7, 10 ]

        Maj7 ->
            [ 0, 4, 7, 11 ]

        Min7 ->
            [ 0, 3, 7, 10 ]

        MinMaj7 ->
            [ 0, 3, 7, 11 ]

        Dim ->
            [ 0, 3, 6 ]

        Dim7 ->
            [ 0, 3, 6, 9 ]

        HalfDim7 ->
            [ 0, 3, 6, 10 ]

        Aug ->
            [ 0, 4, 8 ]

        Sus2 ->
            [ 0, 2, 7 ]

        Sus4 ->
            [ 0, 5, 7 ]

        Sixth ->
            [ 0, 4, 7, 9 ]

        MinSixth ->
            [ 0, 3, 7, 9 ]

        Power ->
            [ 0, 7 ]


extensionSemitone : Extension -> Int
extensionSemitone ext =
    case ext of
        Add9 ->
            14

        Nine ->
            14

        FlatNine ->
            13

        SharpNine ->
            15

        Eleven ->
            17

        SharpEleven ->
            18

        Thirteen ->
            21

        FlatThirteen ->
            20


applyAlteration : Alteration -> List Int -> List Int
applyAlteration alt intervals =
    let
        replaceFifth to =
            List.map
                (\i ->
                    if i == 7 then
                        to

                    else
                        i
                )
                intervals
    in
    case alt of
        Flat5 ->
            replaceFifth 6

        Sharp5 ->
            replaceFifth 8


{-| ルートをC3帯（MIDI 48-59）に置いたクローズドボイシング + 1オクターブ下のベース音。
ボイシング改善はこの関数の差し替えだけで済む。
-}
toPitches : Chord -> List Int
toPitches chord =
    let
        rootMidi =
            48 + modBy 12 chord.root

        intervals =
            List.foldl applyAlteration (qualityIntervals chord.quality) chord.alterations

        chordTones =
            (intervals ++ List.map extensionSemitone chord.extensions)
                |> List.map (\interval -> rootMidi + interval)

        bassMidi =
            36 + modBy 12 (Maybe.withDefault chord.root chord.bass)
    in
    bassMidi :: chordTones
