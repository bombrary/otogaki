module Data.Track exposing
    ( Instrument(..)
    , Track
    , TrackKind(..)
    , allInstruments
    , instrumentFromString
    , instrumentGroups
    , instrumentLabel
    , instrumentToString
    )

import Data.Note exposing (Note)


type Instrument
    = Piano
    | ElectricPiano
    | Organ
    | AcousticGuitar
    | SteelGuitar
    | ElectricGuitarClean
    | ElectricGuitarJazz
    | ElectricBass
    | UprightBass
    | Vibraphone
    | Strings
    | Mellotron
    | DrumKit
    | SynthLead


type TrackKind
    = NoteTrack (List Note)
    | DrumTrack (List Note)


type alias Track =
    { id : Int
    , name : String
    , instrument : Instrument
    , muted : Bool
    , volume : Int
    , kind : TrackKind
    }


allInstruments : List Instrument
allInstruments =
    List.concatMap Tuple.second instrumentGroups


{-| 音色選択 UI（select の optgroup）向けのグループ分け。表示順もここで決まる。
-}
instrumentGroups : List ( String, List Instrument )
instrumentGroups =
    [ ( "鍵盤", [ Piano, ElectricPiano, Organ ] )
    , ( "ギター", [ AcousticGuitar, SteelGuitar, ElectricGuitarClean, ElectricGuitarJazz ] )
    , ( "ベース", [ ElectricBass, UprightBass ] )
    , ( "その他", [ Vibraphone, Strings, Mellotron, SynthLead ] )
    , ( "ドラム", [ DrumKit ] )
    ]


instrumentToString : Instrument -> String
instrumentToString instrument =
    case instrument of
        Piano ->
            "piano"

        ElectricPiano ->
            "electricPiano"

        Organ ->
            "organ"

        AcousticGuitar ->
            "acousticGuitar"

        SteelGuitar ->
            "steelGuitar"

        ElectricGuitarClean ->
            "electricGuitarClean"

        ElectricGuitarJazz ->
            "electricGuitarJazz"

        ElectricBass ->
            "electricBass"

        UprightBass ->
            "uprightBass"

        Vibraphone ->
            "vibraphone"

        Strings ->
            "strings"

        Mellotron ->
            "mellotron"

        DrumKit ->
            "drumKit"

        SynthLead ->
            "synthLead"


instrumentFromString : String -> Maybe Instrument
instrumentFromString raw =
    case raw of
        "piano" ->
            Just Piano

        "electricPiano" ->
            Just ElectricPiano

        "organ" ->
            Just Organ

        "acousticGuitar" ->
            Just AcousticGuitar

        "steelGuitar" ->
            Just SteelGuitar

        "electricGuitarClean" ->
            Just ElectricGuitarClean

        "electricGuitarJazz" ->
            Just ElectricGuitarJazz

        "electricBass" ->
            Just ElectricBass

        "uprightBass" ->
            Just UprightBass

        "vibraphone" ->
            Just Vibraphone

        "strings" ->
            Just Strings

        "mellotron" ->
            Just Mellotron

        "drumKit" ->
            Just DrumKit

        "synthLead" ->
            Just SynthLead

        _ ->
            Nothing


instrumentLabel : Instrument -> String
instrumentLabel instrument =
    case instrument of
        Piano ->
            "ピアノ"

        ElectricPiano ->
            "エレピ"

        Organ ->
            "オルガン"

        AcousticGuitar ->
            "アコギ（ナイロン）"

        SteelGuitar ->
            "アコギ（スチール）"

        ElectricGuitarClean ->
            "エレキ（クリーン）"

        ElectricGuitarJazz ->
            "エレキ（ジャズ）"

        ElectricBass ->
            "ベース"

        UprightBass ->
            "ウッドベース"

        Vibraphone ->
            "ビブラフォン"

        Strings ->
            "ストリングス"

        Mellotron ->
            "メロトロン"

        DrumKit ->
            "ドラム"

        SynthLead ->
            "シンセ"
