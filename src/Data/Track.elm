module Data.Track exposing
    ( Instrument(..)
    , Track
    , TrackKind(..)
    , allInstruments
    , instrumentFromString
    , instrumentLabel
    , instrumentToString
    )

import Data.Note exposing (Note)


type Instrument
    = Piano
    | AcousticGuitar
    | ElectricBass
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
    [ Piano, AcousticGuitar, ElectricBass, DrumKit, SynthLead ]


instrumentToString : Instrument -> String
instrumentToString instrument =
    case instrument of
        Piano ->
            "piano"

        AcousticGuitar ->
            "acousticGuitar"

        ElectricBass ->
            "electricBass"

        DrumKit ->
            "drumKit"

        SynthLead ->
            "synthLead"


instrumentFromString : String -> Maybe Instrument
instrumentFromString raw =
    case raw of
        "piano" ->
            Just Piano

        "acousticGuitar" ->
            Just AcousticGuitar

        "electricBass" ->
            Just ElectricBass

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

        AcousticGuitar ->
            "アコギ"

        ElectricBass ->
            "ベース"

        DrumKit ->
            "ドラム"

        SynthLead ->
            "シンセ"
