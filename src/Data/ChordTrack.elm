module Data.ChordTrack exposing
    ( ChordCell
    , ChordTrack
    , ResolvedChord
    , TokenKind(..)
    , cells
    , empty
    , resolved
    )

import Data.Chord exposing (Chord)
import Data.Chord.Parser as ChordParser
import Data.Time
import Data.Track exposing (Instrument(..))


type alias ChordTrack =
    { text : String
    , instrument : Instrument
    , muted : Bool
    , volume : Int
    }


empty : ChordTrack
empty =
    { text = ""
    , instrument = Piano
    , muted = False
    , volume = 100
    }


type TokenKind
    = TChord Chord
    | TRepeat
    | TRest
    | THold


type alias ChordCell =
    { barIndex : Int
    , chords : List { token : String, result : Result String TokenKind }
    }


parseToken : String -> Result String TokenKind
parseToken tok =
    case tok of
        "%" ->
            Ok TRepeat

        "_" ->
            Ok TRest

        "=" ->
            Ok THold

        _ ->
            ChordParser.parse tok |> Result.map TChord


{-| 小節区切りは | だけ。改行は空白と同じ扱いで、見た目の整形のために自由に使える。
-}
cells : ChordTrack -> List ChordCell
cells track =
    track.text
        |> String.replace "\n" " "
        |> String.split "|"
        |> List.indexedMap
            (\i cell ->
                { barIndex = i
                , chords =
                    String.words cell
                        |> List.filter (\w -> w /= "")
                        |> List.map (\tok -> { token = tok, result = parseToken tok })
                }
            )


type alias ResolvedChord =
    { startTicks : Int
    , durationTicks : Int
    , chord : Chord
    }


type alias ResolveState =
    { lastChord : Maybe Chord
    , eventsRev : List ResolvedChord
    }


{-| セル列を実際に鳴るコードイベント列に展開する。
% = 直前のコードをもう一度鳴らす、\_ = 休符、= = 直前のコードを伸ばす。
-}
resolved : ChordTrack -> List ResolvedChord
resolved track =
    cells track
        |> List.foldl resolveCell { lastChord = Nothing, eventsRev = [] }
        |> .eventsRev
        |> List.reverse


resolveCell : ChordCell -> ResolveState -> ResolveState
resolveCell cell state =
    let
        n =
            List.length cell.chords
    in
    if n == 0 then
        state

    else
        let
            dur =
                Data.Time.ticksPerBar // n
        in
        cell.chords
            |> List.indexedMap Tuple.pair
            |> List.foldl
                (\( j, c ) st ->
                    let
                        start =
                            cell.barIndex * Data.Time.ticksPerBar + j * dur

                        attack chord =
                            { st
                                | lastChord = Just chord
                                , eventsRev =
                                    { startTicks = start, durationTicks = dur, chord = chord }
                                        :: st.eventsRev
                            }
                    in
                    case c.result of
                        Ok (TChord chord) ->
                            attack chord

                        Ok TRepeat ->
                            case st.lastChord of
                                Just chord ->
                                    attack chord

                                Nothing ->
                                    st

                        Ok THold ->
                            case st.eventsRev of
                                ev :: rest ->
                                    if ev.startTicks + ev.durationTicks == start then
                                        { st | eventsRev = { ev | durationTicks = ev.durationTicks + dur } :: rest }

                                    else
                                        st

                                [] ->
                                    st

                        Ok TRest ->
                            st

                        Err _ ->
                            st
                )
                state
