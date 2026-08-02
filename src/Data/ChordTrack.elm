module Data.ChordTrack exposing
    ( ChordCell
    , ChordTrack
    , ResolvedChord
    , TokenKind(..)
    , barCount
    , cells
    , empty
    , resolved
    , transpose
    )

import Data.Chord exposing (Chord)
import Data.Chord.Format as ChordFormat
import Data.Chord.Parser as ChordParser
import Data.Meter
import Data.Timeline exposing (Timeline)
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
    , startTicks : Int
    , lengthTicks : Int
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


{-| テキスト中の小節区切りの数。ticks は不要な場面（曲全体の小節表の長さを決める際など）で使う。
-}
barCount : ChordTrack -> Int
barCount track =
    track.text
        |> String.replace "\n" " "
        |> String.split "|"
        |> List.length


{-| 小節区切りは | だけ。改行は空白と同じ扱いで、見た目の整形のために自由に使える。
各小節の拍子は Timeline から引く（セクションごとに拍子が違う場合に対応）。
-}
cells : Timeline -> ChordTrack -> List ChordCell
cells timeline track =
    track.text
        |> String.replace "\n" " "
        |> String.split "|"
        |> List.indexedMap
            (\i cell ->
                let
                    bar =
                        Data.Timeline.barAt i timeline

                    startTicks =
                        bar |> Maybe.map .startTicks |> Maybe.withDefault (i * Data.Meter.ticksPerBar Data.Meter.default)

                    lengthTicks =
                        bar |> Maybe.map .lengthTicks |> Maybe.withDefault (Data.Meter.ticksPerBar Data.Meter.default)
                in
                { barIndex = i
                , startTicks = startTicks
                , lengthTicks = lengthTicks
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
resolved : Timeline -> ChordTrack -> List ResolvedChord
resolved timeline track =
    cells timeline track
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
                cell.lengthTicks // n
        in
        cell.chords
            |> List.indexedMap Tuple.pair
            |> List.foldl
                (\( j, c ) st ->
                    let
                        start =
                            cell.startTicks + j * dur

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


{-| コード進行テキストを丸ごと移調する。`%`・`_`・`=`・改行・空白・`|` はテキストの部分置換で
そのまま保持し、パースできるコードトークンだけを root/bass を +semitones した上で書き直す。
パースできないトークンは無変換のまま残す。
-}
transpose : Int -> ChordTrack -> ChordTrack
transpose semitones track =
    { track | text = transposeText semitones track.text }


type alias TextSegment =
    { content : String
    , isWord : Bool
    }


isDelim : Char -> Bool
isDelim c =
    c == ' ' || c == '\n' || c == '\t' || c == '|'


{-| テキストを「区切り文字の連続」と「単語（区切り文字以外）の連続」に分割する。
全セグメントを連結すると元のテキストに一致するので、区切り文字側は完全に保持できる。
-}
tokenize : String -> List TextSegment
tokenize text =
    String.toList text
        |> List.foldl
            (\c segs ->
                let
                    wordChar =
                        not (isDelim c)
                in
                case segs of
                    seg :: rest ->
                        if seg.isWord == wordChar then
                            { seg | content = seg.content ++ String.fromChar c } :: rest

                        else
                            { content = String.fromChar c, isWord = wordChar } :: seg :: rest

                    [] ->
                        [ { content = String.fromChar c, isWord = wordChar } ]
            )
            []
        |> List.reverse


transposeText : Int -> String -> String
transposeText semitones text =
    tokenize text
        |> List.map
            (\seg ->
                if seg.isWord then
                    transposeWord semitones seg.content

                else
                    seg.content
            )
        |> String.concat


transposeWord : Int -> String -> String
transposeWord semitones word =
    case word of
        "%" ->
            word

        "_" ->
            word

        "=" ->
            word

        _ ->
            case ChordParser.parse word of
                Ok chord ->
                    ChordFormat.format { preferFlat = False } (transposeChord semitones chord)

                Err _ ->
                    word


transposeChord : Int -> Chord -> Chord
transposeChord semitones chord =
    { chord
        | root = modBy 12 (chord.root + semitones)
        , bass = Maybe.map (\b -> modBy 12 (b + semitones)) chord.bass
    }
