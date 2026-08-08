module Data.Timeline exposing
    ( Bar
    , Timeline
    , barAt
    , fractionalBarToTicks
    , fromSections
    , keyAt
    , meterAt
    , sectionAt
    , sectionBounds
    , sectionIndexAt
    , tailPaddingBars
    , ticksToBarBeat
    , ticksToFractionalBar
    , totalBars
    , totalTicks
    )

import Array exposing (Array)
import Data.Key exposing (Key)
import Data.Meter exposing (Meter)
import Data.Section exposing (Section)


{-| 曲の1小節分の情報。`bars` の index がそのまま小節番号。
-}
type alias Bar =
    { index : Int
    , startTicks : Int
    , lengthTicks : Int
    , meter : Meter
    , key : Key
    , sectionId : Maybe Int
    , sectionIndex : Maybe Int
    }


type alias Timeline =
    { bars : Array Bar
    }


{-| セクションが足りない分だけ常に末尾に残す余白。ここに新規ノートを置ける余地が常にあるようにする（24小節問題の修正）。
-}
tailPaddingBars : Int
tailPaddingBars =
    4


{-| セクション列から曲全体の小節表を作る。`minBars` はセクション合計を上回っても少なくともこの長さまでは
小節表を伸ばす（末尾余白やノートの終端などを呼び出し側で含めて渡す）。
-}
fromSections : { minBars : Int } -> List Section -> Timeline
fromSections { minBars } sections =
    let
        step ( idx, section ) ( acc, startTicks, startBar ) =
            let
                barTicks =
                    Data.Meter.ticksPerBar section.meter

                newBars =
                    List.range 0 (section.lengthBars - 1)
                        |> List.map
                            (\i ->
                                { index = startBar + i
                                , startTicks = startTicks + i * barTicks
                                , lengthTicks = barTicks
                                , meter = section.meter
                                , key = section.key
                                , sectionId = Just section.id
                                , sectionIndex = Just idx
                                }
                            )
            in
            ( acc ++ newBars
            , startTicks + section.lengthBars * barTicks
            , startBar + section.lengthBars
            )

        ( coveredBars, coveredTicks, coveredCount ) =
            List.foldl step ( [], 0, 0 ) (List.indexedMap Tuple.pair sections)

        lastMeter =
            List.reverse sections |> List.head |> Maybe.map .meter |> Maybe.withDefault Data.Meter.default

        lastKey =
            List.reverse sections |> List.head |> Maybe.map .key |> Maybe.withDefault Data.Key.default

        fillCount =
            Basics.max 0 (minBars - coveredCount)

        fillTicks =
            Data.Meter.ticksPerBar lastMeter

        fillBars =
            List.range 0 (fillCount - 1)
                |> List.map
                    (\i ->
                        { index = coveredCount + i
                        , startTicks = coveredTicks + i * fillTicks
                        , lengthTicks = fillTicks
                        , meter = lastMeter
                        , key = lastKey
                        , sectionId = Nothing
                        , sectionIndex = Nothing
                        }
                    )
    in
    { bars = Array.fromList (coveredBars ++ fillBars) }


barAt : Int -> Timeline -> Maybe Bar
barAt index timeline =
    Array.get index timeline.bars


totalBars : Timeline -> Int
totalBars timeline =
    Array.length timeline.bars


totalTicks : Timeline -> Int
totalTicks timeline =
    case Array.get (Array.length timeline.bars - 1) timeline.bars of
        Just bar ->
            bar.startTicks + bar.lengthTicks

        Nothing ->
            0


{-| tick 位置が属する小節。最後の小節より後ろなら最後の小節を返す。
-}
barContaining : Int -> Timeline -> Maybe Bar
barContaining ticks timeline =
    let
        go i =
            case Array.get i timeline.bars of
                Nothing ->
                    if i > 0 then
                        Array.get (i - 1) timeline.bars

                    else
                        Nothing

                Just bar ->
                    if ticks < bar.startTicks + bar.lengthTicks then
                        Just bar

                    else
                        go (i + 1)
    in
    go 0


{-| tick 位置を、拍子の違う小節をまたいでも正しい小数点付き小節位置（0-based）に変換する。セクションルーラー上の
再生位置・ループ範囲の描画位置計算で使う。
-}
ticksToFractionalBar : Int -> Timeline -> Float
ticksToFractionalBar ticks timeline =
    case barContaining ticks timeline of
        Just bar ->
            toFloat bar.index + toFloat (ticks - bar.startTicks) / toFloat bar.lengthTicks

        Nothing ->
            0


{-| `ticksToFractionalBar` の逆変換。セクションルーラー上のクリック/ドラッグ位置を tick に戻すのに使う。
-}
fractionalBarToTicks : Float -> Timeline -> Int
fractionalBarToTicks fractionalBar timeline =
    let
        wholeBar =
            floor fractionalBar

        frac =
            fractionalBar - toFloat wholeBar
    in
    case barAt wholeBar timeline of
        Just bar ->
            bar.startTicks + round (frac * toFloat bar.lengthTicks)

        Nothing ->
            totalTicks timeline


meterAt : Int -> Timeline -> Meter
meterAt ticks timeline =
    barContaining ticks timeline |> Maybe.map .meter |> Maybe.withDefault Data.Meter.default


keyAt : Int -> Timeline -> Key
keyAt ticks timeline =
    barContaining ticks timeline |> Maybe.map .key |> Maybe.withDefault Data.Key.default


sectionAt : Int -> Timeline -> Maybe Int
sectionAt ticks timeline =
    barContaining ticks timeline |> Maybe.andThen .sectionId


{-| tick 位置が属するセクションのリスト内インデックス（0-based）。id ではなく表示順番なので、
削除・並べ替え後も色の割り当てに使える。
-}
sectionIndexAt : Int -> Timeline -> Maybe Int
sectionIndexAt ticks timeline =
    barContaining ticks timeline |> Maybe.andThen .sectionIndex


sectionBounds : Int -> Timeline -> Maybe { startTicks : Int, endTicks : Int }
sectionBounds sectionId timeline =
    let
        matching =
            Array.toList timeline.bars |> List.filter (\b -> b.sectionId == Just sectionId)
    in
    case matching of
        [] ->
            Nothing

        first :: _ ->
            let
                last =
                    List.reverse matching |> List.head |> Maybe.withDefault first
            in
            Just { startTicks = first.startTicks, endTicks = last.startTicks + last.lengthTicks }


{-| tick 位置を（1-based 小節番号, 1-based 拍）に変換する。拍子はその小節の meter から引く。
-}
ticksToBarBeat : Int -> Timeline -> { bar : Int, beat : Int }
ticksToBarBeat ticks timeline =
    case barContaining ticks timeline of
        Just bar ->
            { bar = bar.index + 1
            , beat = (ticks - bar.startTicks) // Data.Meter.ticksPerBeat bar.meter + 1
            }

        Nothing ->
            { bar = 1, beat = 1 }
