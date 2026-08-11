module Data.Time exposing
    ( GridUnit(..)
    , Ticks
    , formatDuration
    , gridLabel
    , gridTicks
    , gridUnitFromString
    , gridUnitToString
    , ppq
    , snapFloor
    , snapRound
    , ticksPerBar
    , ticksPerSixteenth
    )


type alias Ticks =
    Int


ppq : Int
ppq =
    480


ticksPerBar : Int
ticksPerBar =
    ppq * 4


ticksPerSixteenth : Int
ticksPerSixteenth =
    ppq // 4


{-| tick数を「小節.拍.16分音符.tick」の4成分表記に分解する（4/4固定のグリッドを前提）。ノートホバーツールチップの長さ表示で使う。
-}
formatDuration : Int -> String
formatDuration d =
    let
        bars =
            d // ticksPerBar

        beats =
            modBy ticksPerBar d // ppq

        sixteenths =
            modBy ppq d // ticksPerSixteenth

        ticks =
            modBy ticksPerSixteenth d
    in
    [ bars, beats, sixteenths, ticks ]
        |> List.map String.fromInt
        |> String.join "."


{-| ピアノロール・ドラムステップグリッドのスナップ・グリッド線単位。Sixteenth が従来の16分音符固定グリッドと互換。
-}
type GridUnit
    = Sixteenth
    | EighthTriplet
    | SixteenthTriplet


{-| グリッド単位1つ分のtick数。ppq=480なのでどれも整数。
-}
gridTicks : GridUnit -> Int
gridTicks unit =
    case unit of
        Sixteenth ->
            ticksPerSixteenth

        EighthTriplet ->
            ppq // 3

        SixteenthTriplet ->
            ppq // 6


{-| select 要素の value 属性用の短い文字列表現。
-}
gridUnitToString : GridUnit -> String
gridUnitToString unit =
    case unit of
        Sixteenth ->
            "16"

        EighthTriplet ->
            "8t"

        SixteenthTriplet ->
            "16t"


gridUnitFromString : String -> Maybe GridUnit
gridUnitFromString s =
    case s of
        "16" ->
            Just Sixteenth

        "8t" ->
            Just EighthTriplet

        "16t" ->
            Just SixteenthTriplet

        _ ->
            Nothing


{-| グリッド選択 UI に表示する日本語ラベル。
-}
gridLabel : GridUnit -> String
gridLabel unit =
    case unit of
        Sixteenth ->
            "16分"

        EighthTriplet ->
            "8分三連"

        SixteenthTriplet ->
            "16分三連"


{-| ticks を指定グリッド単位に四捨五入でスナップする。ノートドラッグのデルタ量やセクションドラッグなど、中心位置への吸着が自然な場面で使う。
-}
snapRound : Int -> Int -> Int
snapRound gridSize ticks =
    Basics.round (toFloat ticks / toFloat gridSize) * gridSize


{-| ticks を指定グリッド単位に切り下げてスナップする（`//` のゼロ方向切り捨て意味論）。ノート新規配置やリージョン開始位置など、常に手前側に吸着したい場面で使う。
-}
snapFloor : Int -> Int -> Int
snapFloor gridSize ticks =
    (ticks // gridSize) * gridSize
