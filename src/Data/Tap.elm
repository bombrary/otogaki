module Data.Tap exposing
    ( LastTap
    , Target(..)
    , isDoubleTap
    , record
    )

{-| iOS Safari はタッチでは dblclick を発火しないため、独自にダブルタップを検出する。
しきい値（300ms / 24px）は Main.elm の従来実装（PressedNote の isDoubleTap 判定）と同じ。
ノート・ドラムセル・ボイシングオフセットの3種のタップ対象を1つのロジックで扱えるようにする。
-}


type Target
    = TapNote Int
    | TapDrumCell { pitch : Int, tick : Int }
    | TapVoicingOffset Int Int


type alias LastTap =
    { target : Target, time : Float, clientX : Float, clientY : Float }


timeThresholdMs : Float
timeThresholdMs =
    300


distanceThresholdPx : Float
distanceThresholdPx =
    24


{-| 直前のタップと同じ対象への、300ms以内・24px以内の再タップならダブルタップとみなす。 -}
isDoubleTap : Target -> { r | timeStamp : Float, clientX : Float, clientY : Float } -> Maybe LastTap -> Bool
isDoubleTap target pos maybeLast =
    case maybeLast of
        Just last ->
            last.target
                == target
                && (pos.timeStamp - last.time)
                < timeThresholdMs
                && abs (pos.clientX - last.clientX)
                < distanceThresholdPx
                && abs (pos.clientY - last.clientY)
                < distanceThresholdPx

        Nothing ->
            False


{-| 今回のタップを次回の比較用に記録する。 -}
record : Target -> { r | timeStamp : Float, clientX : Float, clientY : Float } -> Maybe LastTap
record target pos =
    Just { target = target, time = pos.timeStamp, clientX = pos.clientX, clientY = pos.clientY }
