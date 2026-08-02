module Data.Meter exposing
    ( Meter
    , default
    , fromString
    , options
    , stepsPerBar
    , stepsPerBeat
    , ticksPerBar
    , ticksPerBeat
    , toString
    )

import Data.Time


{-| 拍子。4/4、3/4、6/8 など。セクション単位で持つ。
-}
type alias Meter =
    { numerator : Int
    , denominator : Int
    }


default : Meter
default =
    { numerator = 4, denominator = 4 }


{-| 1拍の長さ。分母が 4 なら四分音符 = ppq、8 なら八分音符。
-}
ticksPerBeat : Meter -> Int
ticksPerBeat meter =
    (Data.Time.ppq * 4) // Basics.max 1 meter.denominator


ticksPerBar : Meter -> Int
ticksPerBar meter =
    ticksPerBeat meter * Basics.max 1 meter.numerator


{-| グリッドのマス目数。1マス = 16分音符相当。
-}
stepsPerBar : Meter -> Int
stepsPerBar meter =
    Basics.max 1 (ticksPerBar meter // Data.Time.ticksPerSixteenth)


stepsPerBeat : Meter -> Int
stepsPerBeat meter =
    Basics.max 1 (ticksPerBeat meter // Data.Time.ticksPerSixteenth)


toString : Meter -> String
toString meter =
    String.fromInt meter.numerator ++ "/" ++ String.fromInt meter.denominator


fromString : String -> Maybe Meter
fromString raw =
    case String.split "/" (String.trim raw) of
        [ n, d ] ->
            Maybe.map2 (\num den -> { numerator = num, denominator = den })
                (String.toInt n)
                (String.toInt d)
                |> Maybe.andThen validate

        _ ->
            Nothing


validate : Meter -> Maybe Meter
validate meter =
    if meter.numerator >= 1 && meter.numerator <= 16 && List.member meter.denominator [ 2, 4, 8, 16 ] then
        Just meter

    else
        Nothing


{-| UI の選択肢。ドラフトで実際に使うものだけに絞る。
-}
options : List Meter
options =
    [ { numerator = 4, denominator = 4 }
    , { numerator = 3, denominator = 4 }
    , { numerator = 2, denominator = 4 }
    , { numerator = 6, denominator = 8 }
    , { numerator = 5, denominator = 4 }
    , { numerator = 7, denominator = 8 }
    , { numerator = 12, denominator = 8 }
    ]
