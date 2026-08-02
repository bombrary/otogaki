module Midi.VarLen exposing (encode)

import Bytes.Encode as Encode


{-| MIDI の可変長数値表現。7bit ずつ区切り、最終バイト以外は MSB を立てる。
-}
encode : Int -> Encode.Encoder
encode value =
    let
        digits v acc =
            if v < 128 then
                v :: acc

            else
                digits (v // 128) (modBy 128 v :: acc)

        raw =
            digits (Basics.max 0 value) []

        flagged =
            case List.reverse raw of
                lsb :: restRev ->
                    List.reverse (List.map (\b -> b + 128) restRev) ++ [ lsb ]

                [] ->
                    [ 0 ]
    in
    Encode.sequence (List.map Encode.unsignedInt8 flagged)
