module Data.Section exposing (Section)

import Data.Key exposing (Key)
import Data.Meter exposing (Meter)


{-| 曲の区分。絶対位置は持たず、リストの順に先頭から連続して並ぶ。
キーと拍子はこの粒度で持つ（転調や拍子変更はセクションを割って表現する）。
-}
type alias Section =
    { id : Int
    , name : String
    , lengthBars : Int
    , memo : String
    , key : Key
    , meter : Meter
    }
