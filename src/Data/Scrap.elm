module Data.Scrap exposing (Scrap)

import Data.Note exposing (Note)


{-| 断片棚の1アイテム。notes は先頭が 0 tick になるよう正規化して保存する。
-}
type alias Scrap =
    { id : Int
    , name : String
    , notes : List Note
    }
