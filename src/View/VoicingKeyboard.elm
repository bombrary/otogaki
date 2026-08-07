module View.VoicingKeyboard exposing (Config, containerHeight, maxPitch, minPitch, rowHeight, scrollId, view)

import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Set exposing (Set)
import View.Style as Style


type alias Config msg =
    { pressedOffset : Int -> { clientX : Float, clientY : Float, shift : Bool } -> msg
    , doubleClickedOffset : Int -> msg
    }


{-| ピアノロール（`View.PianoRoll`）と同じ音域。
-}
minPitch : Int
minPitch =
    36


maxPitch : Int
maxPitch =
    84


rowHeight : Int
rowHeight =
    14


containerHeight : Int
containerHeight =
    336


keyColumnWidth : Int
keyColumnWidth =
    44


laneWidth : Int
laneWidth =
    120


{-| 開いた直後に root 行までスクロールさせるための DOM id。同時に開くのは 1 行だけなので固定値でよい。
-}
scrollId : String
scrollId =
    "voicing-keyboard-scroll"


isWhite : Int -> Bool
isWhite pitch =
    List.member (modBy 12 pitch) [ 0, 2, 4, 5, 7, 9, 11 ]


{-| 表示上の root（displayRootPitch）からの相対半音。実際に置かれている最低音が基準なので常に 0 以上になる。
-}
offsetLabel : Int -> Int -> String
offsetLabel displayRootPitch pitch =
    let
        rel =
            pitch - displayRootPitch
    in
    if rel == 0 then
        "root"

    else
        "+" ++ String.fromInt rel


pressDecoder : Decode.Decoder { clientX : Float, clientY : Float, shift : Bool }
pressDecoder =
    Decode.map3 (\cx cy sh -> { clientX = cx, clientY = cy, shift = sh })
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "shiftKey" Decode.bool)


view : Config msg -> { rootPitch : Int, displayRootPitch : Int, selected : Set Int } -> Html msg
view config { rootPitch, displayRootPitch, selected } =
    let
        pitches =
            List.range minPitch maxPitch |> List.reverse
    in
    div
        [ HA.id scrollId
        , HA.style "height" (String.fromInt containerHeight ++ "px")
        , HA.style "overflow-y" "auto"
        , HA.style "border" "1px solid #ccc"
        , HA.style "margin-top" "0.4rem"
        ]
        [ div [ HA.style "display" "flex" ]
            [ div [ HA.style "flex" ("0 0 " ++ String.fromInt keyColumnWidth ++ "px") ]
                (List.map (keyRow displayRootPitch rootPitch) pitches)
            , div [ HA.style "flex" ("0 0 " ++ String.fromInt laneWidth ++ "px"), HA.style "position" "relative" ]
                (List.map (laneRow config displayRootPitch rootPitch selected) pitches)
            ]
        ]


keyRow : Int -> Int -> Int -> Html msg
keyRow displayRootPitch rootPitch pitch =
    let
        isBlack =
            not (isWhite pitch)

        isRoot =
            pitch == displayRootPitch

        isBelowRoot =
            pitch < rootPitch

        label =
            if modBy 12 pitch == 0 then
                "C" ++ String.fromInt (pitch // 12 - 1)

            else
                ""
    in
    div
        [ HA.style "height" (String.fromInt rowHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "background"
            (if isBlack then
                "#444"

             else
                "#fff"
            )
        , HA.style "color"
            (if isBlack then
                "#eee"

             else
                "#555"
            )
        , HA.style "opacity"
            (if isBelowRoot then
                "0.35"

             else
                "1"
            )
        , HA.style "border-bottom" "1px solid #ddd"
        , HA.style "border-top"
            (if isRoot then
                "2px solid " ++ Style.colorPrimary

             else
                "none"
            )
        , HA.style "font-size" "9px"
        , HA.style "line-height" (String.fromInt (rowHeight - 1) ++ "px")
        , HA.style "text-align" "right"
        , HA.style "padding-right" "3px"
        , HA.style "user-select" "none"
        ]
        [ text label ]


laneRow : Config msg -> Int -> Int -> Set Int -> Int -> Html msg
laneRow config displayRootPitch rootPitch selected pitch =
    let
        isSelected =
            Set.member pitch selected

        isRoot =
            pitch == displayRootPitch

        isBelowRoot =
            pitch < rootPitch && not isSelected
    in
    div
        [ HA.style "height" (String.fromInt rowHeight ++ "px")
        , HA.style "box-sizing" "border-box"
        , HA.style "background"
            (if isSelected then
                Style.colorHighlight

             else if isWhite pitch then
                "#fff"

             else
                "#f5f5f5"
            )
        , HA.style "opacity"
            (if isBelowRoot then
                "0.35"

             else
                "1"
            )
        , HA.style "border-bottom" "1px solid #eee"
        , HA.style "border-top"
            (if isRoot then
                "2px solid " ++ Style.colorPrimary

             else
                "none"
            )
        , HA.style "font-size" "9px"
        , HA.style "line-height" (String.fromInt (rowHeight - 1) ++ "px")
        , HA.style "padding-left" "4px"
        , HA.style "color" "#333"
        , HA.style "cursor" "pointer"
        , HA.style "user-select" "none"
        , HE.stopPropagationOn "mousedown"
            (Decode.map (\pos -> ( config.pressedOffset pitch pos, True )) pressDecoder)
        , HE.stopPropagationOn "dblclick"
            (Decode.succeed ( config.doubleClickedOffset pitch, True ))
        ]
        [ text
            (if isSelected then
                offsetLabel displayRootPitch pitch

             else
                ""
            )
        ]
