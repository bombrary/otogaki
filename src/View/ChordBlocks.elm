module View.ChordBlocks exposing (view)

import Data.ChordTrack exposing (ChordCell, ChordTrack, TokenKind(..))
import Data.Key exposing (Key)
import Data.Timeline exposing (Timeline)
import Html exposing (Html, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import View.Palette as Palette
import View.Style as Style


{-| 小節ごとのブロックとしてコード進行を表示する。旧 ChordEditor.cellsView/cellView/chordView
（`e325f2e` で削除される前の実装）の移植。ピアノロール風の一直線表示（View.ChordStrip）と
切り替えて使うトグル表示。
-}
view : (Int -> msg) -> Timeline -> Int -> ChordTrack -> Html msg
view clickedChord timeline playheadTicks track =
    div
        [ HA.style "display" "flex"
        , HA.style "gap" "2px"
        , HA.style "margin-top" "0.4rem"
        , HA.style "flex-wrap" "wrap"
        ]
        (List.map (cellView clickedChord timeline playheadTicks) (Data.ChordTrack.cells timeline track))


cellView : (Int -> msg) -> Timeline -> Int -> ChordCell -> Html msg
cellView clickedChord timeline playheadTicks cell =
    let
        isCurrentBar =
            playheadTicks >= cell.startTicks && playheadTicks < cell.startTicks + cell.lengthTicks

        currentToken =
            if isCurrentBar then
                (playheadTicks - cell.startTicks) * List.length cell.chords // Basics.max 1 cell.lengthTicks

            else
                -1

        tokenCount =
            Basics.max 1 (List.length cell.chords)

        tickAtToken index =
            cell.startTicks + index * cell.lengthTicks // tokenCount

        key =
            Data.Timeline.keyAt cell.startTicks timeline

        background =
            if isCurrentBar then
                "#fff6dd"

            else
                case Data.Timeline.sectionIndexAt cell.startTicks timeline of
                    Just idx ->
                        Palette.sectionTint idx

                    Nothing ->
                        Palette.neutral
    in
    div
        [ HA.style "border"
            (if isCurrentBar then
                "1px solid #e6a817"

             else
                "1px solid #ccc"
            )
        , HA.style "border-radius" "3px"
        , HA.style "padding" "0.2rem 0.4rem"
        , HA.style "min-width" "3.5rem"
        , HA.style "background" background
        ]
        (span
            [ HA.style "font-size" "0.7rem"
            , HA.style "color" "#aaa"
            , HA.style "margin-right" "0.3rem"
            , HA.style "cursor" "pointer"
            , HA.title "この小節の頭から再生"
            , HE.onClick (clickedChord cell.startTicks)
            ]
            [ text (String.fromInt (cell.barIndex + 1)) ]
            :: List.indexedMap (\i c -> chordView clickedChord (tickAtToken i) key (i == currentToken) c) cell.chords
        )


chordView : (Int -> msg) -> Int -> Key -> Bool -> { token : String, result : Result String TokenKind } -> Html msg
chordView clickedChord tick key isCurrent c =
    let
        highlight =
            [ HA.style "padding" "0 0.15rem"
            , HA.style "border-radius" "2px"
            , HA.style "background"
                (if isCurrent then
                    Style.colorHighlight

                 else
                    "transparent"
                )
            ]

        clickable =
            [ HA.style "cursor" "pointer"
            , HA.title "クリックでここから再生"
            , HE.onClick (clickedChord tick)
            ]

        degree =
            case c.result of
                Ok (TChord chord) ->
                    Just (Data.Key.degreeLabel key { spelledFlat = Data.Key.isFlatSpelled c.token } chord)

                _ ->
                    Nothing

        withDegree color =
            span
                ([ HA.style "display" "inline-flex"
                 , HA.style "flex-direction" "column"
                 , HA.style "align-items" "center"
                 , HA.style "margin-right" "0.3rem"
                 ]
                    ++ highlight
                    ++ clickable
                )
                [ span [ HA.style "color" color, HA.style "font-weight" "bold" ] [ text c.token ]
                , case degree of
                    Just d ->
                        span [ HA.style "font-size" "0.6rem", HA.style "color" "#aaa" ] [ text d ]

                    Nothing ->
                        text ""
                ]
    in
    case c.result of
        Ok (TChord _) ->
            withDegree "#2c7a2c"

        Ok _ ->
            withDegree "#888"

        Err reason ->
            span
                ([ HA.style "color" "#e74c3c"
                 , HA.style "text-decoration" "underline wavy"
                 , HA.style "margin-right" "0.3rem"
                 , HA.title reason
                 ]
                    ++ highlight
                    ++ clickable
                )
                [ text c.token ]
