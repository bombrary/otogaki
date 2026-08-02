module View.ChordEditor exposing (Config, view)

import Data.ChordTrack exposing (ChordCell, ChordTrack, TokenKind(..))
import Data.Key exposing (Key)
import Data.Timeline exposing (Timeline)
import Html exposing (Html, button, div, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import View.Palette as Palette


type alias Config msg =
    { changedText : String -> msg
    , toggledMute : msg
    , convertToTrack : msg
    , changedVolume : String -> msg
    }


view : Config msg -> Timeline -> Int -> ChordTrack -> Html msg
view config timeline playheadTicks track =
    div
        [ HA.style "margin-top" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" "1px solid #ddd"
        , HA.style "border-radius" "4px"
        ]
        [ div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap" ]
            [ span [ HA.style "font-size" "0.9rem", HA.style "font-weight" "bold" ] [ text "コード進行" ]
            , span [ HA.style "font-size" "0.75rem", HA.style "color" "#888" ]
                [ text "| で小節区切り（改行は無視されるので自由に使ってOK）、空白で小節内分割。% = 直前のコードを繰返し、_ = 休符、= = 直前のコードを伸ばす" ]
            , button
                [ HE.onClick config.toggledMute
                , HA.style "background"
                    (if track.muted then
                        "#e74c3c"

                     else
                        "#eee"
                    )
                , HA.style "color"
                    (if track.muted then
                        "#fff"

                     else
                        "#333"
                    )
                , HA.title "コードトラックをミュート"
                ]
                [ text "M" ]
            , Html.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "100"
                , HA.value (String.fromInt track.volume)
                , HE.onInput config.changedVolume
                , HA.style "width" "5rem"
                , HA.title ("音量: " ++ String.fromInt track.volume)
                ]
                []
            , button
                [ HE.onClick config.convertToTrack
                , HA.title "コード進行をノートに展開して新しいMIDIトラックを作る"
                ]
                [ text "→ MIDIトラック化" ]
            ]
        , textarea
            [ HA.value track.text
            , HE.onInput config.changedText
            , HA.placeholder "例:\nC | G/B | Am7 | F\nC | G7b5 | % | ="
            , HA.style "width" "98%"
            , HA.style "font-size" "1rem"
            , HA.style "margin-top" "0.4rem"
            , HA.style "font-family" "monospace"
            , HA.style "padding" "0.3rem"
            , HA.style "min-height" "3.2rem"
            ]
            []
        , div
            [ HA.style "display" "flex"
            , HA.style "gap" "2px"
            , HA.style "margin-top" "0.4rem"
            , HA.style "flex-wrap" "wrap"
            ]
            (List.map (cellView timeline playheadTicks) (Data.ChordTrack.cells timeline track))
        ]


cellView : Timeline -> Int -> ChordCell -> Html msg
cellView timeline playheadTicks cell =
    let
        isCurrentBar =
            playheadTicks >= cell.startTicks && playheadTicks < cell.startTicks + cell.lengthTicks

        -- セル内のトークンは小節を等分割するので、位置から逆算できる
        currentToken =
            if isCurrentBar then
                (playheadTicks - cell.startTicks) * List.length cell.chords // Basics.max 1 cell.lengthTicks

            else
                -1

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
            ]
            [ text (String.fromInt (cell.barIndex + 1)) ]
            :: List.indexedMap (\i c -> chordView key (i == currentToken) c) cell.chords
        )


chordView : Key -> Bool -> { token : String, result : Result String TokenKind } -> Html msg
chordView key isCurrent c =
    let
        highlight =
            if isCurrent then
                [ HA.style "background" "#ffd54f"
                , HA.style "border-radius" "2px"
                , HA.style "padding" "0 0.15rem"
                ]

            else
                []

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
                )
                [ text c.token ]
