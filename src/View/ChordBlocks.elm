module View.ChordBlocks exposing (Config, view)

import Data.ChordTrack exposing (ChordCell, ChordTrack, TokenKey, TokenKind(..))
import Data.Key exposing (Key)
import Data.Timeline exposing (Timeline)
import Html exposing (Html, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Set exposing (Set)
import View.ChordLane
import View.Palette as Palette
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { clickedChord : Int -> msg
    , doubleClickedToken : TokenKey -> msg
    , pressedToken : TokenKey -> { clientX : Float, clientY : Float, shift : Bool } -> msg
    , draggedWhilePressing : { clientX : Float, clientY : Float, alt : Bool } -> msg
    , draggedOverBar : Int -> msg
    , releasedPress : msg
    }


{-| 小節ごとのブロックとしてコード進行を表示する。旧 ChordEditor.cellsView/cellView/chordView
（`e325f2e` で削除される前の実装）の移植。ピアノロール風の一直線表示（View.ChordStrip）と
切り替えて使うトグル表示。トークンのドラッグ入れ替え（swap、Data.ChordTrack.moveTokens）は
View.ChordLane とロジック（デコーダ）を共有する。flex-wrap で折り返すため clientX 差分での
座標換算が使えず、代わりにセルの pointerenter でドロップ先小節を特定する。
-}
view : Config msg -> Timeline -> Int -> Set TokenKey -> ChordTrack -> Html msg
view config timeline playheadTicks selectedKeys track =
    div
        [ HA.style "display" "flex"
        , HA.style "gap" "2px"
        , HA.style "margin-top" "0.4rem"
        , HA.style "flex-wrap" "wrap"
        , HE.on "pointermove" (Decode.map config.draggedWhilePressing View.ChordLane.tokenMoveDecoder)
        , HE.on "pointerup" (Decode.succeed config.releasedPress)
        , HE.on "pointercancel" (Decode.succeed config.releasedPress)
        , HE.on "pointerleave" (Decode.succeed config.releasedPress)
        ]
        (List.map (cellView config timeline playheadTicks selectedKeys) (Data.ChordTrack.cells timeline track))


cellView : Config msg -> Timeline -> Int -> Set TokenKey -> ChordCell -> Html msg
cellView config timeline playheadTicks selectedKeys cell =
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
                Theme.highlightContainer

            else
                case Data.Timeline.sectionIndexAt cell.startTicks timeline of
                    Just idx ->
                        Palette.sectionTint idx

                    Nothing ->
                        Palette.neutral
    in
    div
        [ HA.style "border-radius" "3px"
        , HA.style "padding" "0.2rem 0.4rem"
        , HA.style "min-width" "3.5rem"
        , HA.style "background" background
        , HE.on "pointerenter" (Decode.map (\_ -> config.draggedOverBar cell.barIndex) View.ChordLane.tokenMoveDecoder)
        ]
        (span
            [ HA.class "m3-btn"
            , HA.style "font-size" "0.7rem"
            , HA.style "color" Theme.onSurfaceVariant
            , HA.style "margin-right" "0.3rem"
            , HA.style "cursor" "pointer"
            , HA.title "この小節の頭から再生"
            , HE.onClick (config.clickedChord cell.startTicks)
            ]
            [ text (String.fromInt (cell.barIndex + 1)) ]
            :: List.indexedMap
                (\i c ->
                    chordView config ( cell.barIndex, i ) (tickAtToken i) key (i == currentToken) (Set.member ( cell.barIndex, i ) selectedKeys) c
                )
                cell.chords
        )


chordView : Config msg -> TokenKey -> Int -> Key -> Bool -> Bool -> { token : String, result : Result String TokenKind } -> Html msg
chordView config tokenKey tick key isCurrent isSelected c =
    let
        highlight =
            [ HA.style "padding" "0 0.15rem"
            , HA.style "border-radius" "2px"
            , HA.style "background"
                (if isSelected then
                    Style.colorSelection

                 else if isCurrent then
                    Style.colorHighlight

                 else
                    "transparent"
                )
            ]

        clickable =
            [ HA.class "m3-btn"
            , HA.style "cursor" "pointer"
            , HA.style "touch-action" "none"
            , HA.title "クリックでここから再生（ダブルクリックで運指を選ぶ、ドラッグで入れ替え）"
            , HA.attribute "data-pointer-release-capture" ""
            , HE.onClick (config.clickedChord tick)
            , HE.onDoubleClick (config.doubleClickedToken tokenKey)
            , HE.stopPropagationOn "pointerdown"
                (Decode.map (\pos -> ( config.pressedToken tokenKey pos, True )) View.ChordLane.tokenPressDecoder)
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
                [ span (HA.style "color" color :: Theme.typeTitleSmall) [ text c.token ]
                , case degree of
                    Just d ->
                        span [ HA.style "font-size" "0.6rem", HA.style "color" Theme.onSurfaceVariant ] [ text d ]

                    Nothing ->
                        text ""
                ]
    in
    case c.result of
        Ok (TChord _) ->
            withDegree Theme.success

        Ok _ ->
            withDegree Theme.onSurfaceVariant

        Err reason ->
            span
                ([ HA.style "color" Theme.error
                 , HA.style "text-decoration" "underline wavy"
                 , HA.style "margin-right" "0.3rem"
                 , HA.title reason
                 ]
                    ++ highlight
                    ++ clickable
                )
                [ text c.token ]
