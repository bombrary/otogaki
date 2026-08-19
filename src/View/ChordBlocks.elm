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
View.ChordLane とロジック（デコーダ）を共有する。grid（8列）で折り返すため clientX 差分での
座標換算が使えず、代わりにセルの pointerenter でドロップ先小節を特定する。
-}
view : Config msg -> Timeline -> Int -> Set TokenKey -> Bool -> Maybe Int -> ChordTrack -> Html msg
view config timeline playheadTicks selectedKeys dragActive dragTarget track =
    div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "repeat(8, minmax(0, 1fr))"
        , HA.style "gap" "2px"
        , HA.style "margin-top" "0.4rem"
        , HA.style "user-select" "none"
        , HA.style "-webkit-user-select" "none"
        , HA.style "cursor"
            (if dragTarget /= Nothing then
                "grabbing"

             else
                "default"
            )
        , HE.on "pointermove" (Decode.map config.draggedWhilePressing View.ChordLane.tokenMoveDecoder)
        , HE.on "pointerup" (Decode.succeed config.releasedPress)
        , HE.on "pointercancel" (Decode.succeed config.releasedPress)
        , HE.on "pointerleave" (Decode.succeed config.releasedPress)
        ]
        (List.map (cellView config timeline playheadTicks selectedKeys dragActive dragTarget) (Data.ChordTrack.cells timeline track))


cellView : Config msg -> Timeline -> Int -> Set TokenKey -> Bool -> Maybe Int -> ChordCell -> Html msg
cellView config timeline playheadTicks selectedKeys dragActive dragTarget cell =
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

        isDropTarget =
            dragTarget == Just cell.barIndex
    in
    div
        [ HA.style "border-radius" "3px"
        , HA.style "padding" "0.2rem 0.4rem"
        , HA.style "min-width" "0"
        , HA.style "background" background
        , HA.style "outline"
            (if isDropTarget then
                "2px solid " ++ Theme.primary

             else
                "none"
            )
        , HA.style "display" "flex"
        , HA.style "flex-wrap" "wrap"
        , HA.style "align-items" "flex-start"
        , HA.style "align-content" "flex-start"
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
                    chordView config ( cell.barIndex, i ) (tickAtToken i) key (i == currentToken) (Set.member ( cell.barIndex, i ) selectedKeys) dragActive c
                )
                cell.chords
        )


chordView : Config msg -> TokenKey -> Int -> Key -> Bool -> Bool -> Bool -> { token : String, result : Result String TokenKind } -> Html msg
chordView config tokenKey tick key isCurrent isSelected dragActive c =
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
            , HA.style "opacity"
                (if isSelected && dragActive then
                    "0.6"

                 else
                    "1"
                )
            ]

        clickable =
            [ HA.class "m3-btn"
            , HA.style "cursor" "pointer"
            , HA.style "touch-action" "none"
            , HA.style "user-select" "none"
            , HA.style "-webkit-user-select" "none"
            , HA.title "クリックでここから再生（ダブルクリックで運指を選ぶ、ドラッグで入れ替え）"
            , HA.attribute "data-pointer-release-capture" ""
            , HE.onClick (config.clickedChord tick)
            , HE.onDoubleClick (config.doubleClickedToken tokenKey)
            , HE.custom "pointerdown"
                (Decode.map
                    (\pos -> { message = config.pressedToken tokenKey pos, stopPropagation = True, preventDefault = True })
                    View.ChordLane.tokenPressDecoder
                )
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
                 , HA.style "min-width" "0"
                 , HA.style "max-width" "100%"
                 ]
                    ++ highlight
                    ++ clickable
                )
                [ span (HA.style "overflow-wrap" "anywhere" :: HA.style "color" color :: Theme.typeTitleSmall) [ text c.token ]
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
                 , HA.style "min-width" "0"
                 , HA.style "max-width" "100%"
                 , HA.style "overflow-wrap" "anywhere"
                 , HA.title reason
                 ]
                    ++ highlight
                    ++ clickable
                )
                [ text c.token ]
