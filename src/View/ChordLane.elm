module View.ChordLane exposing (Config, view)

import Data.ChordTrack exposing (TokenKey, TokenKind(..), TokenSpan)
import Html
import Html.Attributes as HA
import Html.Events
import Json.Decode as Decode
import Set exposing (Set)
import Svg
import Svg.Attributes as SA
import View.Palette as Palette
import View.Style as Style
import View.Theme as Theme


{-| コードトラック選択時のメインペイン用インタラクティブなコードレーン。`View.ChordStrip` と違い
トークン単位で選択・ドラッグ移動できる。背景の mousedown は矩形選択（またはシーク）、
トークン本体の mousedown はそのトークンの選択・ドラッグ開始に使う。
-}
type alias Config msg =
    { pressedToken : TokenKey -> { clientX : Float, clientY : Float, shift : Bool } -> msg
    , pressedLane : { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool } -> msg
    , doubleClickedToken : TokenKey -> msg
    , draggedWhilePressingToken : { clientX : Float, clientY : Float, alt : Bool } -> msg
    , releasedTokenPress : msg
    }


view :
    Config msg
    ->
        { ticksToX : Int -> Float
        , width : Int
        , height : Int
        , playheadTicks : Int
        , rubberBand : Maybe { x : Float, w : Float }
        , selectedKeys : Set TokenKey
        }
    -> List TokenSpan
    -> Html.Html msg
view config opts spans =
    Svg.svg
        [ SA.width (String.fromInt opts.width)
        , SA.height (String.fromInt opts.height)
        , HA.style "display" "block"
        , HA.style "background" Theme.surfaceContainerLow
        , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "cursor" "crosshair"
        , HA.style "touch-action" "none"
        , Html.Events.on "pointerdown" (Decode.map config.pressedLane laneEmptyPressDecoder)
        ]
        (List.concatMap (tokenView config opts) spans
            ++ rubberBandView opts.height opts.rubberBand
            ++ [ playheadLine opts.ticksToX opts.playheadTicks opts.height ]
        )


tokenView :
    Config msg
    ->
        { r
            | ticksToX : Int -> Float
            , playheadTicks : Int
            , height : Int
            , selectedKeys : Set TokenKey
        }
    -> TokenSpan
    -> List (Svg.Svg msg)
tokenView config opts span =
    let
        x =
            opts.ticksToX span.startTicks

        w =
            opts.ticksToX (span.startTicks + span.durationTicks) - x

        selected =
            Set.member span.key opts.selectedKeys

        active =
            opts.playheadTicks >= span.startTicks && opts.playheadTicks < span.startTicks + span.durationTicks

        ( bgFill, borderColor ) =
            if selected then
                ( Style.colorSelection, Theme.selectionDeep )

            else if active then
                ( Theme.highlightContainer, Theme.onHighlightContainer )

            else
                case span.result of
                    Ok TRest ->
                        ( Theme.surfaceContainerHighest, Theme.outlineVariant )

                    Err _ ->
                        ( Theme.errorContainer, Theme.error )

                    _ ->
                        case span.sectionIndex of
                            Just idx ->
                                ( Palette.sectionTint idx, Palette.sectionColor idx )

                            Nothing ->
                                ( Palette.neutral, Theme.outlineVariant )

        textColor =
            if selected then
                Theme.selectionDeep

            else
                case span.sectionIndex of
                    Just idx ->
                        Palette.sectionColor idx

                    Nothing ->
                        Theme.onSurfaceVariant

        textY =
            toFloat opts.height / 2 + 4
    in
    [ Svg.rect
        [ SA.x (String.fromFloat x)
        , SA.y "0"
        , SA.width (String.fromFloat (Basics.max 1 (w - 1)))
        , SA.height (String.fromInt opts.height)
        , SA.fill bgFill
        , SA.stroke borderColor
        , SA.strokeWidth "1"
        , HA.style "cursor" "move"
        , HA.style "touch-action" "none"
        , HA.title span.token
        , HA.attribute "data-pointer-capture" ""
        , Html.Events.stopPropagationOn "pointerdown"
            (Decode.map (\pos -> ( config.pressedToken span.key pos, True )) tokenPressDecoder)
        , Html.Events.stopPropagationOn "pointermove"
            (Decode.map (\pos -> ( config.draggedWhilePressingToken pos, True )) tokenMoveDecoder)
        , Html.Events.stopPropagationOn "pointerup"
            (Decode.succeed ( config.releasedTokenPress, True ))
        , Html.Events.stopPropagationOn "pointercancel"
            (Decode.succeed ( config.releasedTokenPress, True ))
        , Html.Events.onDoubleClick (config.doubleClickedToken span.key)
        ]
        []
    , Svg.text_
        [ SA.x (String.fromFloat (x + 3))
        , SA.y (String.fromFloat textY)
        , SA.fontSize "10"
        , SA.fill textColor
        , SA.pointerEvents "none"
        ]
        [ Svg.text span.token ]
    ]


rubberBandView : Int -> Maybe { x : Float, w : Float } -> List (Svg.Svg msg)
rubberBandView height band =
    case band of
        Nothing ->
            []

        Just r ->
            [ Svg.rect
                [ SA.x (String.fromFloat r.x)
                , SA.y "0"
                , SA.width (String.fromFloat r.w)
                , SA.height (String.fromInt height)
                , SA.fill (Theme.withAlpha 0.15 Theme.primary)
                , SA.stroke Theme.primary
                , SA.strokeDasharray "4 2"
                , SA.pointerEvents "none"
                ]
                []
            ]


playheadLine : (Int -> Float) -> Int -> Int -> Svg.Svg msg
playheadLine ticksToX ticks height =
    Svg.line
        [ SA.x1 (String.fromFloat (ticksToX ticks))
        , SA.y1 "0"
        , SA.x2 (String.fromFloat (ticksToX ticks))
        , SA.y2 (String.fromInt height)
        , SA.stroke Theme.playhead
        , SA.strokeWidth "2"
        ]
        []


laneEmptyPressDecoder : Decode.Decoder { offsetX : Float, offsetY : Float, clientX : Float, clientY : Float, shift : Bool, seekMod : Bool }
laneEmptyPressDecoder =
    Decode.map8
        (\ox oy cx cy sh ctrl meta alt ->
            { offsetX = ox
            , offsetY = oy
            , clientX = cx
            , clientY = cy
            , shift = sh
            , seekMod = ctrl || meta || alt
            }
        )
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)


{-| button フィルタを入れ、右クリックでは発火させない。js/main.js の pointer capture リスナーが button 0 限定なので、
入れないと右ボタンで pendingChordDrag が残留する。
-}
tokenPressDecoder : Decode.Decoder { clientX : Float, clientY : Float, shift : Bool }
tokenPressDecoder =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\button ->
                if button == 0 then
                    Decode.map3 (\cx cy sh -> { clientX = cx, clientY = cy, shift = sh })
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                        (Decode.field "shiftKey" Decode.bool)

                else
                    Decode.fail "not left button"
            )


{-| トークンを押している間のpointermove用。buttonsガードでホバーのみの発火を防ぎ、altKeyも同時に読む。
PianoRoll.elm の noteMoveDecoder と同型。
-}
tokenMoveDecoder : Decode.Decoder { clientX : Float, clientY : Float, alt : Bool }
tokenMoveDecoder =
    Decode.field "buttons" Decode.int
        |> Decode.andThen
            (\buttons ->
                if buttons > 0 then
                    Decode.map3 (\cx cy alt -> { clientX = cx, clientY = cy, alt = alt })
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                        (Decode.field "altKey" Decode.bool)

                else
                    Decode.fail "no button pressed"
            )
