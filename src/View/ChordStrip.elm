module View.ChordStrip exposing (Handlers, height, view)

import Data.ChordTrack exposing (ChordSpan)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Svg
import Svg.Attributes as SA
import View.Palette as Palette
import View.Theme as Theme


{-| 既存呼び出し側（ピアノロール本体・セクションバー）が使うデフォルトの高さ。呼び出し側は `view` の
`height` 引数にこの値を渡すか、より大きい値を渡してコード帯を拡張できる（例: コード進行トラック
メインペイン）。
-}
height : Int
height =
    18


{-| クリック・ダブルクリックのハンドラをまとめたレコード。`doubleClickedChord` を `Nothing` にした
呼び出し側ではダブルクリックのイベントリスナー自体を付けない（コード進行専用トラックの ChordLane 側で
別途ダブルクリック編集を持つ場面など、意図的に無効化したい場合に使う）。
-}
type alias Handlers msg =
    { clickedChord : Int -> msg
    , doubleClickedChord : Maybe (Int -> msg)
    }


{-| ピアノロール・セクションバー・コード進行トラックのメインペインで共通して使うコード帯。
呼び出し側の座標系（pxPerSixteenth ベースの線形変換や、セクションバーの拍子非線形変換など）を
`ticksToX` として外から注入することで、どの場面でも同じ見た目・同じハイライトロジックを保てる。
`height` も呼び出し側が指定する（コード進行トラックメインペインだけ大きくするなど）。
spans が空なら何も描画しない（呼び出し側が段の有無を `height` と一致させてレイアウトを組む契約）。
-}
view : Handlers msg -> { ticksToX : Int -> Float, width : Int, playheadTicks : Int, height : Int } -> List ChordSpan -> Html msg
view handlers opts spans =
    if List.isEmpty spans then
        Html.text ""

    else
        Svg.svg
            [ SA.width (String.fromInt opts.width)
            , SA.height (String.fromInt opts.height)
            , HA.style "display" "block"
            , HA.style "background" Theme.surfaceContainerLow
            , HA.style "border-bottom" ("1px solid " ++ Theme.outlineVariant)
            ]
            (List.concatMap (spanView handlers opts.ticksToX opts.playheadTicks opts.height) spans
                ++ [ playheadLine opts.ticksToX opts.playheadTicks opts.height ]
            )


{-| 個々のコードを矩形＋セクション色で塗り、再生中ならハイライトする。クリックでその位置へシークする。
`handlers.doubleClickedChord` が Just のときだけダブルクリックでそのコードを編集できる（FormPicker を開く）。
幅は `ticksToX (start + duration) - ticksToX start` で求める（`ticksToX` が拍子変更をまたぐと非線形になる場合（セクションバー）でも
正しい幅になるよう、`duration` を直接変換しない）。
-}
spanView : Handlers msg -> (Int -> Float) -> Int -> Int -> ChordSpan -> List (Svg.Svg msg)
spanView handlers ticksToX playheadTicks stripHeight span =
    let
        x =
            ticksToX span.startTicks

        w =
            ticksToX (span.startTicks + span.durationTicks) - x

        active =
            playheadTicks >= span.startTicks && playheadTicks < span.startTicks + span.durationTicks

        bgFill =
            if active then
                Theme.highlightContainer

            else
                case span.sectionIndex of
                    Just idx ->
                        Palette.sectionTint idx

                    Nothing ->
                        Palette.neutral

        borderColor =
            if active then
                Theme.onHighlightContainer

            else
                "transparent"

        textColor =
            case span.sectionIndex of
                Just idx ->
                    Palette.sectionColor idx

                Nothing ->
                    Theme.onSurfaceVariant

        textY =
            toFloat stripHeight / 2 + 4

        doubleClickAttrs =
            case handlers.doubleClickedChord of
                Just f ->
                    [ Html.Events.onDoubleClick (f span.startTicks) ]

                Nothing ->
                    []
    in
    [ Svg.rect
        ([ SA.x (String.fromFloat x)
         , SA.y "0"
         , SA.width (String.fromFloat (Basics.max 1 (w - 1)))
         , SA.height (String.fromInt stripHeight)
         , SA.fill bgFill
         , SA.stroke borderColor
         , SA.strokeWidth "1"
         , SA.cursor "pointer"
         , SA.class "m3-hit"
         , HA.title span.name
         , Html.Events.onClick (handlers.clickedChord span.startTicks)
         ]
            ++ doubleClickAttrs
        )
        []
    , Svg.text_
        [ SA.x (String.fromFloat (x + 3))
        , SA.y (String.fromFloat textY)
        , SA.fontSize "10"
        , SA.fill textColor
        , SA.pointerEvents "none"
        ]
        [ Svg.text span.name ]
    ]


playheadLine : (Int -> Float) -> Int -> Int -> Svg.Svg msg
playheadLine ticksToX ticks stripHeight =
    Svg.line
        [ SA.x1 (String.fromFloat (ticksToX ticks))
        , SA.y1 "0"
        , SA.x2 (String.fromFloat (ticksToX ticks))
        , SA.y2 (String.fromInt stripHeight)
        , SA.stroke Theme.playhead
        , SA.strokeWidth "2"
        ]
        []
