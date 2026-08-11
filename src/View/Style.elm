module View.Style exposing
    ( armedDangerButton
    , badge
    , baseButton
    , colorDanger
    , colorHighlight
    , colorLoop
    , colorLoopHandle
    , colorPrimary
    , colorSelection
    , colorViewRange
    , dangerButton
    , divider
    , focusCss
    , headingText
    , hintText
    , labelText
    , toggleButton
    )

{-| button and text style helpers shared across all View modules.

Material Design 3 のトークン（View.Theme）をラップする層。公開APIの名前・型は変えていないので、
ここを書き換えるだけで全 View に一斉に反映される。

-}

import Html
import Html.Attributes as HA
import View.Theme as Theme


colorPrimary : String
colorPrimary =
    Theme.primary


colorDanger : String
colorDanger =
    Theme.error


colorSelection : String
colorSelection =
    Theme.selection


colorHighlight : String
colorHighlight =
    Theme.highlight


colorLoop : String
colorLoop =
    Theme.loop


colorLoopHandle : String
colorLoopHandle =
    Theme.loopHandle


{-| リージョン上に示すピアノロールの可視範囲枠の色。ループ帯（colorLoop）・プレイヘッド（Theme.playhead）と区別するため highlight を使う。
-}
colorViewRange : String
colorViewRange =
    Theme.highlight


{-| M3 の text button。境界線も塗りもなく、文字は枠 primary、形状 pill。
`m3-btn` クラスで hover/active の state layer（View.Theme.globalCss 定義）が付く。
-}
baseButton : List (Html.Attribute msg)
baseButton =
    [ HA.class "m3-btn"
    , HA.style "padding" "0.3rem 0.6rem"
    , HA.style "border" "1px solid transparent"
    , HA.style "border-radius" Theme.shapeFull
    , HA.style "background" "transparent"
    , HA.style "color" Theme.primary
    , HA.style "cursor" "pointer"
    ]
        ++ Theme.typeLabelLarge


{-| ON: filled button（primary 背景 + onPrimary 文字）。OFF: baseButton と同じ outlined。
-}
toggleButton : Bool -> List (Html.Attribute msg)
toggleButton isOn =
    Theme.typeLabelLarge
        ++ [ HA.class "m3-btn"
           , HA.style "padding" "0.3rem 0.6rem"
           , HA.style "border-radius" Theme.shapeFull
           , HA.style "cursor" "pointer"
           , HA.style "border" "1px solid transparent"
           , HA.style "background"
                (if isOn then
                    Theme.primary

                 else
                    "transparent"
                )
           , HA.style "color"
                (if isOn then
                    Theme.onPrimary

                 else
                    Theme.primary
                )
           ]


{-| outlined error button（文字・枠が error）。削除の1段階目など、確認前の危険操作に使う。
-}
dangerButton : List (Html.Attribute msg)
dangerButton =
    [ HA.class "m3-btn"
    , HA.style "padding" "0.3rem 0.6rem"
    , HA.style "border" "1px solid transparent"
    , HA.style "border-radius" Theme.shapeFull
    , HA.style "background" "transparent"
    , HA.style "color" Theme.error
    , HA.style "cursor" "pointer"
    ]
        ++ Theme.typeLabelLarge


{-| filled error button（削除確定など、実行直前の確認済み状態）。
-}
armedDangerButton : List (Html.Attribute msg)
armedDangerButton =
    [ HA.class "m3-btn"
    , HA.style "padding" "0.3rem 0.6rem"
    , HA.style "border" "1px solid transparent"
    , HA.style "border-radius" Theme.shapeFull
    , HA.style "background" Theme.error
    , HA.style "color" Theme.onError
    , HA.style "cursor" "pointer"
    ]
        ++ Theme.typeLabelLarge


headingText : List (Html.Attribute msg)
headingText =
    Theme.typeTitleSmall


labelText : List (Html.Attribute msg)
labelText =
    Theme.typeBodyMedium


hintText : List (Html.Attribute msg)
hintText =
    Theme.typeBodySmall ++ [ HA.style "color" Theme.onSurfaceVariant ]


divider : Html.Html msg
divider =
    Html.div
        [ HA.style "border-left" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "height" "1.2rem"
        , HA.style "margin" "0 0.2rem"
        ]
        []


focusCss : Html.Html msg
focusCss =
    Theme.globalCss


badge : String -> List (Html.Attribute msg)
badge tone =
    let
        ( bg, fg ) =
            case tone of
                "loading" ->
                    ( Theme.pendingContainer, Theme.onPendingContainer )

                "failed" ->
                    ( Theme.errorContainer, Theme.onErrorContainer )

                "success" ->
                    ( Theme.successContainer, Theme.onSuccessContainer )

                _ ->
                    ( Theme.surfaceContainerHigh, Theme.onSurfaceVariant )
    in
    Theme.typeBodySmall
        ++ [ HA.style "padding" "0.1rem 0.4rem"
           , HA.style "border-radius" Theme.shapeS
           , HA.style "background" bg
           , HA.style "color" fg
           ]
