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
    , dangerButton
    , divider
    , focusCss
    , headingText
    , hintText
    , labelText
    , toggleButton
    )

{-| button and text style helpers shared across all View modules. -}

import Html
import Html.Attributes as HA


colorPrimary : String
colorPrimary =
    "#4a90d9"


colorDanger : String
colorDanger =
    "#e74c3c"


colorSelection : String
colorSelection =
    "#d6336c"


colorHighlight : String
colorHighlight =
    "#ffd54f"


colorLoop : String
colorLoop =
    "#5e60ce"


colorLoopHandle : String
colorLoopHandle =
    "#3f3d9e"


baseButton : List (Html.Attribute msg)
baseButton =
    [ HA.style "padding" "0.3rem 0.6rem"
    , HA.style "border" "1px solid #ccc"
    , HA.style "border-radius" "4px"
    , HA.style "background" "#fff"
    , HA.style "font-size" "0.85rem"
    , HA.style "cursor" "pointer"
    ]


toggleButton : Bool -> List (Html.Attribute msg)
toggleButton isOn =
    baseButton
        ++ [ HA.style "background"
                (if isOn then
                    colorPrimary

                 else
                    "#eee"
                )
           , HA.style "color"
                (if isOn then
                    "#fff"

                 else
                    "#333"
                )
           , HA.style "border-color"
                (if isOn then
                    colorPrimary

                 else
                    "#ccc"
                )
           ]


dangerButton : List (Html.Attribute msg)
dangerButton =
    baseButton
        ++ [ HA.style "color" colorDanger
           , HA.style "border-color" colorDanger
           ]


armedDangerButton : List (Html.Attribute msg)
armedDangerButton =
    baseButton
        ++ [ HA.style "background" colorDanger
           , HA.style "color" "#fff"
           , HA.style "border-color" colorDanger
           ]


headingText : List (Html.Attribute msg)
headingText =
    [ HA.style "font-size" "0.9rem"
    , HA.style "font-weight" "bold"
    ]


labelText : List (Html.Attribute msg)
labelText =
    [ HA.style "font-size" "0.85rem" ]


hintText : List (Html.Attribute msg)
hintText =
    [ HA.style "font-size" "0.75rem"
    , HA.style "color" "#888"
    ]


divider : Html.Html msg
divider =
    Html.div
        [ HA.style "border-left" "1px solid #ddd"
        , HA.style "height" "1.2rem"
        , HA.style "margin" "0 0.2rem"
        ]
        []


focusCss : Html.Html msg
focusCss =
    Html.node "style"
        []
        [ Html.text
            ("button:focus-visible, [tabindex]:focus-visible, input:focus-visible, select:focus-visible { outline: 2px solid "
                ++ colorPrimary
                ++ "; outline-offset: 1px; }"
            )
        ]


badge : String -> List (Html.Attribute msg)
badge tone =
    let
        ( bg, fg ) =
            case tone of
                "loading" ->
                    ( "#fdf0e4", "#e67e22" )

                "failed" ->
                    ( "#fdecea", "#e74c3c" )

                "success" ->
                    ( "#e8f8ee", "#2c7a2c" )

                _ ->
                    ( "#eee", "#333" )
    in
    [ HA.style "padding" "0.1rem 0.4rem"
    , HA.style "border-radius" "8px"
    , HA.style "font-size" "0.75rem"
    , HA.style "background" bg
    , HA.style "color" fg
    ]
