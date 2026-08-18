module View.Toast exposing (Tone(..), Toast, view)

import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events as HE
import View.Theme as Theme


{-| 画面下部に一時的に表示する通知（M3 スナックバー相当）。
自動消滅のタイマーと id 照合は Main.elm 側（showToast/DismissedToast）が担う。
-}
type Tone
    = Success
    | Error


type alias Toast =
    { id : Int
    , tone : Tone
    , message : String
    }


view : (Int -> msg) -> Toast -> Html msg
view onDismiss toast =
    let
        ( background, foreground ) =
            case toast.tone of
                Success ->
                    ( Theme.inverseSurface, Theme.inverseOnSurface )

                Error ->
                    ( Theme.errorContainer, Theme.onErrorContainer )
    in
    div
        [ HA.style "position" "fixed"
        , HA.style "bottom" "1.5rem"
        , HA.style "left" "50%"
        , HA.style "transform" "translateX(-50%)"
        , HA.style "background" background
        , HA.style "color" foreground
        , HA.style "padding" "0.75rem 1.25rem"
        , HA.style "border-radius" Theme.shapeS
        , HA.style "box-shadow" Theme.elevation3
        , HA.style "z-index" "1100"
        , HA.style "cursor" "pointer"
        , HA.style "max-width" "90vw"
        , HA.style "font-size" "0.875rem"
        , HA.title "クリックで閉じる"
        , HE.onClick (onDismiss toast.id)
        ]
        [ text toast.message ]
