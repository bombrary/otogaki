module View.ScaleGuide exposing (Config, view)

{-| キーのスケールトーンとダイアトニックコードを常時参照できるコンパクトなガイド。
読み取り専用（クリックで進行に挿入等はしない）。左ペインに details パネルとして常設する。
-}

import Data.Chord.Format as Format
import Data.Key as Key exposing (Key)
import Html exposing (Html, div, option, select, span, text)
import Html.Attributes as HA
import Html.Events as HE
import View.Style as Style
import View.Theme as Theme


type alias Config msg =
    { changedTonic : String -> msg
    , changedMode : String -> msg
    }


{-| effectiveKey は override があればそれ、無ければセクション追従で決まった実効キー（呼び出し側が解決する）。
override は Nothing なら「追従」表示、Just ならそのキーが手動選択されている。
-}
view : Config msg -> { effectiveKey : Key, override : Maybe Key } -> Html msg
view config { effectiveKey, override } =
    let
        preferFlat =
            Key.isFlatSpelled (Key.tonicName effectiveKey.tonic)
    in
    Html.details
        [ HA.attribute "open" ""
        , HA.style "margin-top" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "border-radius" "4px"
        ]
        [ Html.summary (HA.style "cursor" "pointer" :: Style.headingText) [ text "スケールガイド" ]
        , keySelector config effectiveKey override
        , toneRow preferFlat effectiveKey
        , chordTable preferFlat effectiveKey
        ]


keySelector : Config msg -> Key -> Maybe Key -> Html msg
keySelector config effectiveKey override =
    div
        [ HA.style "display" "flex"
        , HA.style "gap" "0.5rem"
        , HA.style "align-items" "center"
        , HA.style "flex-wrap" "wrap"
        , HA.style "margin-top" "0.3rem"
        ]
        ([ span [ HA.style "font-size" "0.85rem" ] [ text "キー:" ]
         , select [ HE.onInput config.changedTonic ]
            (option [ HA.value "follow", HA.selected (override == Nothing) ] [ text "追従" ]
                :: (Key.tonicOptions
                        |> List.map
                            (\( tonic, label_ ) ->
                                option
                                    [ HA.value (String.fromInt tonic)
                                    , HA.selected (Maybe.map .tonic override == Just tonic)
                                    ]
                                    [ text label_ ]
                            )
                   )
            )
         ]
            ++ (case override of
                    Just ov ->
                        [ modeSelector config ov ]

                    Nothing ->
                        []
               )
            ++ [ span (HA.style "margin-left" "0.3rem" :: Style.hintText) [ text ("(" ++ Key.toString effectiveKey ++ ")") ] ]
        )


modeSelector : Config msg -> Key -> Html msg
modeSelector config key =
    select [ HE.onInput config.changedMode ]
        [ option [ HA.value "major", HA.selected (key.mode == Key.Major) ] [ text "Major" ]
        , option [ HA.value "minor", HA.selected (key.mode == Key.Minor) ] [ text "Minor" ]
        , option [ HA.value "dorian", HA.selected (key.mode == Key.Dorian) ] [ text "ドリアン" ]
        , option [ HA.value "phrygian", HA.selected (key.mode == Key.Phrygian) ] [ text "フリジアン" ]
        , option [ HA.value "lydian", HA.selected (key.mode == Key.Lydian) ] [ text "リディアン" ]
        , option [ HA.value "mixolydian", HA.selected (key.mode == Key.Mixolydian) ] [ text "ミクソリディアン" ]
        , option [ HA.value "locrian", HA.selected (key.mode == Key.Locrian) ] [ text "ロクリアン" ]
        , option [ HA.value "harmonicMinor", HA.selected (key.mode == Key.HarmonicMinor) ] [ text "ハーモニックマイナー" ]
        ]


toneRow : Bool -> Key -> Html msg
toneRow preferFlat key =
    div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "margin-top" "0.4rem", HA.style "flex-wrap" "wrap" ]
        (Key.scaleTones key
            |> List.indexedMap
                (\i pitch ->
                    span
                        (if i == 0 then
                            [ HA.style "color" Theme.primary, HA.style "font-weight" "700" ]

                         else
                            []
                        )
                        [ text (Format.pitchName preferFlat pitch) ]
                )
        )


chordTable : Bool -> Key -> Html msg
chordTable preferFlat key =
    div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "auto auto auto"
        , HA.style "gap" "0.15rem 0.6rem"
        , HA.style "margin-top" "0.4rem"
        , HA.style "font-size" "0.75rem"
        ]
        (Key.diatonicChords key
            |> List.concatMap
                (\deg ->
                    [ span [ HA.style "color" Theme.onSurfaceVariant ] [ text (Key.degreeLabel key { spelledFlat = preferFlat } deg.triad) ]
                    , span [] [ text (Format.formatPlain { preferFlat = preferFlat } deg.triad) ]
                    , span [] [ text (Format.formatPlain { preferFlat = preferFlat } deg.seventh) ]
                    ]
                )
        )
