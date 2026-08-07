module View.Arrange exposing (Config, view)

import Data.Note exposing (Note)
import Data.Time
import Data.Track exposing (Track, TrackKind(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, option, select, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Set exposing (Set)
import Svg
import Svg.Attributes as SA
import View.Style as Style


type alias Config msg =
    { selectTrack : Int -> msg
    , addTrack : msg
    , removeTrack : Int -> msg
    , toggleMute : Int -> msg
    , changeInstrument : Int -> String -> msg
    , changeVolume : Int -> String -> msg
    , renameTrack : Int -> String -> msg
    , toggledGhost : Int -> msg
    }


view : Config msg -> Int -> Int -> Dict String String -> Set Int -> Maybe Int -> List Track -> Html msg
view config totalBars selectedTrackId loadStates ghostTrackIds pendingDeleteId tracks =
    Html.details
        [ HA.attribute "open" "", HA.style "margin-top" "1rem" ]
        (Html.summary (HA.style "cursor" "pointer" :: Style.headingText) [ text "トラック" ]
            :: List.map (trackRow config totalBars selectedTrackId loadStates ghostTrackIds pendingDeleteId) tracks
            ++ [ button (Style.baseButton ++ [ HE.onClick config.addTrack, HA.style "margin-top" "0.5rem" ]) [ text "+ トラック" ] ]
        )


stopClick : msg -> Html.Attribute msg
stopClick msg =
    HE.stopPropagationOn "click" (Decode.succeed ( msg, True ))


trackRow : Config msg -> Int -> Int -> Dict String String -> Set Int -> Maybe Int -> Track -> Html msg
trackRow config totalBars selectedTrackId loadStates ghostTrackIds pendingDeleteId track =
    let
        selected =
            track.id == selectedTrackId

        loadState =
            Dict.get (Data.Track.instrumentToString track.instrument) loadStates

        isGhost =
            Set.member track.id ghostTrackIds
    in
    div
        [ HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "gap" "0.5rem"
        , HA.style "padding" "0.25rem 0.5rem"
        , HA.style "background"
            (if selected then
                "#eef4fb"

             else
                "#fff"
            )
        , HA.style "border"
            (if selected then
                "1px solid #4a90d9"

             else
                "1px solid #ddd"
            )
        , HA.style "border-radius" "4px"
        , HA.style "margin-bottom" "2px"
        , HA.style "cursor" "pointer"
        , HE.onClick (config.selectTrack track.id)
        ]
        [ input
            [ HA.value track.name
            , HE.onInput (config.renameTrack track.id)
            , stopClick (config.selectTrack track.id)
            , HA.style "width" "6rem"
            , HA.style "font-size" "0.9rem"
            , HA.style "border" "1px solid transparent"
            , HA.style "background" "transparent"
            , HA.title "トラック名を編集"
            ]
            []
        , instrumentSelect config track
        , loadBadge loadState
        , muteButton config track
        , ghostButton config isGhost track
        , volumeSlider config track
        , clipPreview totalBars track
        , Style.divider
        , if pendingDeleteId == Just track.id then
            button
                (Style.armedDangerButton
                    ++ [ stopClick (config.removeTrack track.id)
                       , HA.title "トラック削除"
                       , HA.attribute "aria-label" "トラックを本当に削除"
                       , HA.style "min-width" "6rem"
                       , HA.style "text-align" "center"
                       ]
                )
                [ text "本当に削除？" ]

          else
            button
                (Style.dangerButton
                    ++ [ stopClick (config.removeTrack track.id)
                       , HA.title "トラック削除"
                       , HA.attribute "aria-label" "トラックを削除"
                       , HA.style "min-width" "6rem"
                       , HA.style "text-align" "center"
                       ]
                )
                [ text "✕" ]
        ]


instrumentSelect : Config msg -> Track -> Html msg
instrumentSelect config track =
    select
        [ HE.onInput (config.changeInstrument track.id)
        , stopClick (config.selectTrack track.id)
        ]
        (List.map
            (\inst ->
                option
                    [ HA.value (Data.Track.instrumentToString inst)
                    , HA.selected (inst == track.instrument)
                    ]
                    [ text (Data.Track.instrumentLabel inst) ]
            )
            Data.Track.allInstruments
        )


loadBadge : Maybe String -> Html msg
loadBadge state =
    case state of
        Just "loading" ->
            span (Style.badge "loading") [ text "⏳ 読込中…" ]

        Just "failed" ->
            span (Style.badge "failed") [ text "⚠ 読込失敗" ]

        _ ->
            text ""


muteButton : Config msg -> Track -> Html msg
muteButton config track =
    button
        (Style.toggleButton track.muted
            ++ [ stopClick (config.toggleMute track.id)
               , HA.title "ミュート"
               , HA.attribute "aria-label" "ミュート"
               ]
        )
        [ text "M" ]


ghostButton : Config msg -> Bool -> Track -> Html msg
ghostButton config isGhost track =
    button
        (Style.toggleButton isGhost
            ++ [ stopClick (config.toggledGhost track.id)
               , HA.title "ピアノロールにこのトラックを透けて重ね表示"
               , HA.attribute "aria-label" "ゴースト表示を切替"
               ]
        )
        [ text "👻" ]


volumeSlider : Config msg -> Track -> Html msg
volumeSlider config track =
    input
        [ HA.type_ "range"
        , HA.min "0"
        , HA.max "100"
        , HA.value (String.fromInt track.volume)
        , HE.onInput (config.changeVolume track.id)
        , stopClick (config.selectTrack track.id)
        , HA.style "width" "70px"
        , HA.title ("音量 " ++ String.fromInt track.volume)
        ]
        []


notesOf : TrackKind -> List Note
notesOf kind =
    case kind of
        NoteTrack notes ->
            notes

        DrumTrack notes ->
            notes


clipPreview : Int -> Track -> Html msg
clipPreview totalBars track =
    let
        width =
            240

        height =
            32

        totalTicks =
            Basics.max 1 totalBars * Data.Time.ticksPerBar

        noteRect note =
            let
                x =
                    toFloat note.start / toFloat totalTicks * width

                w =
                    Basics.max 1 (toFloat note.duration / toFloat totalTicks * width)

                y =
                    toFloat (108 - note.pitch) / 72 * height
            in
            Svg.rect
                [ SA.x (String.fromFloat x)
                , SA.y (String.fromFloat y)
                , SA.width (String.fromFloat w)
                , SA.height "2"
                , SA.fill "#4a90d9"
                ]
                []
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        , HA.style "background" "#fafafa"
        , HA.style "border" "1px solid #eee"
        ]
        (List.map noteRect (notesOf track.kind))
