module View.Arrange exposing (Config, view)

import Data.Note exposing (Note)
import Data.Time
import Data.Track exposing (Track, TrackKind(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, option, select, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Svg
import Svg.Attributes as SA


type alias Config msg =
    { selectTrack : Int -> msg
    , addTrack : msg
    , removeTrack : Int -> msg
    , toggleMute : Int -> msg
    , changeInstrument : Int -> String -> msg
    , changeVolume : Int -> String -> msg
    , renameTrack : Int -> String -> msg
    }


view : Config msg -> Int -> Dict String String -> List Track -> Html msg
view config selectedTrackId loadStates tracks =
    div [ HA.style "margin-top" "1rem" ]
        (List.map (trackRow config selectedTrackId loadStates) tracks
            ++ [ button [ HE.onClick config.addTrack, HA.style "margin-top" "0.5rem" ] [ text "+ トラック追加" ] ]
        )


stopClick : msg -> Html.Attribute msg
stopClick msg =
    HE.stopPropagationOn "click" (Decode.succeed ( msg, True ))


trackRow : Config msg -> Int -> Dict String String -> Track -> Html msg
trackRow config selectedTrackId loadStates track =
    let
        selected =
            track.id == selectedTrackId

        loadState =
            Dict.get (Data.Track.instrumentToString track.instrument) loadStates
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
        , volumeSlider config track
        , clipPreview track
        , button [ stopClick (config.removeTrack track.id), HA.title "トラック削除" ] [ text "✕" ]
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
            span [ HA.style "color" "#e67e22", HA.style "font-size" "0.8rem" ] [ text "読込中…" ]

        Just "failed" ->
            span [ HA.style "color" "#e74c3c", HA.style "font-size" "0.8rem" ] [ text "読込失敗" ]

        _ ->
            text ""


muteButton : Config msg -> Track -> Html msg
muteButton config track =
    button
        [ stopClick (config.toggleMute track.id)
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
        , HA.title "ミュート"
        ]
        [ text "M" ]


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


clipPreview : Track -> Html msg
clipPreview track =
    let
        width =
            240

        height =
            32

        totalTicks =
            16 * Data.Time.ticksPerBar

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
