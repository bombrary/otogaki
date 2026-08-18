module View.Arrange exposing (Config, view)

import Data.ChordTrack exposing (ChordTrack)
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
import View.Theme as Theme


type alias Config msg =
    { selectTrack : Int -> msg
    , addTrack : msg
    , removeTrack : Int -> msg
    , toggleMute : Int -> msg
    , changeInstrument : Int -> String -> msg
    , changeVolume : Int -> String -> msg
    , renameTrack : Int -> String -> msg
    , toggledGhost : Int -> msg
    , chordRow : ChordRowConfig msg
    }


{-| トラック一覧の先頭に置く「コード進行」行用のメッセージ。ChordTrack は id を持たない単一のトラックなので、
通常トラックの Config とは別に、id 引数のない形（ChordEditor.Config と同じ体系）で持つ。
-}
type alias ChordRowConfig msg =
    { select : msg
    , toggleMute : msg
    , changeInstrument : String -> msg
    , changeVolume : String -> msg
    , toggledGhost : msg
    }


view : Config msg -> Int -> Int -> Dict String String -> Set Int -> Maybe Int -> ChordTrack -> List Track -> Html msg
view config totalBars selectedTrackId loadStates ghostTrackIds pendingDeleteId chordTrack tracks =
    Html.details
        [ HA.attribute "open" "", HA.style "margin-top" "1rem" ]
        (Html.summary (HA.style "cursor" "pointer" :: Style.headingText) [ text "トラック" ]
            :: chordTrackRow config selectedTrackId loadStates ghostTrackIds chordTrack
            :: List.map (trackRow config totalBars selectedTrackId loadStates ghostTrackIds pendingDeleteId) tracks
            ++ [ button (Style.baseButton ++ [ HE.onClick config.addTrack, HA.style "margin-top" "0.5rem" ]) [ text "+ トラック" ] ]
        )


stopClick : msg -> Html.Attribute msg
stopClick msg =
    HE.stopPropagationOn "click" (Decode.succeed ( msg, True ))


{-| トラック一覧の先頭に置く「コード進行」行。ChordTrack は常に1本だけなので、通常トラックの trackRowと違い
削除ボタン・リネーム入力は持たない。選択判定は Data.ChordTrack.trackId（擬似 ID -1）との比較で行う。
-}
chordTrackRow : Config msg -> Int -> Dict String String -> Set Int -> ChordTrack -> Html msg
chordTrackRow config selectedTrackId loadStates ghostTrackIds chordTrack =
    let
        selected =
            selectedTrackId == Data.ChordTrack.trackId

        loadState =
            Dict.get (Data.Track.instrumentToString chordTrack.instrument) loadStates

        isGhost =
            Set.member Data.ChordTrack.trackId ghostTrackIds

        row =
            config.chordRow
    in
    div
        [ HA.style "display" "flex"
        , HA.style "flex-wrap" "wrap"
        , HA.style "align-items" "center"
        , HA.style "gap" "0.5rem"
        , HA.style "padding" "0.25rem 0.5rem"
        , HA.style "background"
            (if selected then
                Theme.secondaryContainer

             else
                Theme.surfaceContainerLowest
            )
        , HA.style "border-radius" "4px"
        , HA.style "margin-bottom" "2px"
        , HA.style "cursor" "pointer"
        , HA.class "m3-btn"
        , HE.onClick row.select
        ]
        [ span (HA.style "width" "6rem" :: Theme.typeTitleSmall) [ text "コード進行" ]
        , select
            [ HE.onInput row.changeInstrument
            , stopClick row.select
            ]
            (instrumentOptionGroups ((/=) Data.Track.DrumKit) chordTrack.instrument)
        , loadBadge loadState
        , button
            (Style.toggleButton chordTrack.muted
                ++ [ stopClick row.toggleMute
                   , HA.title "ミュート"
                   , HA.attribute "aria-label" "ミュート"
                   ]
            )
            [ text "M" ]
        , button
            (Style.toggleButton isGhost
                ++ [ stopClick row.toggledGhost
                   , HA.title "ピアノロールにコードの構成音を透けて重ね表示"
                   , HA.attribute "aria-label" "ゴースト表示を切替"
                   ]
            )
            [ text "👻" ]
        , input
            [ HA.type_ "range"
            , HA.min "0"
            , HA.max "100"
            , HA.value (String.fromInt chordTrack.volume)
            , HE.onInput row.changeVolume
            , stopClick row.select
            , HA.style "width" "70px"
            , HA.title ("音量 " ++ String.fromInt chordTrack.volume)
            ]
            []
        ]


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
        , HA.style "flex-wrap" "wrap"
        , HA.style "align-items" "center"
        , HA.style "gap" "0.5rem"
        , HA.style "padding" "0.25rem 0.5rem"
        , HA.style "background"
            (if selected then
                Theme.secondaryContainer

             else
                Theme.surfaceContainerLowest
            )
        , HA.style "border-radius" "4px"
        , HA.style "margin-bottom" "2px"
        , HA.style "cursor" "pointer"
        , HA.class "m3-btn"
        , HE.onClick (config.selectTrack track.id)
        ]
        [ input
            [ HA.value track.name
            , HE.onInput (config.renameTrack track.id)
            , stopClick (config.selectTrack track.id)
            , HA.style "width" "6rem"
            , HA.style "font-size" "0.9rem"
            , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
            , HA.style "border-radius" "4px"
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
        (instrumentOptionGroups (always True) track.instrument)


{-| 音色 select の中身を Data.Track.instrumentGroups から optgroup 単位で組み立てる。
include で楽器を除外できる（コード進行行は DrumKit を除外）。除外した結果空になったグループは描画しない。
-}
instrumentOptionGroups : (Data.Track.Instrument -> Bool) -> Data.Track.Instrument -> List (Html msg)
instrumentOptionGroups include selected =
    Data.Track.instrumentGroups
        |> List.filterMap
            (\( groupName, insts ) ->
                case List.filter include insts of
                    [] ->
                        Nothing

                    filtered ->
                        Just
                            (Html.node "optgroup"
                                [ HA.attribute "label" groupName ]
                                (List.map (instrumentOption selected) filtered)
                            )
            )


instrumentOption : Data.Track.Instrument -> Data.Track.Instrument -> Html msg
instrumentOption selected inst =
    option
        [ HA.value (Data.Track.instrumentToString inst)
        , HA.selected (inst == selected)
        ]
        [ text (Data.Track.instrumentLabel inst) ]


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

        trackNotes =
            notesOf track.kind

        pitchPadding =
            3

        ( paddedMinPitch, paddedMaxPitch ) =
            case List.map .pitch trackNotes of
                [] ->
                    ( 60 - pitchPadding, 72 + pitchPadding )

                pitches ->
                    ( (List.minimum pitches |> Maybe.withDefault 60) - pitchPadding
                    , (List.maximum pitches |> Maybe.withDefault 72) + pitchPadding
                    )

        pitchRange =
            Basics.max 1 (paddedMaxPitch - paddedMinPitch)

        noteRect note =
            let
                x =
                    toFloat note.start / toFloat totalTicks * width

                w =
                    Basics.max 1 (toFloat note.duration / toFloat totalTicks * width)

                y =
                    toFloat (paddedMaxPitch - note.pitch) / toFloat pitchRange * height
            in
            Svg.rect
                [ SA.x (String.fromFloat x)
                , SA.y (String.fromFloat y)
                , SA.width (String.fromFloat w)
                , SA.height "2"
                , SA.fill Theme.primary
                ]
                []
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        , HA.style "background" Theme.surfaceContainerLow
        ]
        (List.map noteRect trackNotes)
