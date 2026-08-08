module View.ChordEditor exposing (Config, view)

import Data.Chord exposing (Chord)
import Data.Chord.Format as Format
import Data.ChordTrack exposing (ChordCell, ChordTrack, TokenKind(..))
import Data.GuitarForm as GuitarForm
import Data.Key exposing (Key)
import Data.StrumExpand
import Data.Timeline exposing (Timeline)
import Data.Track
import Data.Voicing exposing (Voicing, anchorPitch)
import Data.VoicingPreset as VoicingPreset
import Dict exposing (Dict)
import Html exposing (Html, button, div, option, select, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)
import View.Fretboard as Fretboard
import View.Palette as Palette
import View.Style as Style
import View.VoicingKeyboard as VoicingKeyboard


type alias Config msg =
    { changedText : String -> msg
    , toggledMute : msg
    , convertToTrack : msg
    , changedVolume : String -> msg
    , clickedChord : Int -> msg
    , toggledGhost : msg
    , changedInstrument : String -> msg
    , toggledVoicingEnabled : msg
    , toggledGuitarFormEnabled : msg
    , clickedCopyText : msg
    , clickedAddVoicing : String -> msg
    , clickedVoicingRow : Int -> msg
    , changedVoicingName : Int -> String -> msg
    , pressedVoicingOffset : Int -> Int -> { clientX : Float, clientY : Float, shift : Bool } -> msg
    , doubleClickedVoicingOffset : Int -> Int -> msg
    , pressedFretboardCell : Int -> Int -> Int -> msg
    , doubleClickedFretboardCell : Int -> Int -> Int -> msg
    , clickedPlayVoicing : Int -> msg
    , clickedRemoveVoicing : Int -> msg
    , changedVoicingPreviewRoot : String -> msg
    , changedVoicingPresetQuality : String -> msg
    , changedVoicingPresetShape : String -> msg
    , appliedVoicingPreset : Int -> msg
    }


type alias VoicingState =
    { voicings : List Voicing
    , enabled : Bool
    , guitarFormEnabled : Bool
    , editingIndex : Maybe Int
    , pendingDelete : Maybe Int
    , copyFeedback : Bool
    , previewRootPc : Int
    , presetQualityName : String
    , presetShapeName : String
    }


view : Config msg -> Timeline -> Int -> Bool -> Dict String String -> VoicingState -> ChordTrack -> Html msg
view config timeline playheadTicks isGhost loadStates voicingState track =
    Html.details
        [ HA.attribute "open" ""
        , HA.style "margin-top" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" "1px solid #ddd"
        , HA.style "border-radius" "4px"
        ]
        [ Html.summary (HA.style "cursor" "pointer" :: Style.headingText) [ text "コード進行" ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.3rem" ]
            [ span [ HA.style "font-size" "0.75rem", HA.style "color" "#888" ]
                [ text "| で小節区切り（改行は無視されるので自由に使ってOK）、空白で小節内分割。% = 直前のコードを繰返し、_ = 休符、= = 直前のコードを伸ばす、// 以降は行末までコメント。コードをクリックすると再生位置がそこへ移動" ]
            , instrumentSelect config track
            , loadBadge (Dict.get (Data.Track.instrumentToString track.instrument) loadStates)
            , button
                (Style.toggleButton track.muted
                    ++ [ HE.onClick config.toggledMute
                       , HA.title "コードトラックをミュート"
                       ]
                )
                [ text "M" ]
            , button
                (Style.toggleButton isGhost
                    ++ [ HE.onClick config.toggledGhost
                       , HA.title "ピアノロールにコードの構成音を透けて重ね表示"
                       ]
                )
                [ text "👻" ]
            , Html.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "100"
                , HA.value (String.fromInt track.volume)
                , HE.onInput config.changedVolume
                , HA.style "width" "5rem"
                , HA.title ("音量: " ++ String.fromInt track.volume)
                ]
                []
            , button
                (Style.baseButton
                    ++ [ HE.onClick config.convertToTrack
                       , HA.title "コード進行をノートに展開して新しいMIDIトラックを作る"
                       ]
                )
                [ text "→ MIDIトラック化" ]
            , Style.divider
            , button
                (Style.toggleButton voicingState.enabled
                    ++ [ HE.onClick config.toggledVoicingEnabled
                       , HA.title "@NAME ボイシングを適用するかどうか（OFFなら固定ルールで鳴らす）"
                       ]
                )
                [ text "🎵 ボイシング" ]
            , button
                (Style.toggleButton voicingState.guitarFormEnabled
                    ++ [ HE.onClick config.toggledGuitarFormEnabled
                       , HA.title "登録ボイシングのないコードをどちらで鳴らす/表示するか（ON=ギターの実運指を優先、OFF=ピアノ的な理論値をそのまま）"
                       ]
                )
                [ text
                    (if voicingState.guitarFormEnabled then
                        "🎸 ギター指板"

                     else
                        "🎹 ピアノ"
                    )
                ]
            , button
                (Style.baseButton
                    ++ [ HE.onClick config.clickedCopyText
                       , HA.title "@NAME を除いたプレーンなコード譜をクリップボードへコピー"
                       ]
                )
                [ text
                    (if voicingState.copyFeedback then
                        "✓ コピーした"

                     else
                        "📋 コード譜をコピー"
                    )
                ]
            ]
        , voiceLeadingView timeline playheadTicks voicingState.enabled voicingState.guitarFormEnabled voicingState.voicings track
        , textarea
            [ HA.value track.text
            , HE.onInput config.changedText
            , HA.placeholder "例:\nC | G/B | Am7 | F  // イントロ\nC | G7b5 | % | =  // Aメロ"
            , HA.style "width" "98%"
            , HA.style "font-size" "1rem"
            , HA.style "margin-top" "0.4rem"
            , HA.style "font-family" "monospace"
            , HA.style "padding" "0.3rem"
            , HA.style "min-height" "8rem"
            , HA.style "resize" "vertical"
            ]
            []
        , div
            [ HA.style "display" "flex"
            , HA.style "gap" "2px"
            , HA.style "margin-top" "0.4rem"
            , HA.style "flex-wrap" "wrap"
            ]
            (List.map (cellView config timeline playheadTicks) (Data.ChordTrack.cells timeline track))
        , voicingsSection config voicingState (Data.ChordTrack.cells timeline track)
        ]


{-| 再生位置にあるコードと、その直前のコード。`Data.ChordTrack.resolved` は時系列順なので、
現在イベントの判定は `cellView` の isCurrentBar と同じで、その直前に見たイベントを一回の走査で追う。
-}
adjacentChords : Timeline -> Int -> ChordTrack -> { previous : Maybe Chord, current : Maybe Chord }
adjacentChords timeline playheadTicks track =
    let
        go remaining prev =
            case remaining of
                [] ->
                    { previous = prev, current = Nothing }

                rc :: rest ->
                    if playheadTicks >= rc.startTicks && playheadTicks < rc.startTicks + rc.durationTicks then
                        { previous = prev, current = Just rc.chord }

                    else
                        go rest (Just rc.chord)
    in
    go (Data.ChordTrack.resolved timeline track) Nothing


{-| 今鳴っているコードの運指を、直前コードとの比較（保持=青、移動=灰）付きで常時表示する（ボイスリーディング表示）。
Data.StrumExpand.formFor で Form を決めるので、実際のストローク音生成・普段の再生と必ず一致する。ボイシングトグル OFF
のときは voicings を空にする。`guitarFormEnabled` が False（ピアノモード）のときは指板自体を出さず現在コード名だけ
表示する。True（ギターモード）のときは `formFor` が Nothing でも常に指板ボックスを一緒に描画して（空のまま）、
進行中のコードごとに箱が出たり消えたりして UI がガタつくのを防ぐ。
-}
voiceLeadingView : Timeline -> Int -> Bool -> Bool -> List Voicing -> ChordTrack -> Html msg
voiceLeadingView timeline playheadTicks voicingEnabled guitarFormEnabled voicings track =
    let
        adjacent =
            adjacentChords timeline playheadTicks track
    in
    case adjacent.current of
        Nothing ->
            text ""

        Just chord ->
            let
                activeVoicings =
                    if voicingEnabled then
                        voicings

                    else
                        []

                label c =
                    span [ HA.style "font-weight" "bold", HA.style "white-space" "nowrap" ]
                        [ text ("🎸 " ++ Format.format { preferFlat = False } c) ]
            in
            if not guitarFormEnabled then
                div [ HA.style "margin-top" "0.4rem" ] [ label chord ]

            else
                let
                    currentForm =
                        Data.StrumExpand.formFor True activeVoicings chord

                    previousForm =
                        adjacent.previous |> Maybe.andThen (Data.StrumExpand.formFor True activeVoicings)

                    currentIndexed =
                        currentForm |> Maybe.map GuitarForm.toIndexedPitches |> Maybe.withDefault []

                    heldIndexed =
                        currentForm |> Maybe.map (GuitarForm.heldPitches previousForm) |> Maybe.withDefault []

                    currentSelected =
                        List.map Tuple.second currentIndexed |> Set.fromList

                    currentPicks =
                        List.map (\( s, p ) -> ( p, s )) heldIndexed |> Set.fromList

                    currentBoard =
                        div
                            [ HA.style "display" "flex"
                            , HA.style "align-items" "flex-start"
                            , HA.style "gap" "0.5rem"
                            ]
                            [ label chord
                            , Fretboard.viewReadOnly { rootPitch = 0, selected = currentSelected, picks = currentPicks }
                            ]
                in
                case adjacent.previous of
                    Nothing ->
                        div [ HA.style "margin-top" "0.4rem" ] [ currentBoard ]

                    Just prevChord ->
                        let
                            previousIndexed =
                                previousForm |> Maybe.map GuitarForm.toIndexedPitches |> Maybe.withDefault []

                            previousSelected =
                                List.map Tuple.second previousIndexed |> Set.fromList

                            previousPicks =
                                List.map (\( s, p ) -> ( p, s )) previousIndexed |> Set.fromList

                            previousBoard =
                                div
                                    [ HA.style "display" "flex"
                                    , HA.style "align-items" "flex-start"
                                    , HA.style "gap" "0.5rem"
                                    ]
                                    [ label prevChord
                                    , Fretboard.viewReadOnly { rootPitch = 0, selected = previousSelected, picks = previousPicks }
                                    ]
                        in
                        div
                            [ HA.style "display" "flex"
                            , HA.style "align-items" "flex-start"
                            , HA.style "gap" "0.5rem"
                            , HA.style "margin-top" "0.4rem"
                            ]
                            [ previousBoard
                            , span [ HA.style "align-self" "center", HA.style "color" "#aaa" ] [ text "→" ]
                            , currentBoard
                            ]


{-| コード進行で使われているが未登録のボイシング名を重複なく列挙する。
-}
unregisteredVoicingNames : List Voicing -> List ChordCell -> List String
unregisteredVoicingNames voicings cells =
    let
        registered =
            List.map .name voicings

        usedNames =
            cells
                |> List.concatMap .chords
                |> List.filterMap
                    (\c ->
                        case c.result of
                            Ok (TChord chord) ->
                                chord.voicing

                            _ ->
                                Nothing
                    )
    in
    usedNames
        |> List.filter (\name -> not (List.member name registered))
        |> List.foldl
            (\name acc ->
                if List.member name acc then
                    acc

                else
                    acc ++ [ name ]
            )
            []


voicingsSection : Config msg -> VoicingState -> List ChordCell -> Html msg
voicingsSection config voicingState cells =
    let
        missing =
            unregisteredVoicingNames voicingState.voicings cells
    in
    Html.details
        [ HA.style "margin-top" "0.5rem"
        , HA.style "padding" "0.4rem"
        , HA.style "border" "1px solid #eee"
        , HA.style "border-radius" "4px"
        ]
        (Html.summary [ HA.style "cursor" "pointer", HA.style "font-size" "0.85rem", HA.style "color" "#666" ] [ text "ボイシング辞書" ]
            :: (if List.isEmpty missing then
                    []

                else
                    [ div [ HA.style "font-size" "0.75rem", HA.style "color" "#e6a817", HA.style "margin-top" "0.3rem" ]
                        (text "未登録: "
                            :: List.map
                                (\name ->
                                    button
                                        (Style.baseButton
                                            ++ [ HE.onClick (config.clickedAddVoicing name)
                                               , HA.style "margin-left" "0.3rem"
                                               , HA.title ("「" ++ name ++ "」を新規登録")
                                               ]
                                        )
                                        [ text name ]
                                )
                                missing
                        )
                    ]
               )
            ++ List.indexedMap (voicingRow config voicingState) voicingState.voicings
            ++ [ button
                    (Style.baseButton
                        ++ [ HE.onClick (config.clickedAddVoicing "")
                           , HA.style "margin-top" "0.3rem"
                           ]
                    )
                    [ text "+ ボイシング" ]
               ]
        )


voicingRow : Config msg -> VoicingState -> Int -> Voicing -> Html msg
voicingRow config voicingState index voicing =
    let
        rootPitch =
            anchorPitch + voicingState.previewRootPc
    in
    div [ HA.style "margin-top" "0.4rem" ]
        [ div [ HA.style "display" "flex", HA.style "gap" "0.4rem", HA.style "align-items" "center" ]
            [ Html.input
                [ HA.value voicing.name
                , HE.onInput (config.changedVoicingName index)
                , HA.style "width" "8rem"
                , HA.placeholder "名前"
                ]
                []
            , button
                (Style.baseButton ++ [ HE.onClick (config.clickedVoicingRow index) ])
                [ text
                    (if voicingState.editingIndex == Just index then
                        "閉じる"

                     else
                        "編集"
                    )
                ]
            , button
                (Style.baseButton ++ [ HE.onClick (config.clickedPlayVoicing index), HA.title "このボイシングを和音で確かめる（試聴キーに従う）" ])
                [ text "▶ 全部鳴らす" ]
            , if voicingState.pendingDelete == Just index then
                button
                    (Style.armedDangerButton ++ [ HE.onClick (config.clickedRemoveVoicing index) ])
                    [ text "本当に削除？" ]

              else
                button
                    (Style.dangerButton ++ [ HE.onClick (config.clickedRemoveVoicing index) ])
                    [ text "✕" ]
            ]
        , if voicingState.editingIndex == Just index then
            div []
                [ div
                    [ HA.style "display" "flex"
                    , HA.style "gap" "0.4rem"
                    , HA.style "align-items" "center"
                    , HA.style "flex-wrap" "wrap"
                    , HA.style "margin-top" "0.3rem"
                    , HA.style "font-size" "0.75rem"
                    , HA.style "color" "#666"
                    ]
                    [ text "試聴キー:"
                    , previewRootSelect config voicingState.previewRootPc
                    , Style.divider
                    , text "プリセット:"
                    , qualitySelect config voicingState.presetQualityName
                    , shapeSelect config voicingState.presetShapeName
                    , button
                        (Style.baseButton
                            ++ [ HE.onClick (config.appliedVoicingPreset index)
                               , HA.title "選んだコード品質×配置で offsets を上書き"
                               ]
                        )
                        [ text "プリセットを適用" ]
                    ]
                , let
                    selected =
                        Set.fromList (List.map ((+) rootPitch) voicing.offsets)

                    displayRootPitch =
                        Data.Voicing.displayRoot rootPitch voicing.offsets
                  in
                  div [ HA.style "display" "flex", HA.style "gap" "0.6rem", HA.style "align-items" "flex-start", HA.style "overflow-x" "auto" ]
                    [ VoicingKeyboard.view
                        { pressedOffset = config.pressedVoicingOffset index
                        , doubleClickedOffset = config.doubleClickedVoicingOffset index
                        }
                        { rootPitch = rootPitch, displayRootPitch = displayRootPitch, selected = selected }
                    , Fretboard.view
                        { pressedFret = config.pressedFretboardCell index
                        , doubleClickedFret = config.doubleClickedFretboardCell index
                        }
                        { rootPitch = rootPitch, selected = selected, picks = voicing.stringPicks }
                    ]
                ]

          else
            text ""
        ]


previewRootSelect : Config msg -> Int -> Html msg
previewRootSelect config previewRootPc =
    select
        [ HE.onInput config.changedVoicingPreviewRoot ]
        (List.range 0 11
            |> List.map
                (\pc ->
                    option
                        [ HA.value (String.fromInt pc), HA.selected (pc == previewRootPc) ]
                        [ text (Format.pitchName False pc) ]
                )
        )


qualitySelect : Config msg -> String -> Html msg
qualitySelect config presetQualityName =
    select
        [ HE.onInput config.changedVoicingPresetQuality ]
        (VoicingPreset.qualities
            |> List.map VoicingPreset.qualityLabel
            |> List.map
                (\name ->
                    option [ HA.value name, HA.selected (name == presetQualityName) ] [ text name ]
                )
        )


shapeSelect : Config msg -> String -> Html msg
shapeSelect config presetShapeName =
    select
        [ HE.onInput config.changedVoicingPresetShape ]
        (VoicingPreset.shapes
            |> List.map
                (\( name, _ ) ->
                    option [ HA.value name, HA.selected (name == presetShapeName) ] [ text name ]
                )
        )


cellView : Config msg -> Timeline -> Int -> ChordCell -> Html msg
cellView config timeline playheadTicks cell =
    let
        isCurrentBar =
            playheadTicks >= cell.startTicks && playheadTicks < cell.startTicks + cell.lengthTicks

        -- セル内のトークンは小節を等分割するので、位置から逆算できる
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
                "#fff6dd"

            else
                case Data.Timeline.sectionIndexAt cell.startTicks timeline of
                    Just idx ->
                        Palette.sectionTint idx

                    Nothing ->
                        Palette.neutral
    in
    div
        [ HA.style "border"
            (if isCurrentBar then
                "1px solid #e6a817"

             else
                "1px solid #ccc"
            )
        , HA.style "border-radius" "3px"
        , HA.style "padding" "0.2rem 0.4rem"
        , HA.style "min-width" "3.5rem"
        , HA.style "background" background
        ]
        (span
            [ HA.style "font-size" "0.7rem"
            , HA.style "color" "#aaa"
            , HA.style "margin-right" "0.3rem"
            , HA.style "cursor" "pointer"
            , HA.title "この小節の頭から再生"
            , HE.onClick (config.clickedChord cell.startTicks)
            ]
            [ text (String.fromInt (cell.barIndex + 1)) ]
            :: List.indexedMap (\i c -> chordView config (tickAtToken i) key (i == currentToken) c) cell.chords
        )


chordView : Config msg -> Int -> Key -> Bool -> { token : String, result : Result String TokenKind } -> Html msg
chordView config tick key isCurrent c =
    let
        highlight =
            [ HA.style "padding" "0 0.15rem"
            , HA.style "border-radius" "2px"
            , HA.style "background"
                (if isCurrent then
                    Style.colorHighlight

                 else
                    "transparent"
                )
            ]

        clickable =
            [ HA.style "cursor" "pointer"
            , HA.title "クリックでここから再生"
            , HE.onClick (config.clickedChord tick)
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
                 ]
                    ++ highlight
                    ++ clickable
                )
                [ span [ HA.style "color" color, HA.style "font-weight" "bold" ] [ text c.token ]
                , case degree of
                    Just d ->
                        span [ HA.style "font-size" "0.6rem", HA.style "color" "#aaa" ] [ text d ]

                    Nothing ->
                        text ""
                ]
    in
    case c.result of
        Ok (TChord _) ->
            withDegree "#2c7a2c"

        Ok _ ->
            withDegree "#888"

        Err reason ->
            span
                ([ HA.style "color" "#e74c3c"
                 , HA.style "text-decoration" "underline wavy"
                 , HA.style "margin-right" "0.3rem"
                 , HA.title reason
                 ]
                    ++ highlight
                    ++ clickable
                )
                [ text c.token ]


instrumentSelect : Config msg -> ChordTrack -> Html msg
instrumentSelect config track =
    select
        [ HE.onInput config.changedInstrument ]
        (Data.Track.allInstruments
            |> List.filter ((/=) Data.Track.DrumKit)
            |> List.map
                (\inst ->
                    option
                        [ HA.value (Data.Track.instrumentToString inst)
                        , HA.selected (inst == track.instrument)
                        ]
                        [ text (Data.Track.instrumentLabel inst) ]
                )
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
