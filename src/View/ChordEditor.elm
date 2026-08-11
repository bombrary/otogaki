module View.ChordEditor exposing (Config, progressionEditorView, view, voicingEditorView)

import Data.Chord exposing (Chord)
import Data.Chord.Format as Format
import Data.ChordTrack exposing (ChordCell, ChordTrack, TokenKind(..))
import Data.GuitarForm as GuitarForm
import Data.StrumExpand
import Data.Timeline exposing (Timeline)
import Data.Voicing exposing (Voicing, anchorPitch)
import Data.VoicingPreset as VoicingPreset
import Html exposing (Html, button, div, option, select, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)
import View.Fretboard as Fretboard
import View.Palette as Palette
import View.Style as Style
import View.Theme as Theme
import View.VoicingKeyboard as VoicingKeyboard


type alias Config msg =
    { changedChordSheetText : String -> msg
    , toggledChordProgressionModal : msg
    , convertToTrack : msg
    , toggledVoicingEnabled : msg
    , toggledGuitarFormEnabled : msg
    , clickedCopyText : msg
    , clickedAddVoicing : String -> msg
    , clickedVoicingRow : Int -> msg
    , changedVoicingName : Int -> String -> msg
    , pressedVoicingOffset : Int -> Int -> { clientX : Float, clientY : Float, shift : Bool } -> msg
    , doubleClickedVoicingOffset : Int -> Int -> msg
    , pressedVoicingKeyboardKey : Int -> msg
    , pressedFretboardCell : Int -> Int -> Int -> msg
    , doubleClickedFretboardCell : Int -> Int -> Int -> msg
    , clickedPlayVoicing : Int -> msg
    , clickedRemoveVoicing : Int -> msg
    , changedVoicingPreviewRoot : String -> msg
    , changedVoicingPresetQuality : String -> msg
    , changedVoicingPresetShape : String -> msg
    , appliedVoicingPreset : Int -> msg
    , clickedResetVoicing : Int -> msg
    , hoveredFretCell : { pitch : Int, interval : Int, x : Float, y : Float } -> msg
    , unhoveredFretCell : msg
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
    , selectedOffsets : Set Int
    }


{-| 再生位置（playheadTicks）に鳴っているコードを、指板の運指と一緒に表示する。ボイスリーディング
（直前コードとの比較・保持弦/移動弦の色分け）はしない。単一コードの運指のみ。
`hover` は Fretboard のホバー（常時有効）をそのまま中継するだけ。
-}
currentChordView : Timeline -> Int -> VoicingState -> ChordTrack -> Fretboard.HoverConfig msg -> Html msg
currentChordView timeline playheadTicks voicingState track hover =
    let
        activeChord =
            Data.ChordTrack.resolved timeline track
                |> List.filter (\rc -> playheadTicks >= rc.startTicks && playheadTicks < rc.startTicks + rc.durationTicks)
                |> List.head
                |> Maybe.map .chord

        effectiveVoicings =
            if voicingState.enabled then
                voicingState.voicings

            else
                []
    in
    case activeChord of
        Nothing ->
            text ""

        Just chord ->
            div [ HA.style "margin-top" "0.5rem" ]
                [ div Style.headingText [ text ("\u{1F3B8} " ++ Format.format { preferFlat = False } chord) ]
                , case Data.StrumExpand.formFor voicingState.guitarFormEnabled effectiveVoicings chord of
                    Just form ->
                        Fretboard.viewReadOnly hover (formToFretboardData chord form)

                    Nothing ->
                        div [ HA.style "font-size" "0.75rem", HA.style "color" Theme.onSurfaceVariant ] [ text "（運指を決められません）" ]
                ]


formToFretboardData : Chord -> GuitarForm.Form -> { rootPitch : Int, selected : Set Int, picks : GuitarForm.StringPicks }
formToFretboardData chord form =
    Fretboard.formData (anchorPitch + modBy 12 chord.root) form


view : Config msg -> Timeline -> Int -> VoicingState -> ChordTrack -> Html msg
view config timeline playheadTicks voicingState track =
    Html.details
        [ HA.attribute "open" ""
        , HA.style "margin-top" "1rem"
        , HA.style "padding" "0.5rem"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "border-radius" "4px"
        ]
        [ Html.summary (HA.style "cursor" "pointer" :: Style.headingText) [ text "コード進行" ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.5rem", HA.style "align-items" "center", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.3rem" ]
            [ button
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
            , button
                (Style.baseButton
                    ++ [ HE.onClick config.toggledChordProgressionModal
                       , HA.title "広い画面でコード進行のテキストを編集"
                       ]
                )
                [ text "✎ コード進行を編集" ]
            ]
        , currentChordView timeline playheadTicks voicingState track { hoveredFret = config.hoveredFretCell, unhoveredFret = config.unhoveredFretCell }
        , voicingsSection config voicingState (Data.ChordTrack.cells timeline track)
        ]


{-| コード進行のテキストエリア。380px のサイドバーには打ちづらいという声を受け、
Main.elm 側で `View.Modal` に載せて広い画面で表示する想定。
-}
progressionEditorView : Config msg -> String -> Html msg
progressionEditorView config sheetText =
    div []
        [ div Style.headingText [ text "コード進行を編集" ]
        , span [ HA.style "font-size" "0.75rem", HA.style "color" Theme.onSurfaceVariant, HA.style "display" "block", HA.style "margin-top" "0.3rem" ]
            [ text "空行でセクションを区切り、ブロック先頭の縦棒を含まない行がセクション名になります。縦棒で小節区切り、空白で小節内分割。% = 直前のコードを繰返し、_ = 休符、= = 直前のコードを伸ばす、// 以降は行末までコメント。入力するたびに自動反映されます（形式が崩れている間は反映されません）。コードをクリックすると再生位置がそこへ移動" ]
        , textarea
            [ HA.value sheetText
            , HE.onInput config.changedChordSheetText
            , HA.placeholder "例:\nAメロ\nC | G/B | Am7 | F |\n\nサビ\nC | G7b5 | % | = |"
            , HA.style "width" "100%"
            , HA.style "font-size" "1rem"
            , HA.style "margin-top" "0.4rem"
            , HA.style "font-family" "monospace"
            , HA.style "padding" "0.3rem"
            , HA.style "min-height" "16rem"
            , HA.style "resize" "vertical"
            , HA.style "box-sizing" "border-box"
            ]
            []
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
        , HA.style "border" ("1px solid " ++ Theme.surfaceContainerHighest)
        , HA.style "border-radius" "4px"
        ]
        (Html.summary [ HA.style "cursor" "pointer", HA.style "font-size" "0.85rem", HA.style "color" Theme.onSurfaceVariant ] [ text "ボイシング辞書" ]
            :: (if List.isEmpty missing then
                    []

                else
                    [ div [ HA.style "font-size" "0.75rem", HA.style "color" Theme.onHighlightContainer, HA.style "margin-top" "0.3rem" ]
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
                [ text "▶　全部同時発音" ]
            , if voicingState.pendingDelete == Just index then
                button
                    (Style.armedDangerButton ++ [ HE.onClick (config.clickedRemoveVoicing index) ])
                    [ text "本当に削除？" ]

              else
                button
                    (Style.dangerButton ++ [ HE.onClick (config.clickedRemoveVoicing index) ])
                    [ text "✕" ]
            ]
        ]


{-| ボイシング1件のピアノ＋指板編集画面。380px のサイドバーには収まらないので、
Main.elm 側で `View.Modal` に載せて表示する想定。
-}
voicingEditorView : Config msg -> VoicingState -> Int -> Voicing -> Html msg
voicingEditorView config voicingState index voicing =
    let
        rootPitch =
            anchorPitch + voicingState.previewRootPc

        selected =
            Set.fromList (List.map ((+) rootPitch) voicing.offsets)

        displayRootPitch =
            Data.Voicing.displayRoot rootPitch voicing.offsets
    in
    div []
        [ div Style.headingText [ text ("ボイシングを編集: " ++ voicing.name) ]
        , div
            [ HA.style "display" "flex"
            , HA.style "gap" "0.4rem"
            , HA.style "align-items" "center"
            , HA.style "flex-wrap" "wrap"
            , HA.style "margin-top" "0.5rem"
            , HA.style "font-size" "0.75rem"
            , HA.style "color" Theme.onSurfaceVariant
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
            , button (Style.dangerButton ++ [ HE.onClick (config.clickedResetVoicing index), HA.title "offsetsと弦選択を空にして最初からやり直す" ]) [ text "初期化" ]
            , Style.divider
            , button
                (Style.baseButton ++ [ HE.onClick (config.clickedPlayVoicing index), HA.title "このボイシングを和音で確かめる（試聴キーに従う）" ])
                [ text "▶　全部同時発音" ]
            ]
        , div [ HA.style "display" "flex", HA.style "gap" "0.6rem", HA.style "align-items" "flex-start", HA.style "overflow-x" "auto", HA.style "margin-top" "0.6rem" ]
            [ VoicingKeyboard.view
                { pressedOffset = config.pressedVoicingOffset index
                , doubleClickedOffset = config.doubleClickedVoicingOffset index
                , pressedKey = config.pressedVoicingKeyboardKey
                }
                { hoveredKey = config.hoveredFretCell, unhoveredKey = config.unhoveredFretCell }
                { rootPitch = rootPitch
                , displayRootPitch = displayRootPitch
                , placed = selected
                , selectedPitches = Set.map ((+) rootPitch) voicingState.selectedOffsets
                }
            , Fretboard.view
                { pressedFret = config.pressedFretboardCell index
                , doubleClickedFret = config.doubleClickedFretboardCell index
                }
                { hoveredFret = config.hoveredFretCell, unhoveredFret = config.unhoveredFretCell }
                { rootPitch = rootPitch, selected = selected, picks = voicing.stringPicks }
            ]
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
