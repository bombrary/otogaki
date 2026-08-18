module View.FormPicker exposing (Config, Tab(..), view)

import Data.Chord exposing (Chord)
import Data.Chord.Format as ChordFormat
import Data.Chord.Parser as ChordParser
import Data.GuitarForm as GuitarForm
import Data.Voicing exposing (anchorPitch)
import Data.VoicingPreset as VoicingPreset
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import View.Fretboard as Fretboard
import View.Style as Style
import View.Theme as Theme


{-| コードのトークンをダブルクリックしたときに開く「フォーム候補選択」モーダルの中身。
`View.Modal.view` に載せて使う想定(Modal へのラップは呼び出し側 = Main.elm で行う)。
コード名自体も draft として直接編集できる(`changedDraft`/`submittedDraft`)。

「候補から選ぶ」(`CandidatesTab`)と「手で編集」(`EditTab`)の2タブを持つ。編集対象の実体
(`View.ChordEditor.voicingEditorView` で組み立てた `Html msg`)は呼び出し側が `editor` として渡す
(Main.elm が保持する編集ロジック・状態をこちらへ複製しないため)。
-}
type alias Config msg =
    { chose : GuitarForm.Candidate -> msg
    , choseShape : VoicingPreset.Shape -> msg
    , cleared : msg
    , hoveredFret : { pitch : Int, interval : Int, x : Float, y : Float } -> msg
    , unhoveredFret : msg
    , changedDraft : String -> msg
    , submittedDraft : msg
    , selectedTab : Tab -> msg
    }


type Tab
    = CandidatesTab
    | EditTab


{-| draft はモーダルを開いた時点のトークン文字列を初期値とする編集中のテキスト(呼び出し側 = Main.elm が
`model.formPicker.draft` として保持する)。表示するコード・候補一覧はこの draft を `ChordParser.parse` した
結果から動的に計算する。パースに失敗している間はエラーメッセージだけを出し、候補は出さない
(Enter か適用ボタンで確定するまでトラックへは反映されない)。

`tab` で現在どちらのタブが開いているかを、`editor` で「手で編集」タブの中身(対象が無ければ `Nothing`)を渡す。
-}
view : Config msg -> { draft : String, tab : Tab, editor : Maybe (Html msg) } -> Html msg
view config { draft, tab, editor } =
    let
        parseResult =
            ChordParser.parse draft
    in
    div []
        [ draftInputRow config draft
        , tabBar config tab parseResult
        , case tab of
            CandidatesTab ->
                case parseResult of
                    Ok chord ->
                        div [] (candidatesSection config chord)

                    Err message ->
                        div
                            [ HA.style "margin-top" "0.4rem", HA.style "font-size" "0.75rem", HA.style "color" Style.colorDanger ]
                            [ text ("コードとして読み取れません: " ++ message) ]

            EditTab ->
                case editor of
                    Just html ->
                        html

                    Nothing ->
                        div
                            [ HA.style "margin-top" "0.6rem", HA.style "font-size" "0.8rem", HA.style "color" Theme.onSurfaceVariant ]
                            [ text "対象が見つかりません。" ]
        ]


draftInputRow : Config msg -> String -> Html msg
draftInputRow config draft =
    div [ HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "0.4rem" ]
        [ input
            [ HA.type_ "text"
            , HA.value draft
            , HA.style "font-size" "1rem"
            , HA.style "font-weight" "600"
            , HA.style "padding" "0.2rem 0.4rem"
            , HA.style "flex" "1"
            , HE.onInput config.changedDraft
            , onEnter config.submittedDraft
            ]
            []
        , button (Style.baseButton ++ [ HE.onClick config.submittedDraft ]) [ text "適用" ]
        ]


{-| draft の parse に失敗している間は「手で編集」ボタンを disabled にする(`EditTab` へ切り替えても
対象を決定できないため)。
-}
tabBar : Config msg -> Tab -> Result String Chord -> Html msg
tabBar config tab parseResult =
    let
        editDisabled =
            case parseResult of
                Ok _ ->
                    False

                Err _ ->
                    True
    in
    div [ HA.style "display" "flex", HA.style "gap" "0.4rem", HA.style "margin-top" "0.6rem" ]
        [ button
            (Style.toggleButton (tab == CandidatesTab) ++ [ HE.onClick (config.selectedTab CandidatesTab) ])
            [ text "候補から選ぶ" ]
        , button
            (Style.toggleButton (tab == EditTab)
                ++ [ HE.onClick (config.selectedTab EditTab)
                   , HA.disabled editDisabled
                   , HA.style "opacity"
                        (if editDisabled then
                            "0.5"

                         else
                            "1"
                        )
                   , HA.style "cursor"
                        (if editDisabled then
                            "not-allowed"

                         else
                            "pointer"
                        )
                   , HA.title
                        (if editDisabled then
                            "コードとして読み取れないため編集できません"

                         else
                            ""
                        )
                   ]
            )
            [ text "手で編集" ]
        ]


onEnter : msg -> Html.Attribute msg
onEnter msg =
    HE.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "not enter"
                )
        )


{-| chord は draft がパースに成功した結果。`chord.voicing` が既存登録を指していれば解除ボタンを出す。
表示順序は「ボイシング型(Closed/Drop2/Wide)」を先に、「ギター運指候補」(`GuitarForm.candidatesFor`の順序、
Open → E型 → A型)を後に並べる（ギターを弾かない人の方が多いかもしれないため、本アプリの明示的な方針）。
-}
candidatesSection : Config msg -> Chord -> List (Html msg)
candidatesSection config chord =
    let
        candidates =
            GuitarForm.candidatesFor chord

        rootPitch =
            anchorPitch + modBy 12 chord.root
    in
    [ case chord.voicing of
        Just name ->
            div [ HA.style "margin-top" "0.4rem", HA.style "display" "flex", HA.style "align-items" "center", HA.style "gap" "0.4rem" ]
                [ span [ HA.style "font-size" "0.75rem", HA.style "color" Theme.onSurfaceVariant ]
                    [ text ("現在の登録: @" ++ name) ]
                , button
                    (Style.dangerButton ++ [ HE.onClick config.cleared, HA.title "このトークンの @NAME を外す(辞書のエントリ自体は残る)" ]
                    )
                    [ text "解除" ]
                ]

        Nothing ->
            text ""
    , div [ HA.style "margin-top" "0.6rem", HA.style "font-size" "0.75rem", HA.style "font-weight" "600", HA.style "color" Theme.onSurfaceVariant ]
        [ text "ボイシング型" ]
    , div [ HA.style "font-size" "0.7rem", HA.style "color" Theme.onSurfaceVariant, HA.style "margin-top" "0.2rem" ]
        [ text "※ Closed/Drop2/Wide はテンション（#9 等）を含まない基本形になります" ]
    , div [ HA.style "display" "flex", HA.style "gap" "0.6rem", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.3rem" ]
        (List.map (\( _, shape ) -> shapeCard config chord shape) VoicingPreset.shapes)
    , div [ HA.style "margin-top" "0.8rem", HA.style "font-size" "0.75rem", HA.style "font-weight" "600", HA.style "color" Theme.onSurfaceVariant ]
        [ text "ギター運指" ]
    , if List.isEmpty candidates then
        div
            [ HA.style "margin-top" "0.4rem", HA.style "font-size" "0.8rem", HA.style "color" Theme.onSurfaceVariant ]
            [ text "このコードには固定フォームの候補がありません。再生・指板表示は bestForm による自動運指を使います。" ]

      else
        div
            [ HA.style "display" "flex", HA.style "gap" "0.6rem", HA.style "flex-wrap" "wrap", HA.style "margin-top" "0.3rem" ]
            (List.map (candidateCard config rootPitch) candidates)
    ]


candidateCard : Config msg -> Int -> GuitarForm.Candidate -> Html msg
candidateCard config rootPitch candidate =
    div
        [ HA.class "m3-btn"
        , HA.style "cursor" "pointer"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "border-radius" "4px"
        , HA.style "padding" "0.5rem"
        , HE.onClick (config.chose candidate)
        ]
        [ div Style.headingText [ text (GuitarForm.candidateLabel candidate) ]
        , div [ HA.style "font-size" "0.7rem", HA.style "color" Theme.onSurfaceVariant ]
            [ text ("最大 " ++ String.fromInt (GuitarForm.maxFret candidate.form) ++ "F") ]
        , Fretboard.viewReadOnly
            { hoveredFret = config.hoveredFret, unhoveredFret = config.unhoveredFret }
            (Fretboard.formData rootPitch candidate.form)
        ]


{-| ボイシング型(Closed/Drop2/Wide)のカード。`candidateCard` と違い指板プレビューは持たず、
見出し + 構成音プレビュー(低音から高音の順の音名)だけを表示するシンプルな作り。
`VoicingPreset.offsetsFor` は quality の基本音のみを見ており、extensions/alterations（テンション・
オルタード）は反映されない点に注意（例: C7(#9) の Closed は素の C7 相当になる）。
-}
shapeCard : Config msg -> Chord -> VoicingPreset.Shape -> Html msg
shapeCard config chord shape =
    let
        offsets =
            VoicingPreset.offsetsFor chord.quality shape

        rootPitch =
            anchorPitch + modBy 12 chord.root

        toneNames =
            offsets
                |> List.sort
                |> List.map (\o -> ChordFormat.pitchName False (modBy 12 (rootPitch + o)))
                |> String.join " "
    in
    div
        [ HA.class "m3-btn"
        , HA.style "cursor" "pointer"
        , HA.style "border" ("1px solid " ++ Theme.outlineVariant)
        , HA.style "border-radius" "4px"
        , HA.style "padding" "0.5rem"
        , HE.onClick (config.choseShape shape)
        ]
        [ div Style.headingText [ text (VoicingPreset.shapeLabel shape) ]
        , div [ HA.style "font-size" "0.7rem", HA.style "color" Theme.onSurfaceVariant ]
            [ text toneNames ]
        ]
