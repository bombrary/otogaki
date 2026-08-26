module Data.Help exposing
    ( Line
    , Tab(..)
    , Topic
    , TopicId(..)
    , domId
    , find
    , tabLabel
    , tabOf
    , tabs
    , topics
    , topicsIn
    )

{-| アプリ内ヘルプの唯一の情報源。README.md の「キーボード」表はここからの抜粋であり、
`ShortcutsTab` の `Line` は `Main.elm` の `GotKey`（キー分岐）と一対一で対応する。
`GotKey` に分岐を足す／消すときは、必ずこのリストも直すこと（`tests/HelpTest.elm` は構造の壊れは
拾うが、`GotKey` との対応まではコンパイラでは縛れない）。

`View/HelpPanel.elm` はこのデータを描画するだけで、文言そのものは一切持たない。

-}


type Tab
    = ShortcutsTab
    | OperationTab
    | NotationTab
    | GlossaryTab


type TopicId
    = Transport
    | EditKeys
    | SelectKeys
    | VoicingKeys
    | KeyboardPlay
    | PianoRollOps
    | VelocityLane
    | LoopOps
    | SectionOps
    | ChordOps
    | DrumApply
    | DrumPresets
    | TrackOps
    | Modifiers
    | TouchOps
    | FileOps
    | RefAudioOps
    | ScrapOps
    | PaneOps
    | ChordSheet
    | VoicingNotation
    | VoicingEditorOps
    | Degrees
    | Icons
    | Terms


{-| 左列は「キー」「UI のラベル」「アイコン」のいずれか。空文字なら 1 列で説明だけ出す想定
（現状は全トピックで使っている）。
-}
type alias Line =
    { term : String
    , desc : String
    }


type alias Topic =
    { id : TopicId
    , tab : Tab
    , title : String
    , lines : List Line
    }


tabs : List Tab
tabs =
    [ ShortcutsTab, OperationTab, NotationTab, GlossaryTab ]


tabLabel : Tab -> String
tabLabel tab =
    case tab of
        ShortcutsTab ->
            "ショートカット"

        OperationTab ->
            "操作"

        NotationTab ->
            "記法"

        GlossaryTab ->
            "用語・アイコン"


{-| トピックの所属タブを引く。見つからなければ操作タブ扱い（ここに来ることは想定していない。
`tests/HelpTest.elm` の `tabOf` 整合テストで検出する）。
-}
tabOf : TopicId -> Tab
tabOf topicId =
    find topicId |> Maybe.map .tab |> Maybe.withDefault OperationTab


topicsIn : Tab -> List Topic
topicsIn tab =
    List.filter (\t -> t.tab == tab) topics


find : TopicId -> Maybe Topic
find topicId =
    topics |> List.filter (\t -> t.id == topicId) |> List.head


{-| ヘルプ本文中の該当カードに付ける DOM id。今は aria 用の予約のみで、実際のスクロール制御には
使っていない（ジャンプは「先頭にピン留め」で行う。`Browser.Dom` によるアンカージャンプは
タッチの慣性スクロールと相性が悪いため今回は見送った）。
-}
domId : TopicId -> String
domId topicId =
    "help-topic-" ++ topicSlug topicId


topicSlug : TopicId -> String
topicSlug topicId =
    case topicId of
        Transport ->
            "transport"

        EditKeys ->
            "edit-keys"

        SelectKeys ->
            "select-keys"

        VoicingKeys ->
            "voicing-keys"

        KeyboardPlay ->
            "keyboard-play"

        PianoRollOps ->
            "piano-roll-ops"

        VelocityLane ->
            "velocity-lane"

        LoopOps ->
            "loop-ops"

        SectionOps ->
            "section-ops"

        ChordOps ->
            "chord-ops"

        DrumApply ->
            "drum-apply"

        DrumPresets ->
            "drum-presets"

        TrackOps ->
            "track-ops"

        Modifiers ->
            "modifiers"

        TouchOps ->
            "touch-ops"

        FileOps ->
            "file-ops"

        RefAudioOps ->
            "ref-audio-ops"

        ScrapOps ->
            "scrap-ops"

        PaneOps ->
            "pane-ops"

        ChordSheet ->
            "chord-sheet"

        VoicingNotation ->
            "voicing-notation"

        VoicingEditorOps ->
            "voicing-editor-ops"

        Degrees ->
            "degrees"

        Icons ->
            "icons"

        Terms ->
            "terms"


line : String -> String -> Line
line term desc =
    { term = term, desc = desc }


topics : List Topic
topics =
    [ { id = Transport
      , tab = ShortcutsTab
      , title = "再生・移動"
      , lines =
            [ line "Space" "再生 / 停止（現在のループモードに従う。フォーカス位置によらず常にこの動作。ボタンは Enter で押せる）"
            , line "Home / End" "曲の先頭 / 末尾へシーク"
            , line "[ / ]" "ループ範囲の開始 / 終了を再生位置に設定"
            , line "1〜9" "その番目のセクションの先頭へジャンプ"
            , line "?" "このヘルプを開閉"
            , line "Tab" "ボタンやピアノロール領域にフォーカスを移動"
            ]
      }
    , { id = EditKeys
      , tab = ShortcutsTab
      , title = "ノートの編集"
      , lines =
            [ line "Ctrl/Cmd+Z" "元に戻す（+Shift または Ctrl/Cmd+Y でやり直し）"
            , line "n" "再生位置にノートを追加（鍵盤表示中は無効）"
            , line "c" "カットツールの切替（ノートをクリックした位置で分割）"
            , line "g" "隣接する選択ノートをマージ（カットの逆操作）"
            , line "Ctrl/Cmd+C / X / V" "コピー / カット / 再生位置に貼付"
            , line "Delete / Backspace" "選択ノートを削除（同じ動作）"
            , line "↑↓" "選択ノートを半音移動（+Shift でオクターブ）"
            , line "Ctrl/Cmd+←→" "選択ノートを 1/16 移動（+Shift で1小節）"
            ]
      }
    , { id = SelectKeys
      , tab = ShortcutsTab
      , title = "選択"
      , lines =
            [ line "←→" "隣のノートを選択（音付き）"
            , line "Ctrl/Cmd+A" "選択中トラックの全ノートを選択"
            , line "Ctrl/Cmd+Shift+A" "選択中のセクション内のノートを選択"
            , line "Shift+ドラッグ" "矩形選択"
            , line "Escape" "選択解除（セクション/トラック/断片の削除確認待ちも解除）、開いているモーダルを閉じる（コード進行の展開パネルは対象外）"
            ]
      }
    , { id = VoicingKeys
      , tab = ShortcutsTab
      , title = "ボイシング編集中のキー"
      , lines =
            [ line "↑↓" "選択した構成音を半音移動（+Shift でオクターブ）"
            , line "Delete / Backspace" "選択した構成音を削除"
            ]
      }
    , { id = KeyboardPlay
      , tab = ShortcutsTab
      , title = "PC キーで弾く（鍵盤を開いているとき）"
      , lines =
            [ line "Z〜M" "C3〜B3（選択中トラックの音色で鳴る）"
            , line "Q,2,W,3,E,R,5,T,6,Y,7,U" "C4〜B4"
            , line "I" "C5"
            , line "（鍵盤表示中）" "n / c / g / 数字キーのショートカットは無効になります"
            ]
      }
    , { id = PianoRollOps
      , tab = OperationTab
      , title = "ピアノロールの操作"
      , lines =
            [ line "クリック" "ノートを配置（1/16 スナップ）"
            , line "ドラッグ" "ノート本体をつまむと移動、端をつまむと長さを変更"
            , line "ダブルクリック / 右クリック" "ノートを削除"
            , line "✂ カットツール" "ONの間はクリックした位置でノートを分割（c キーでも切替）"
            , line "🔒 ロック" "ONの間は配置・移動・リサイズ・削除・ベロシティ変更をすべて止め、スクロールとタップ選択・長押し矩形選択だけを残す（l キーで切替。Escapeでは解除されません）"
            , line "Shift+ドラッグ" "矩形選択"
            , line "Alt+ドラッグ" "スナップを無効化して自由な位置に置く"
            ]
      }
    , { id = VelocityLane
      , tab = OperationTab
      , title = "ベロシティレーン（Vel）"
      , lines =
            [ line "縦ドラッグ" "ノートの強さ（velocity）を変える"
            ]
      }
    , { id = LoopOps
      , tab = OperationTab
      , title = "ループ"
      , lines =
            [ line "ループ: オフ / 全体 / セクション / 範囲" "Space で再生したときの繰り返し範囲"
            , line "範囲" "この場合だけ、ルーラーの帯を Shift+ドラッグで作り直せて、端をつまんで伸縮もできる（他のモードでは帯を編集できない）"
            , line "[ / ]" "ループ範囲の開始 / 終了を再生位置に設定（「範囲」を選んでいなくても使える）"
            ]
      }
    , { id = SectionOps
      , tab = OperationTab
      , title = "セクション"
      , lines =
            [ line "ブロックのドラッグ" "セクションの並べ替え"
            , line "右端のドラッグ" "小節数の変更"
            , line "小節数の入力欄" "フォーカスを外す（blur）と確定"
            , line "1〜9" "その番目のセクションの先頭へジャンプ"
            ]
      }
    , { id = ChordOps
      , tab = OperationTab
      , title = "コード進行の操作"
      , lines =
            [ line "トークンのドラッグ" "小節の入れ替え（ライン表示・ブロック表示どちらでも）"
            , line "背景のドラッグ" "範囲選択"
            , line "ダブルクリック" "運指・ボイシングの選択画面を開く"
            , line "Enter" "運指選択画面の入力を確定"
            , line "クリック" "そのコードの位置へ再生位置を移動"
            ]
      }
    , { id = DrumApply
      , tab = OperationTab
      , title = "ドラム：適用先と適用方法"
      , lines =
            [ line "適用先" "プリセットを書き込む範囲。セクション＝選択中のセクション／ループ範囲／プレイヘッドから指定小節数／曲全体"
            , line "適用方法・差し替え" "そのパターンが使う楽器（レーン）だけ範囲内を消してから書く。既定。何度押しても結果が変わらない（冪等）"
            , line "適用方法・重ねる" "既存のノートを消さずに足す（同じ位置・同じ音の重複は作らない）"
            , line "適用方法・全消去して差し替え" "楽器を問わず範囲内のノートを全部消してから書く"
            , line "長さ" "適用先が「プレイヘッドから」のときだけ使える、書き込む小節数"
            , line "レーン名クリック" "そのレーンをプリセット適用の対象から外す（除外中は打消し線で表示）"
            ]
      }
    , { id = DrumPresets
      , tab = OperationTab
      , title = "ドラム：プリセットの種類"
      , lines =
            [ line "フルキット" "8ビート・16ビート・4つ打ち・バラード・シャッフル・ロック8・ファンク16・ボサノバ。キック・スネア・ハットなどをまとめて書き込む"
            , line "レーン単体" "「キック: 4つ打ち」「ハット: 8分」「スネア: 2・4」のように名前の先頭に楽器名が付くもの。そのレーンだけ書き込む"
            , line "フィル" "「フィル: タム下り」など、小節後半のフィルインを書き込む"
            ]
      }
    , { id = TrackOps
      , tab = OperationTab
      , title = "トラック一覧"
      , lines =
            [ line "👻" "ピアノロールに他トラックのノートを薄く重ねて表示"
            , line "M" "ミュート"
            , line "音量スライダー" "そのトラックの音量"
            , line "⠠ ドラッグハンドル" "トラックの並べ替え"
            ]
      }
    , { id = Modifiers
      , tab = OperationTab
      , title = "修飾キーと代替モード"
      , lines =
            [ line "Shift" "複数選択・矩形選択"
            , line "Ctrl/Cmd + 空白クリック" "その位置へシーク"
            , line "Alt + ドラッグ" "スナップを無効化"
            , line "タッチの「修飾キー:」行" "物理キーが押せない環境向けの代替。通常/シーク/スナップOFF を選んでからタップやドラッグする（矩形選択は長押しに一本化済み。デスクトップでも常時表示）"
            ]
      }
    , { id = TouchOps
      , tab = OperationTab
      , title = "タッチ操作"
      , lines =
            [ line "長押し（0.5秒）してから動かす" "矩形選択・ループ範囲ドラッグ・複数選択など、Shiftドラッグ相当の操作に切り替わる。ノートの上でも同じ（指を止めてから動かすのがコツ）"
            , line "ドラムのノートありセルの長押し" "例外的にノートを削除（他は矩形選択に使うため）"
            , line "ダブルタップ" "ノートを削除（ダブルクリックと同じ）"
            , line "🔒 ロック中" "配置・編集を止めてスクロールだけにする（l キーで切替）"
            , line "下部の4タブ" "曲構成 / 編集 / トラック / 素材 を切替（幅が狭い、または pointer:coarse な端末で自動的にこのレイアウトになる）"
            ]
      }
    , { id = FileOps
      , tab = OperationTab
      , title = "保存と書き出し"
      , lines =
            [ line "JSON書出 / JSON読込" "プロジェクト全体をファイルに保存・復元する唯一の形式"
            , line "MIDI書出" "曲を .mid で書き出す（書き出し専用。読み込みはできません）"
            , line "WAV書出" "曲を音声ファイルとして書き出す（書き出し専用）。「ループ範囲を書き出す」を使うには先にループ:範囲を設定しておく"
            , line "新規" "現在の内容を破棄して新規作成（もう一度押すと確定。Ctrl/Cmd+Zで戻せる）"
            ]
      }
    , { id = RefAudioOps
      , tab = OperationTab
      , title = "参考オーディオ"
      , lines =
            [ line "読み込み" "耳コピ用の音声ファイル。データはブラウザ内（IndexedDB）に保存され、リロード後も自動で読み込まれる"
            , line "オフセット(ms)" "指定した位置を小節1・拍1に合わせる。フォーカスを外すと確定"
            , line "解除" "読み込みを止める（曲データ自体は消えない）"
            ]
      }
    , { id = ScrapOps
      , tab = OperationTab
      , title = "断片棚"
      , lines =
            [ line "+ 断片" "ピアノロールで選択したノートを断片として保存"
            , line "▶" "断片を試し聞き"
            , line "配置" "選択中のトラックの再生位置に配置"
            ]
      }
    , { id = PaneOps
      , tab = OperationTab
      , title = "画面レイアウト"
      , lines =
            [ line "左右の仕切り" "ドラッグでサイドバーの幅を変える"
            , line "◀ / ▶" "サイドバーの折りたたみ"
            , line "幅1200px未満・タッチ端末" "曲構成/編集/トラック/素材 の4ページ・下部タブバーに自動で切り替わる"
            ]
      }
    , { id = ChordSheet
      , tab = NotationTab
      , title = "コード譜の書き方"
      , lines =
            [ line "|" "小節区切り"
            , line "空行" "セクションの区切り。区切った直後の行がそのセクション名になる"
            , line "%" "直前のコードを繰り返す"
            , line "_" "休符"
            , line "=" "直前のコードを伸ばす"
            , line "// 以降" "行末までコメント"
            , line "リズム: 8ビート" "セクション先頭にこう書くと、そのセクションのコードを指定したリズムで刻む（8ビート・16ビート・フォーク・アルペジオ・全音符・シンコペーション）"
            ]
      }
    , { id = VoicingNotation
      , tab = NotationTab
      , title = "@NAME とボイシング辞書"
      , lines =
            [ line "ボイシング辞書" "@NAME でコードに指定できる、名前付きの運指の一覧"
            , line "@NAME" "コード名の後ろに付けて、辞書に登録した運指を使う（例: FM7@drop2）"
            , line "🎵 ボイシング" "ON=@NAME の運指を使う、OFF=固定ルールで鳴らす"
            , line "🎸/🎹 トグル" "登録ボイシングのないコードを今どちらで鳴らす/表示しているか（押すと切り替わる。切替先ではなく現在の状態を示すラベル）"
            ]
      }
    , { id = VoicingEditorOps
      , tab = NotationTab
      , title = "ボイシングの編集画面"
      , lines =
            [ line "鍵盤クリック" "構成音を置く"
            , line "鍵盤ダブルクリック" "構成音を消す"
            , line "指板セルのクリック / ダブルクリック" "押弦位置を置く / 消す"
            , line "試聴キー" "▶ で試聴するときのルート音（保存される offsets はキーに依らない相対値）"
            , line "Closed / Drop2 / Wide" "和音の積み方。テンション（#9 等）を含まない基本形になる"
            , line "初期化" "offsets と弦選択を空にして最初からやり直す（Ctrl/Cmd+Zで戻せる）"
            , line "▶ 和音で試聴" "このボイシングを和音として鳴らす"
            ]
      }
    , { id = Degrees
      , tab = NotationTab
      , title = "度数表示とキー・旋法"
      , lines =
            [ line "度数表示" "再生中のコードの下に、そのセクションのキー（教会旋法も含む）に対する度数（I, V7, vi など）を表示"
            , line "セクションのキー" "セクションごとに設定でき、転調や拍子変更はセクションを割って表現する"
            ]
      }
    , { id = Icons
      , tab = GlossaryTab
      , title = "アイコン凡例"
      , lines =
            [ line "🎼 / 🎹 / 🎚 / 📦" "タッチレイアウトのタブ（曲構成 / 編集 / トラック / 素材）"
            , line "🎹" "文脈によって「ピアノロール表示」「ピアノ表示」「鍵盤を開く」「編集タブ」の4つの意味がある"
            , line "✂" "ピアノロールではカットツール、ツールバーでは小節削除の意味"
            , line "👻" "ゴースト表示（他トラックのノートを薄く重ねる）"
            , line "M" "ミュート"
            , line "Vel" "ベロシティ（音の強さ）"
            , line "⠠" "ドラッグして並べ替えるハンドル"
            , line "📝" "セクションにメモがあることを示す印"
            , line "🔁" "ループ"
            , line "📌" "再生に追従してスクロール"
            , line "✦" "コード進行を編集"
            ]
      }
    , { id = Terms
      , tab = GlossaryTab
      , title = "用語"
      , lines =
            [ line "ボイシング辞書" "@NAME で指定できる、名前付きの運指の一覧"
            , line "断片" "曲に置く前に退避しておけるフレーズ"
            , line "差し替え / 全消去して差し替え" "ドラムパターン適用時の違い。差し替え＝そのパターンが使う楽器の行だけ消す（既定）、全消去して差し替え＝範囲内を楽器を問わず全部消す"
            , line "スナップ" "ノートやドラッグの位置を 1/16 などの格子に吸着させる機能"
            , line "blur確定" "入力欄からフォーカスを外すと値が確定する仕組み（BPM・小節数・オフセットなど）"
            ]
      }
    ]
