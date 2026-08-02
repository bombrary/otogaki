module Data.Chord.Parser exposing (parse)

import Data.Chord exposing (Alteration(..), Chord, Extension(..), Quality(..))


parse : String -> Result String Chord
parse raw =
    let
        s =
            String.trim raw
    in
    if s == "" then
        Err "空のコード"

    else
        case String.split "/" s of
            [ mainPart ] ->
                parseMain mainPart Nothing

            [ mainPart, bassStr ] ->
                case parsePitch bassStr of
                    Just bass ->
                        parseMain mainPart (Just bass)

                    Nothing ->
                        Err ("ベース音が読めない: " ++ bassStr)

            _ ->
                Err "スラッシュが多すぎる"


parseMain : String -> Maybe Int -> Result String Chord
parseMain s bass =
    case rootAndRest s of
        Just ( root, rest ) ->
            parseQuality rest
                |> Result.andThen
                    (\( qEntry, leftover ) ->
                        parseSuffixes leftover ( qEntry.exts, qEntry.alts )
                            |> Result.map
                                (\( allExts, allAlts ) ->
                                    { root = root
                                    , quality = qEntry.quality
                                    , extensions = allExts
                                    , alterations = allAlts
                                    , bass = bass
                                    }
                                )
                    )

        Nothing ->
            Err ("ルート音が読めない: " ++ s)


letterSemitone : Char -> Maybe Int
letterSemitone c =
    case c of
        'C' ->
            Just 0

        'D' ->
            Just 2

        'E' ->
            Just 4

        'F' ->
            Just 5

        'G' ->
            Just 7

        'A' ->
            Just 9

        'B' ->
            Just 11

        _ ->
            Nothing


rootAndRest : String -> Maybe ( Int, String )
rootAndRest s =
    String.uncons s
        |> Maybe.andThen
            (\( c, rest ) ->
                letterSemitone (Char.toUpper c)
                    |> Maybe.map
                        (\base ->
                            case String.uncons rest of
                                Just ( '#', r2 ) ->
                                    ( modBy 12 (base + 1), r2 )

                                Just ( 'b', r2 ) ->
                                    ( modBy 12 (base - 1), r2 )

                                _ ->
                                    ( base, rest )
                        )
            )


parsePitch : String -> Maybe Int
parsePitch s =
    case rootAndRest (String.trim s) of
        Just ( pitch, "" ) ->
            Just pitch

        _ ->
            Nothing


type alias QualityEntry =
    { quality : Quality
    , exts : List Extension
    , alts : List Alteration
    }


entry : Quality -> List Extension -> List Alteration -> QualityEntry
entry q exts alts =
    { quality = q, exts = exts, alts = alts }


qualityTable : List ( String, QualityEntry )
qualityTable =
    [ ( "mmaj7", entry MinMaj7 [] [] )
    , ( "mM7", entry MinMaj7 [] [] )
    , ( "maj13", entry Maj7 [ Thirteen ] [] )
    , ( "maj9", entry Maj7 [ Nine ] [] )
    , ( "maj7", entry Maj7 [] [] )
    , ( "M13", entry Maj7 [ Thirteen ] [] )
    , ( "M9", entry Maj7 [ Nine ] [] )
    , ( "M7", entry Maj7 [] [] )
    , ( "△13", entry Maj7 [ Thirteen ] [] )
    , ( "△9", entry Maj7 [ Nine ] [] )
    , ( "△7", entry Maj7 [] [] )
    , ( "△", entry Maj7 [] [] )
    , ( "m7b5", entry HalfDim7 [] [] )
    , ( "m7-5", entry HalfDim7 [] [] )
    , ( "ø", entry HalfDim7 [] [] )
    , ( "dim7", entry Dim7 [] [] )
    , ( "dim", entry Dim [] [] )
    , ( "°7", entry Dim7 [] [] )
    , ( "°", entry Dim [] [] )
    , ( "aug7", entry Dom7 [] [ Sharp5 ] )
    , ( "augM7", entry Maj7 [] [ Sharp5 ] )
    , ( "aug", entry Aug [] [] )
    , ( "+7", entry Dom7 [] [ Sharp5 ] )
    , ( "+", entry Aug [] [] )
    , ( "min7", entry Min7 [] [] )
    , ( "min", entry Min [] [] )
    , ( "m13", entry Min7 [ Thirteen ] [] )
    , ( "m11", entry Min7 [ Eleven ] [] )
    , ( "m9", entry Min7 [ Nine ] [] )
    , ( "m7", entry Min7 [] [] )
    , ( "m6", entry MinSixth [] [] )
    , ( "m", entry Min [] [] )
    , ( "13", entry Dom7 [ Thirteen ] [] )
    , ( "11", entry Dom7 [ Eleven ] [] )
    , ( "9", entry Dom7 [ Nine ] [] )
    , ( "7", entry Dom7 [] [] )
    , ( "sus4", entry Sus4 [] [] )
    , ( "sus2", entry Sus2 [] [] )
    , ( "69", entry Sixth [ Nine ] [] )
    , ( "6", entry Sixth [] [] )
    , ( "5", entry Power [] [] )
    , ( "add9", entry Maj [ Add9 ] [] )
    ]


parseQuality : String -> Result String ( QualityEntry, String )
parseQuality rest =
    if rest == "" then
        Ok ( entry Maj [] [], "" )

    else
        let
            sorted =
                List.sortBy (\( t, _ ) -> negate (String.length t)) qualityTable

            match =
                sorted
                    |> List.filter (\( t, _ ) -> String.startsWith t rest)
                    |> List.head
        in
        case match of
            Just ( t, e ) ->
                Ok ( e, String.dropLeft (String.length t) rest )

            Nothing ->
                Ok ( entry Maj [] [], rest )


type Suffix
    = SExt Extension
    | SAlt Alteration


suffixTable : List ( String, Suffix )
suffixTable =
    [ ( "(add9)", SExt Add9 )
    , ( "add9", SExt Add9 )
    , ( "(b9)", SExt FlatNine )
    , ( "b9", SExt FlatNine )
    , ( "(#9)", SExt SharpNine )
    , ( "#9", SExt SharpNine )
    , ( "(#11)", SExt SharpEleven )
    , ( "#11", SExt SharpEleven )
    , ( "(b13)", SExt FlatThirteen )
    , ( "b13", SExt FlatThirteen )
    , ( "(9)", SExt Nine )
    , ( "(11)", SExt Eleven )
    , ( "(13)", SExt Thirteen )
    , ( "(b5)", SAlt Flat5 )
    , ( "b5", SAlt Flat5 )
    , ( "-5", SAlt Flat5 )
    , ( "(#5)", SAlt Sharp5 )
    , ( "#5", SAlt Sharp5 )
    , ( "+5", SAlt Sharp5 )
    ]


parseSuffixes : String -> ( List Extension, List Alteration ) -> Result String ( List Extension, List Alteration )
parseSuffixes leftover ( exts, alts ) =
    if leftover == "" then
        Ok ( exts, alts )

    else
        let
            sorted =
                List.sortBy (\( t, _ ) -> negate (String.length t)) suffixTable

            match =
                sorted
                    |> List.filter (\( t, _ ) -> String.startsWith t leftover)
                    |> List.head
        in
        case match of
            Just ( t, suffix ) ->
                let
                    remaining =
                        String.dropLeft (String.length t) leftover
                in
                case suffix of
                    SExt ext ->
                        parseSuffixes remaining ( exts ++ [ ext ], alts )

                    SAlt alt ->
                        parseSuffixes remaining ( exts, alts ++ [ alt ] )

            Nothing ->
                Err ("解釈できない表記: " ++ leftover)
