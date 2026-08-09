module Data.Chord.Detect exposing (detect)

{-| 選択されたノートのピッチ集合からコード名を推定する。
ピッチクラス集合の完全一致で判定し、最低音をベースとして優先する。
-}

import Data.Chord.Format as Format
import Set exposing (Set)


templates : List ( String, List Int )
templates =
    [ ( "", [ 0, 4, 7 ] )
    , ( "m", [ 0, 3, 7 ] )
    , ( "dim", [ 0, 3, 6 ] )
    , ( "aug", [ 0, 4, 8 ] )
    , ( "sus2", [ 0, 2, 7 ] )
    , ( "sus4", [ 0, 5, 7 ] )
    , ( "5", [ 0, 7 ] )
    , ( "7", [ 0, 4, 7, 10 ] )
    , ( "maj7", [ 0, 4, 7, 11 ] )
    , ( "m7", [ 0, 3, 7, 10 ] )
    , ( "mM7", [ 0, 3, 7, 11 ] )
    , ( "dim7", [ 0, 3, 6, 9 ] )
    , ( "m7b5", [ 0, 3, 6, 10 ] )
    , ( "6", [ 0, 4, 7, 9 ] )
    , ( "m6", [ 0, 3, 7, 9 ] )
    , ( "7b5", [ 0, 4, 6, 10 ] )
    , ( "aug7", [ 0, 4, 8, 10 ] )
    , ( "augM7", [ 0, 4, 8, 11 ] )
    , ( "add9", [ 0, 2, 4, 7 ] )
    , ( "madd9", [ 0, 2, 3, 7 ] )
    , ( "69", [ 0, 2, 4, 7, 9 ] )
    , ( "9", [ 0, 2, 4, 7, 10 ] )
    , ( "maj9", [ 0, 2, 4, 7, 11 ] )
    , ( "m9", [ 0, 2, 3, 7, 10 ] )
    , ( "7b9", [ 0, 1, 4, 7, 10 ] )
    , ( "7#9", [ 0, 3, 4, 7, 10 ] )
    , ( "11", [ 0, 2, 4, 5, 7, 10 ] )
    , ( "13", [ 0, 2, 4, 7, 9, 10 ] )
    ]


matchesFor : Set Int -> Int -> List String
matchesFor pcs root =
    let
        rel =
            Set.map (\p -> modBy 12 (p - root)) pcs
    in
    templates
        |> List.filter (\( _, tmpl ) -> Set.fromList tmpl == rel)
        |> List.map Tuple.first


detect : List Int -> Maybe String
detect pitches =
    case List.minimum pitches of
        Nothing ->
            Nothing

        Just lowest ->
            let
                bassPc =
                    modBy 12 lowest

                pcs =
                    Set.fromList (List.map (modBy 12) pitches)

                nameFor root suffix =
                    Format.pitchName False root
                        ++ suffix
                        ++ (if root /= bassPc then
                                "/" ++ Format.pitchName False bassPc

                            else
                                ""
                           )

                candidates =
                    Set.toList pcs
                        |> List.concatMap
                            (\root ->
                                matchesFor pcs root
                                    |> List.map (\suffix -> ( root, suffix ))
                            )

                preferred =
                    candidates
                        |> List.filter (\( root, _ ) -> root == bassPc)
                        |> List.head
            in
            case preferred of
                Just ( root, suffix ) ->
                    Just (nameFor root suffix)

                Nothing ->
                    candidates
                        |> List.head
                        |> Maybe.map (\( root, suffix ) -> nameFor root suffix)
