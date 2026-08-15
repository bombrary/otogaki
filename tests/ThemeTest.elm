module ThemeTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import View.Theme as Theme


suite : Test
suite =
    describe "View.Theme"
        [ describe "withAlpha"
            [ test "primary を 50% で color-mix する" <|
                \_ ->
                    Expect.equal "color-mix(in srgb, var(--md-primary) 50%, transparent)" (Theme.withAlpha 0.5 Theme.primary)
            , test "primary を 15% で color-mix する" <|
                \_ ->
                    Expect.equal "color-mix(in srgb, var(--md-primary) 15%, transparent)" (Theme.withAlpha 0.15 Theme.primary)
            , test "scrim を 32% で color-mix する" <|
                \_ ->
                    Expect.equal "color-mix(in srgb, var(--md-scrim) 32%, transparent)" (Theme.withAlpha 0.32 Theme.scrim)
            , test "alpha 1 は 100% になる" <|
                \_ ->
                    Expect.equal "color-mix(in srgb, var(--md-error) 100%, transparent)" (Theme.withAlpha 1 Theme.error)
            ]
        , describe "themeToString / themeFromString"
            [ test "SystemTheme は round-trip する" <|
                \_ ->
                    Expect.equal (Just Theme.SystemTheme) (Theme.themeFromString (Theme.themeToString Theme.SystemTheme))
            , test "LightTheme は round-trip する" <|
                \_ ->
                    Expect.equal (Just Theme.LightTheme) (Theme.themeFromString (Theme.themeToString Theme.LightTheme))
            , test "DarkTheme は round-trip する" <|
                \_ ->
                    Expect.equal (Just Theme.DarkTheme) (Theme.themeFromString (Theme.themeToString Theme.DarkTheme))
            , test "不明な文字列は Nothing を返す" <|
                \_ ->
                    Expect.equal Nothing (Theme.themeFromString "sepia")
            ]
        ]
