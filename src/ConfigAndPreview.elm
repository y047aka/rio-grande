module ConfigAndPreview exposing (configAndPreview)

import Config
import Css exposing (..)
import Css.Palette as Palette exposing (darkPalette, palette, setBackground, setColor)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, aside, div, text)
import Html.Styled.Attributes exposing (css)
import UI.Header as Header


type alias ConfigSection model =
    { label : String
    , configs : List (Html (Config.Msg model))
    }


configAndPreview :
    { title : String
    , toMsg : Config.Msg model -> msg
    , theme : Theme
    , inverted : Bool
    , preview : List (Html msg)
    , configSections : List (ConfigSection model)
    }
    -> Html msg
configAndPreview { title, toMsg, theme, inverted, preview, configSections } =
    let
        title_ =
            if title == "" then
                text ""

            else
                Header.header { theme = theme } [] [ text title ]
    in
    div []
        [ title_
        , div
            [ css
                [ property "display" "grid"
                , property "grid-template-columns" "1fr 300px"
                , border3 (px 1) solid (hex "#DDD")
                , borderRadius (px 15)
                , overflow hidden
                ]
            ]
            [ previewPanel { inverted = inverted } preview
            , configPanel toMsg configSections
            ]
        ]


previewPanel : { inverted : Bool } -> List (Html msg) -> Html msg
previewPanel { inverted } previewSections =
    let
        theme =
            if inverted then
                Dark

            else
                Light
    in
    div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent center
            , padding (em 2)
            , palette (Palette.init |> setBackground (hex "#FFFFFF"))
            , darkPalette theme
                (Palette.init
                    |> setBackground (hex "#1B1C1D")
                    |> setColor (rgba 255 255 255 0.9)
                )
            ]
        ]
        previewSections


configPanel : (Config.Msg model -> msg) -> List (ConfigSection model) -> Html msg
configPanel toMsg configSections =
    Html.map toMsg <|
        aside
            [ css
                [ padding (em 1)
                , borderLeft3 (px 1) solid (hex "#DDD")
                ]
            ]
            (List.map
                (\configSection ->
                    div
                        [ css
                            [ displayFlex
                            , flexDirection column
                            , property "gap" "15px"
                            , paddingBottom (px 15)
                            , nthChild "n+2"
                                [ paddingTop (px 15)
                                , borderTop3 (px 1) solid (hex "#DDD")
                                ]
                            , lastChild [ paddingBottom zero ]
                            ]
                        ]
                        (div
                            [ css
                                [ fontWeight bold
                                , empty [ display none ]
                                ]
                            ]
                            [ text configSection.label ]
                            :: configSection.configs
                        )
                )
                configSections
            )
