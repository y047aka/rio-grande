module ConfigAndPreview exposing (configAndPreview)

import Config exposing (Config(..), configToField)
import Css exposing (..)
import Data.Theme exposing (Theme(..))
import Html.Styled as Html exposing (Html, aside, div, label, p, text)
import Html.Styled.Attributes exposing (css)
import UI.Header as Header


type alias ConfigSection option msg =
    { label : String
    , configs : List { label : String, config : Config option msg, note : String }
    }


configAndPreview :
    { title : String
    , preview : List (Html msg)
    , configSets : List (ConfigSection option msg)
    }
    -> Html msg
configAndPreview { title, preview, configSets } =
    let
        title_ =
            if title == "" then
                text ""

            else
                Header.header { theme = Light } [] [ text title ]
    in
    Html.styled Html.div
        [ padding2 (em 2) zero
        , position relative
        , property "-webkit-tap-highlight-color" "transparent"
        , whiteSpace preWrap
        ]
        []
        [ title_
        , div
            [ css
                [ property "display" "grid"
                , property "grid-template-columns" "1fr 300px"
                , property "gap" "50px"
                ]
            ]
            [ div [ css [ width (pct 100) ] ] preview
            , configPanel configSets
            ]
        ]


configPanel : List (ConfigSection option msg) -> Html msg
configPanel configSets =
    aside
        [ css
            [ paddingLeft (px 15)
            , borderLeft3 (px 1) solid (hex "#DDD")
            ]
        ]
        (List.map
            (\configSet ->
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
                        ]
                    ]
                    (div
                        [ css
                            [ fontWeight bold
                            , empty [ display none ]
                            ]
                        ]
                        [ text configSet.label ]
                        :: List.map
                            (\field ->
                                div [ css [ displayFlex, flexDirection column, property "gap" "5px" ] ]
                                    [ label [] [ text field.label ]
                                    , configToField field.config
                                    , p [ css [ color (hex "#999") ] ] [ text field.note ]
                                    ]
                            )
                            configSet.configs
                    )
            )
            configSets
        )
