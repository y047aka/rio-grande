module Skeleton exposing (skeleton)

import Css exposing (..)
import Css.FontAwesome exposing (fontAwesome)
import Css.Global exposing (global)
import Css.Media as Media exposing (only, screen, withMedia)
import Css.Palette as Palette exposing (darkPalette, palette, setBackground, setColor)
import Css.Reset exposing (normalize)
import Css.ResetAndCustomize exposing (additionalReset, globalCustomize)
import Data.Theme as Theme exposing (Theme(..))
import Html.Styled as Html exposing (Attribute, Html, div, header, option, select, text)
import Html.Styled.Attributes exposing (css, selected, value)
import Html.Styled.Events exposing (onInput)
import UI.Breadcrumb exposing (BreadcrumbItem, Divider(..), breadcrumbWithProps)


skeleton : { model | theme : Theme } -> { changeThemeMsg : Theme -> msg } -> List (Html msg) -> Html msg
skeleton model { changeThemeMsg } body =
    div []
        [ global (normalize ++ additionalReset ++ globalCustomize ++ fontAwesome)
        , siteHeader model { changeThemeMsg = changeThemeMsg } { title = "title" }
        , main_ { theme = Light } [] body
        ]


siteHeader : { model | theme : Theme } -> { changeThemeMsg : Theme -> msg } -> { title : String } -> Html msg
siteHeader model { changeThemeMsg } page =
    header
        [ css
            [ position sticky
            , top zero
            , zIndex (int 1)
            , displayFlex
            , justifyContent spaceBetween
            , padding (px 20)
            , backgroundColor (hex "#FFF")
            , borderBottom3 (px 1) solid (hex "#EEE")
            ]
        ]
        [ breadcrumbWithProps { divider = Slash, size = Nothing, theme = model.theme }
            (breadcrumbItems page)
        , div []
            [ select [ onInput (Theme.fromString >> Maybe.withDefault model.theme >> (\theme -> changeThemeMsg theme)) ] <|
                List.map (\theme -> option [ value (Theme.toString theme), selected (model.theme == theme) ] [ text (Theme.toString theme) ])
                    [ System, Light, Dark ]
            ]
        ]


breadcrumbItems : { title : String } -> List BreadcrumbItem
breadcrumbItems { title } =
    case "/" of
        "/" ->
            [ { label = "Top", url = "/" } ]

        _ ->
            [ { label = "Top", url = "/" }
            , { label = title, url = "" }
            ]


main_ : { theme : Theme } -> List (Attribute msg) -> List (Html msg) -> Html msg
main_ { theme } attributes children =
    Html.styled Html.main_
        [ margin zero
        , padding zero
        , palette Palette.init
        , darkPalette theme
            (Palette.init
                |> setBackground (hex "#1B1C1D")
                |> setColor (rgba 255 255 255 0.9)
            )
        ]
        attributes
        [ container [] children ]


container : List (Attribute msg) -> List (Html msg) -> Html msg
container =
    Html.styled Html.div
        [ maxWidth (pct 100)

        -- Mobile
        , withMedia [ only screen [ Media.maxWidth (px 559.98) ] ]
            [ width auto
            , marginLeft (em 1)
            , marginRight (em 1)
            ]

        -- Tablet
        , withMedia [ only screen [ Media.minWidth (px 560), Media.maxWidth (px 1199.98) ] ]
            [ width auto
            , marginLeft (em 3)
            , marginRight (em 3)
            ]

        -- Large Monitor
        , withMedia [ only screen [ Media.minWidth (px 1200) ] ]
            [ width (px 1120)
            , marginLeft auto
            , marginRight auto
            ]
        ]
