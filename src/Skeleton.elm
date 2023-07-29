module Skeleton exposing (skeleton)

import Css exposing (..)
import Css.FontAwesome exposing (fontAwesome)
import Css.Global exposing (global)
import Css.Palette as Palette exposing (darkPalette, palette, paletteWith, setBackground, setColor)
import Css.Reset exposing (normalize)
import Css.ResetAndCustomize exposing (additionalReset, globalCustomize)
import Data.Theme as Theme exposing (Theme(..))
import Html.Styled as Html exposing (Attribute, Html, div, header, option, select, text)
import Html.Styled.Attributes exposing (css, selected, value)
import Html.Styled.Events exposing (onInput)
import UI.Breadcrumb exposing (BreadcrumbItem, Divider(..), breadcrumbWithProps)
import UI.Layout.Box as Box exposing (box)
import UI.Layout.Center as Center
import UI.Layout.Sidebar exposing (withSidebar)
import UI.Layout.Stack as Stack exposing (stack)
import Url exposing (Url)


skeleton : { url : Url, theme : Theme, changeThemeMsg : Theme -> msg } -> { title : String, body : List (Html msg) } -> Html msg
skeleton props { title, body } =
    div []
        [ global (normalize ++ additionalReset ++ globalCustomize ++ fontAwesome)
        , global
            [ Css.Global.body
                [ palette Palette.init
                , darkPalette props.theme
                    (Palette.init
                        |> setBackground (hex "#1B1C1D")
                        |> setColor (rgba 255 255 255 0.9)
                    )
                ]
            ]
        , siteHeader { theme = props.theme, changeThemeMsg = props.changeThemeMsg } { title = title, url = props.url }
        , main_ { theme = props.theme } [] body
        ]


siteHeader : { theme : Theme, changeThemeMsg : Theme -> msg } -> { title : String, url : Url } -> Html msg
siteHeader props page =
    header
        [ css
            [ position sticky
            , top zero
            , zIndex (int 1)
            , padding (px 20)
            , paletteWith { border = borderBottom3 (px 1) solid }
                (Palette.init
                    |> setBackground (hex "#FFF")
                    |> Palette.setBorder (hex "#DDD")
                )
            , darkPalette props.theme
                (Palette.init
                    |> setBackground (hex "#1B1C1D")
                    |> setColor (rgba 255 255 255 0.9)
                )
            ]
        ]
        [ withSidebar
            { side = "right"
            , sideWith = 5
            , contentMin = 25
            , space = 1
            , noStretch = False
            }
            []
            [ breadcrumbWithProps { divider = Slash, size = Nothing, theme = props.theme }
                (breadcrumbItems page)
            , div []
                [ select
                    [ css [ width (pct 100) ]
                    , onInput (Theme.fromString >> Maybe.withDefault props.theme >> (\theme -> props.changeThemeMsg theme))
                    ]
                    (List.map (\theme -> option [ value (Theme.toString theme), selected (props.theme == theme) ] [ text (Theme.toString theme) ])
                        [ System, Light, Dark ]
                    )
                ]
            ]
        ]


breadcrumbItems : { title : String, url : Url } -> List BreadcrumbItem
breadcrumbItems { title, url } =
    case url.path of
        "/" ->
            [ { label = "Top", url = "/" } ]

        _ ->
            [ { label = "Top", url = "/" }
            , { label = title, url = Url.toString url }
            ]


main_ : { theme : Theme } -> List (Attribute msg) -> List (Html msg) -> Html msg
main_ { theme } attributes children =
    Html.styled Html.main_
        [ margin zero
        , palette Palette.init
        , darkPalette theme
            (Palette.init
                |> setBackground (hex "#1B1C1D")
                |> setColor (rgba 255 255 255 0.9)
            )
        ]
        attributes
        [ box
            (Box.defaultProps
                |> Box.setBorderWidth 0
                |> Box.setPalette Palette.init
            )
            []
            [ Center.center (Center.defaultProps |> Center.setMax 170)
                []
                [ stack (Stack.defaultProps |> Stack.setGap 1.5) [] children ]
            ]
        ]
